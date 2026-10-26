;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; A LOAD PROBE for a lab, read-only: several visitors at once, each
;; walking the public archive the way a browser does -- the listing, an
;; archived session's record, its replay (the model compiled again on the
;; server, the costly part), and the drawing downloaded as a PNG (the
;; model built and rasterized).  No prompt is sent, no credit drawn, no
;; human check met and no session opened, so the per-address caps do not
;; bind and the agent's key is never needed: what is measured is the
;; server's own work under concurrent visitors, which is what a crowd
;; brings a small host first.
;;
;; Run it from a Gendl room against a lab's address:
;;
;;   (prompt-lab::load-probe "http://127.0.0.1:9080/prompt-lab" :visitors 4 :rounds 3)
;;
;; Each visitor runs ROUNDS rounds; a round takes the next archived id
;; (the listing's, newest first, at most IDS of them) and makes the four
;; requests.  The answer is a report: per request kind the count, the
;; failures (a status other than 200, or no answer within SECONDS) and
;; the latency's median, 90th percentile and worst, plus the wall time
;; and the requests a second.  Keep VISITORS modest on a small host (the
;; public lab's host has two cores and three gigabytes): start at 2,
;; then 4, then 6, and watch the host between runs.  The probe is the
;; aserve client in process, as the lab's own calls are.
;;

(defun load-probe-request (url &key (method :get) body (seconds 60) binary?)
  "One request; values: the status (0 for no answer), the body's length,
the seconds it took, and the body's text.  BINARY? reads the body as
octets (a PNG is not text, and the client would try to decode it)."
  (let ((start (get-internal-real-time))
        ;; https wants the server's name on the handshake (SNI), as the
        ;; lab's own post-json sends it
        (ssl-args (and (string= "https://" url :end2 (min 8 (length url)))
                       (let* ((from (length "https://"))
                              (end (or (position #\/ url :start from) (length url))))
                         (list :server-name (subseq url from end))))))
    (handler-case
        (multiple-value-bind (text status)
            (bt:with-timeout (seconds)
              (net.aserve.client:do-http-request url :method method
                                                     :content body
                                                     :content-type (and body "application/json")
                                                     :headers '(("Accept" . "*/*"))
                                                     :ssl-args ssl-args
                                                     :format (if binary? :binary :text)))
          (values (or status 0) (length (or text ""))
                  (/ (- (get-internal-real-time) start) (float internal-time-units-per-second))
                  (if (stringp text) text "")))
      (error () (values 0 0 (/ (- (get-internal-real-time) start) (float internal-time-units-per-second)) "")))))

(defun load-probe-ids (base ids)
  "The newest IDS archived session ids of BASE's lab that left a model."
  (multiple-value-bind (status length seconds text) (load-probe-request (format nil "~a/api/archive" base))
    (declare (ignore length seconds))
    (unless (eql status 200) (error "The archive door of ~a answered ~a." base status))
  (let* ((json (yason:parse text))
         (sessions (and (hash-table-p json) (gethash "sessions" json))))
    (loop for s across (coerce sessions 'vector)
          when (and (hash-table-p s) (eq (gethash "model" s) t) (stringp (gethash "id" s)))
            collect (gethash "id" s) into found
          when (>= (length found) ids) do (return found)
          finally (return found)))))

(defun load-probe-visitor (base ids rounds seconds record)
  "One visitor: ROUNDS rounds over IDS, each the four requests, timed into
RECORD (a function of kind, status and seconds)."
  (dotimes (round rounds)
    (let ((id (nth (mod round (length ids)) ids)))
      (flet ((ask (kind url &key (method :get) body binary?)
               (multiple-value-bind (status length took) (load-probe-request url :method method :body body :seconds seconds :binary? binary?)
                 (declare (ignore length))
                 (funcall record kind status took))))
        (ask "archive listing" (format nil "~a/api/archive" base))
        (ask "archived record" (format nil "~a/api/archived?id=~a" base id))
        (ask "replay (compile)" (format nil "~a/api/replay" base) :method :post
             :body (format nil "{\"id\":~s}" id))
        (ask "png download (build + raster)" (format nil "~a/api/download?replay=~a&format=png" base id) :binary? t)))))

(defun load-probe-percentile (sorted fraction)
  (if (null sorted) 0
      (nth (min (1- (length sorted)) (floor (* fraction (length sorted)))) sorted)))

(defun load-probe (base &key (visitors 2) (rounds 2) (ids 6) (seconds 90))
  "Run the probe against BASE (the lab's prefix as a URL).  Answers the
report as a string, and prints it."
  (let* ((ids (load-probe-ids base ids))
         (lock (bt:make-lock "load probe"))
         (samples (make-hash-table :test #'equal))
         (start (get-internal-real-time))
         (record (lambda (kind status took)
                   (bt:with-lock-held (lock) (push (cons status took) (gethash kind samples))))))
    (when (null ids) (error "No archived session with a model at ~a." base))
    (let ((threads (loop for v from 1 to visitors
                         collect (let ((offset v))
                                   (bt:make-thread
                                    (lambda ()
                                      ;; visitors start on different ids so the first
                                      ;; round is not one id compiled by everyone
                                      (load-probe-visitor base (append (nthcdr (mod offset (length ids)) ids)
                                                                       (subseq ids 0 (mod offset (length ids))))
                                                          rounds seconds record))
                                    :name (format nil "load probe visitor ~a" v))))))
      (dolist (thread threads) (bt:join-thread thread)))
    (let* ((wall (/ (- (get-internal-real-time) start) (float internal-time-units-per-second)))
           (total 0)
           (report (with-output-to-string (out)
                     (format out "load probe of ~a: ~d visitors x ~d rounds over ~d ids, ~,1f s wall~%"
                             base visitors rounds (length ids) wall)
                     (dolist (kind '("archive listing" "archived record" "replay (compile)" "png download (build + raster)"))
                       (let* ((rows (gethash kind samples))
                              (times (sort (mapcar #'cdr rows) #'<))
                              (failed (count-if-not (lambda (row) (eql (car row) 200)) rows)))
                         (incf total (length rows))
                         (format out "  ~34a n ~3d  failed ~2d  median ~6,2fs  p90 ~6,2fs  worst ~6,2fs~@[  (statuses ~{~a~^, ~})~]~%"
                                 kind (length rows) failed
                                 (load-probe-percentile times 0.5) (load-probe-percentile times 0.9)
                                 (if times (car (last times)) 0)
                                 (and (plusp failed)
                                      (let ((seen nil))
                                        (dolist (row rows)
                                          (unless (eql (car row) 200)
                                            (let ((entry (assoc (car row) seen)))
                                              (if entry (incf (cdr entry)) (push (cons (car row) 1) seen)))))
                                        (mapcar (lambda (e) (format nil "~a x~d" (car e) (cdr e))) (reverse seen)))))))
                     (format out "  ~d requests, ~,2f a second~%" total (if (plusp wall) (/ total wall) 0)))))
      (write-string report)
      report)))
