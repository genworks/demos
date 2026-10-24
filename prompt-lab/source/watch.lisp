(in-package :prompt-lab)

;;
;; MONITORING THE PUBLIC LAB (2026-10-23).  The free public lab runs
;; strangers' code in one shared image -- remote code execution is the
;; product -- so the house watches it, the same way for every visitor:
;;
;;  - a TAMPER-EVIDENT audit log: one line per event, each carrying the
;;    hash of the line before it, so a later edit or a deletion breaks
;;    the chain; a daily ANCHOR of the head hash goes to the operator
;;    out of band (the gate's alert door), so even a whole file rewritten
;;    cannot match the anchor already sent;
;;  - FLAGS on what a sandbox should never see: model source that reaches
;;    for the filesystem, a process, the network or the lab's internals,
;;    and prompts that try to pull out the agent's rules or a secret;
;;  - a HONEYTOKEN: a plausible fake secret left where a prowler would
;;    find it; its string in any source or prompt is proof it was read;
;;  - ALERTS relayed to the operator through the gate (<messages-url>/alert),
;;    so this image, which a visitor's code can read, holds no notify URL.
;;
;; Nothing here BLOCKS a build (watch, do not gate); it records and
;; alerts.  Every entry is wrapped so a fault never reaches the request.
;; The log holds visitors' prompts and addresses: it stays on the ship,
;; like the archive, never in a repo.
;;

(defparameter *watch-root* nil
  "Directory for the audit log and the honeytoken; nil = no monitoring.
A free public lab's stack init sets it (beside the archive).")

(defparameter *watch-alerts?* t
  "Whether a high flag and the daily anchor are relayed to the operator
through the gate's alert door.")

(defparameter *watch-honeytoken-file* nil
  "A path the monitor plants a fake secret at (a place a prowler tries);
its string later in any source or prompt is exfiltration.  Nil: none.")

(defvar *watch-lock* (bt:make-lock "prompt-lab watch"))
(defvar *watch-chain* nil "Hash of the last record written: the chain head.")
(defvar *watch-honeytoken* nil "The decoy secret string, minted once.")
(defvar *watch-anchor-thread* nil)

(defun watch-on? () (and (stringp *watch-root*) t))

(defun watch-directory ()
  (let ((s (namestring *watch-root*)))
    (pathname (if (and (plusp (length s)) (char= (char s (1- (length s))) #\/))
                  s (concatenate 'string s "/")))))

(defun watch-sha256-hex (string)
  "SHA-256 of STRING's UTF-8 bytes as hex, or nil if ironclad is absent."
  (ignore-errors
   (let ((digest (find-symbol "DIGEST-SEQUENCE" :ironclad))
         (hex (find-symbol "BYTE-ARRAY-TO-HEX-STRING" :ironclad)))
     (and digest hex
          (funcall hex (funcall digest :sha256
                                (babel:string-to-octets string :encoding :utf-8)))))))

(defun watch-log-file (&optional (day (day-key)))
  (merge-pathnames (format nil "watch-~a.log" day) (watch-directory)))

(defun watch-last-line (file)
  (ignore-errors
   (with-open-file (in file :if-does-not-exist nil :external-format :utf-8)
     (when in
       (loop with last = nil for line = (read-line in nil) while line
             do (when (plusp (length line)) (setq last line))
             finally (return last))))))

(defun watch-load-chain! ()
  "Recover the chain head from the newest day's log, so a restart keeps
the chain unbroken."
  (or *watch-chain*
      (setf *watch-chain*
            (let* ((files (ignore-errors (directory (merge-pathnames "watch-*.log" (watch-directory)))))
                   (newest (first (sort (copy-list files) #'string> :key #'namestring)))
                   (line (and newest (watch-last-line newest)))
                   (json (and line (ignore-errors (yason:parse line)))))
              (or (and (hash-table-p json) (gethash "h" json)) "genesis")))))

(defun watch-json-object (fields)
  "A JSON object string from FIELDS, a plist of string keys to string or
number values, in the order given (so the canonical text is stable)."
  (with-output-to-string (s)
    (yason:with-output (s)
      (yason:with-object ()
        (loop for (k v) on fields by #'cddr do (yason:encode-object-element k v))))))

(defun watch-record! (kind &key session address detail (level :note))
  "Append one audit record, chained to the one before.  LEVEL :high also
alerts the operator.  Never signals."
  (ignore-errors
   (when (watch-on?)
     (bt:with-lock-held (*watch-lock*)
       (watch-load-chain!)
       (let* ((canonical (watch-json-object
                          (list "t" (get-universal-time)
                                "kind" (string-downcase (symbol-name kind))
                                "level" (string-downcase (symbol-name level))
                                "session" (or (and session (session-id session)) "")
                                "address" (or address (and session (session-address session)) "")
                                "detail" (if detail (princ-to-string detail) "")
                                "prev" *watch-chain*)))
              (hash (or (watch-sha256-hex (concatenate 'string *watch-chain* canonical)) "no-hash"))
              (line (concatenate 'string (subseq canonical 0 (1- (length canonical)))
                                 (format nil ",\"h\":~s}" hash))))
         (ensure-directories-exist (watch-directory))
         (with-open-file (out (watch-log-file) :direction :output :if-exists :append
                                               :if-does-not-exist :create :external-format :utf-8)
           (write-line line out))
         (setf *watch-chain* hash)))
     (when (and (eq level :high) *watch-alerts?*)
       (watch-alert! kind session address detail)))))

(defun watch-alert! (kind session address detail)
  "Relay a high flag to the operator through the gate's alert door."
  (ignore-errors
   (gate-post "alert"
              (h "title" (format nil "~(~a~) in the public lab" (symbol-name kind))
                 "text" (format nil "~a~@[~%session ~a~]~@[~%from ~a~]~@[~%~a~]"
                                (lab-title) (and session (session-id session))
                                address (and detail (clip-line (princ-to-string detail) 500)))
                 "priority" "high" "tags" "rotating_light"))))

;; --- what a sandbox should never reach for (substrings, case-folded) ------

(defparameter *watch-source-signals*
  '(("run-program" . :high) ("run-shell-command" . :high) ("make-process" . :high)
    ("do-http-request" . :high) ("usocket" . :high) ("drakma" . :high)
    ("make-socket" . :high) ("/dev/" . :high) ("/proc/" . :high) ("/projects" . :high)
    ("/run/secrets" . :high) ("etc/passwd" . :high) ("getenv" . :high)
    ("environment-variable" . :high) ("messages-url" . :high) ("api-key" . :high)
    ("anthropic" . :high) ("lisply" . :high) ("delete-directory" . :high)
    ("delete-file" . :note) ("rename-file" . :note) ("with-open-file" . :note)
    ("uiop:" . :note) ("asdf:" . :note) ("(load " . :note) ("#." . :note)
    ("net.aserve" . :note) ("sb-ext" . :high) ("excl:" . :high) ("ccl:" . :high))
  "What a model's source should not hold in a sandbox; (substring . level).")

(defparameter *watch-prompt-signals*
  '(("ignore previous" . :high) ("ignore all" . :high) ("disregard" . :high)
    ("system prompt" . :high) ("your instructions" . :high) ("your rules" . :high)
    ("reveal" . :high) ("api key" . :high) ("api-key" . :high) ("run-program" . :high)
    ("/dev/" . :high) ("/proc/" . :high) ("/projects" . :high)
    ("environment variable" . :high) ("exfiltrat" . :high) ("read the file" . :note))
  "Phrases in a prompt that mean trouble; (substring . level).")

(defun watch-scan (text signals)
  "The hits of SIGNALS in TEXT, case-folded, and the highest level found."
  (let ((low (and (stringp text) (string-downcase text))) (hits nil) (worst :note))
    (when low
      (dolist (sig signals)
        (when (search (car sig) low)
          (push (car sig) hits)
          (when (eq (cdr sig) :high) (setq worst :high))))
      (when (and *watch-honeytoken* (search (string-downcase *watch-honeytoken*) low))
        (push "honeytoken" hits) (setq worst :high)))
    (values hits worst)))

(defun watch-source! (session source)
  "Record a model write with any flags; alert on a high one."
  (ignore-errors
   (when (watch-on?)
     (multiple-value-bind (hits worst) (watch-scan source *watch-source-signals*)
       (watch-record! :source :session session :level (if hits worst :note)
                      :detail (if hits
                                  (format nil "~d chars; flags: ~{~a~^, ~}" (length source) hits)
                                  (format nil "~d chars; clean" (length source))))))))

(defun watch-prompt! (session prompt address)
  "Record a prompt with any flags; alert on a high one."
  (ignore-errors
   (when (watch-on?)
     (multiple-value-bind (hits worst) (watch-scan prompt *watch-prompt-signals*)
       (watch-record! :prompt :session session :address address :level (if hits worst :note)
                      :detail (if hits
                                  (format nil "flags: ~{~a~^, ~} | ~a" hits (clip-line prompt 200))
                                  (clip-line prompt 200)))))))

(defun watch-event! (kind session &key address detail (level :note))
  "A plain audit entry (a session opened, a refusal) for a caller to log."
  (watch-record! kind :session session :address address :detail detail :level level))

(defun watch-verify (&optional (day (day-key)))
  "Walk a day's log and check the hash chain.  Values: the line count,
true when every link holds, and the first broken line's number or nil.
The operator's check that the log was not edited; the daily anchor is
the out-of-band head to check the last line against."
  (let ((file (watch-log-file day)) (prev "genesis") (n 0) (intact t) (bad nil))
    (when (probe-file file)
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil) while line
              when (plusp (length line)) do
                (incf n)
                (let* ((json (ignore-errors (yason:parse line)))
                       (hh (and (hash-table-p json) (gethash "h" json)))
                       (cut (search ",\"h\":" line))
                       (canon (and cut (concatenate 'string (subseq line 0 cut) "}")))
                       (want (and canon (watch-sha256-hex (concatenate 'string prev canon)))))
                  (when (and intact (not (and hh (equal (gethash "prev" json) prev) (equal hh want))))
                    (setq intact nil bad n))
                  (setq prev (or hh prev))))))
    (values n intact bad)))

;; --- the honeytoken, planted where a prowler would look ------------------

(defun watch-plant-honeytoken! ()
  (ignore-errors
   (when (and (watch-on?) *watch-honeytoken-file*)
     (setq *watch-honeytoken* (format nil "sk-ant-api03-~a~a" (new-owner-key) (new-owner-key)))
     (with-open-file (out *watch-honeytoken-file* :direction :output
                                                  :if-exists :supersede :if-does-not-exist :create)
       (format out "# Anthropic API key (prompt lab)~%ANTHROPIC_API_KEY=~a~%" *watch-honeytoken*))
     t)))

;; --- the daily anchor: an out-of-band copy of the chain head -------------

(defun watch-anchor! ()
  (ignore-errors
   (when (and (watch-on?) *watch-alerts?*)
     (let* ((file (watch-log-file))
            (count (if (probe-file file)
                       (with-open-file (in file :external-format :utf-8)
                         (loop for l = (read-line in nil) while l count (plusp (length l))))
                       0)))
       (gate-post "alert"
                  (h "title" "public lab: daily audit anchor"
                     "text" (format nil "~a~%~a records today~%chain head ~a"
                                    (lab-title) count (watch-load-chain!))
                     "priority" "low" "tags" "lock"))))))

(defun start-watch-anchor! ()
  (when (and (watch-on?) (not *watch-anchor-thread*))
    (setf *watch-anchor-thread*
          (bt:make-thread (lambda () (loop (ignore-errors (watch-anchor!)) (sleep (* 24 3600))))
                          :name "prompt-lab watch anchor"))))

(defun start-watch! ()
  "Begin monitoring: load the chain, plant the honeytoken, write a boot
mark (so a restart is visible in the log), start the daily anchor."
  (when (watch-on?)
    (ensure-directories-exist (watch-directory))
    (watch-load-chain!)
    (watch-plant-honeytoken!)
    (watch-record! :boot :detail (format nil "~a watching" (lab-title)))
    (start-watch-anchor!)
    (format t "~&[prompt-lab] watch on: ~a~:[~; (honeytoken planted)~]~%"
            *watch-root* *watch-honeytoken*)))
