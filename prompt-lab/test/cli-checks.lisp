;;
;; Checks for the command line's tokens and doors (private.lisp COMMAND-LINE
;; TOKENS, deploy.lisp THE COMMAND LINE).  Load into a Lisp that has
;; prompt-lab loaded -- a throwaway lab, never a public one -- and call
;;
;;   (prompt-lab::cli-run-checks)                       ; the functions alone
;;   (prompt-lab::cli-run-checks :base "http://127.0.0.1:9080")
;;
;; With :base, the lab at that address must have its private front published
;; (publish-private-front!) over the same spool and deployed root the checks
;; bind, and must take no deployments itself (*deploy-front* nil): the doors
;; are then asked over HTTP as a program would ask them.  Files go under
;; /tmp/prompt-lab-cli-unit/.  Returns (:passed N :failed M :failures (...)).
;;

(in-package :prompt-lab)

(defun %cli-test-ask (method url &key bearer json)
  "Values: the answer's JSON (a hash table, or nil) and its status."
  (multiple-value-bind (body status)
      (net.aserve.client:do-http-request url :method method
        :headers (and bearer (list (cons "Authorization" (format nil "Bearer ~a" bearer))))
        :content (and json (encode json)) :content-type (and json "application/json"))
    (values (ignore-errors (yason:parse body)) status)))

(defun %cli-test-file-text (file)
  (if (probe-file file) (uiop:read-file-string file) ""))

(defun cli-run-checks (&key base (dir "/tmp/prompt-lab-cli-unit/"))
  (let ((results nil))
    (macrolet ((check (name form)
                 `(push (cons ,name (handler-case (and ,form t)
                                      (error (e) (format nil "error: ~a" e))))
                        results)))
      ;; --- what the command line takes
      (check "a file with (define-object app ...) is a web app" (defines-app? "(define-object app (prompt-lab:web-app)) "))
      (check "... in any case and spacing" (defines-app? (format nil "(DEFINE-OBJECT~%  App(web-app))")))
      (check "a model file is not" (not (defines-app? "(define-object model (base-object))")))
      (check "nor an apple" (not (defines-app? "(define-object apple (base-object))")))
      (check "the figures of a text: bytes, lines"
             (multiple-value-bind (bytes lines sha) (text-figures (format nil "ab~%c"))
               (and (= bytes 4) (= lines 2) (= (length sha) 64))))

      ;; --- the tokens, kept as hashes in the spool
      (ignore-errors (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore))
      (let ((spool (format nil "~aspool/" dir))
            (root (format nil "~adeployed/" dir)))
        (ensure-directories-exist (format nil "~arequests/" spool))
        (ensure-directories-exist (format nil "~aready/" spool))
        (ensure-directories-exist root)
        (let* ((*private-spool* spool)
               (*private-deployed-root* root)
               (wallet "aaaaaaaaaaaaaaaaaaaaaaaa")
               (other "bbbbbbbbbbbbbbbbbbbbbbbb")
               (one (make-cli-token! "rig-one" wallet)))
          (check "a token reads pltok_ and 32 hex digits"
                 (and (= (length one) 38) (string= "pltok_" one :end2 6)
                      (every #'(lambda (c) (digit-char-p c 16)) (subseq one 6))))
          (check "the spool keeps its hash, never the token"
                 (let ((text (%cli-test-file-text (cli-tokens-file))))
                   (and (search (cli-token-hash one) text) (not (search one text)))))
          (check "the token is found by itself" (equal (gethash "name" (cli-token-entry one)) "rig-one"))
          (check "a stranger's string is not" (null (cli-token-entry "pltok_00000000000000000000000000000000")))
          (let ((two (make-cli-token! "rig-one" wallet)))
            (check "a new token for the same name replaces the old"
                   (and (null (cli-token-entry one)) (cli-token-entry two) (= (length (cli-tokens)) 1)))
            (make-cli-token! "rig-one" other)
            (make-cli-token! "rig-two" wallet)
            (check "another author's and another name's are their own" (= (length (cli-tokens)) 3))
            (check "its state: made, not yet used"
                   (let ((state (cli-token-state "rig-one" wallet)))
                     (and (integerp (gethash "made" state)) (null (gethash "used" state)))))
            (note-cli-token-used! (cli-token-hash two))
            (check "... then used" (integerp (gethash "used" (cli-token-state "rig-one" wallet))))
            (drop-cli-tokens! "rig-one" wallet)
            (check "dropped for its author, kept for the other"
                   (and (null (cli-token-entry two)) (cli-token-state "rig-one" other) (cli-token-state "rig-two" wallet))))

          ;; --- the doors, asked as a program asks them
          (when base
            (let* ((front (format nil "~a~a/api" base *private-prefix*))
                   (lab (format nil "~a~a/api" base *url-prefix*))
                   (token (make-cli-token! "rig-cli" wallet))
                   (vm-token (new-owner-key))
                   (id (format nil "pl-~a" (subseq (new-owner-key) 0 10))))
              (check "where: no token is refused 403"
                     (eql 403 (nth-value 1 (%cli-test-ask :get (format nil "~a/cli/where" front)))))
              (check "where: a wrong token is refused 403"
                     (eql 403 (nth-value 1 (%cli-test-ask :get (format nil "~a/cli/where" front)
                                                          :bearer "pltok_00000000000000000000000000000000"))))
              (check "where: a good token with no session open is 404"
                     (eql 404 (nth-value 1 (%cli-test-ask :get (format nil "~a/cli/where" front) :bearer token))))
              ;; a session of this author's, up
              (write-private-request! id wallet vm-token :monetize :free :standing "trusted")
              (with-open-file (out (format nil "~aready/~a.json" spool id) :direction :output :if-exists :supersede)
                (write-string (encode (h "id" id "state" "up" "host" (format nil "~a.example" id))) out))
              (multiple-value-bind (answer status) (%cli-test-ask :get (format nil "~a/cli/where" front) :bearer token)
                (check "where: the session's address and state" (and (eql status 200) (equal (gethash "state" answer) "up")
                                                                      (search id (gethash "url" answer))))
                (check "... and never its token" (and answer (null (gethash "token" answer))
                                                      (not (search vm-token (encode answer))))))
              (check "open: a session already open is answered, none filed"
                     (multiple-value-bind (answer status) (%cli-test-ask :post (format nil "~a/cli/open" front) :bearer token)
                       (and (eql status 200) (equal (gethash "id" answer) id)
                            (= 1 (length (directory (format nil "~arequests/*.json" spool)))))))
              ;; the session's own doors
              (flet ((check-door (cli-token name)
                       (nth-value 1 (%cli-test-ask :post (format nil "~a/vm/cli-check" front)
                                                   :json (h "id" id "token" vm-token "cli_token" cli-token "name" name)))))
                (check "check: no such deployment is refused" (eql 403 (check-door token "rig-cli")))
                ;; the deployment, the author's, in the private root
                (with-private-house
                  (let ((file (merge-pathnames "deployment.json" (deployment-directory "rig-cli"))))
                    (ensure-directories-exist file)
                    (with-open-file (out file :direction :output :if-exists :supersede)
                      (write-string (encode (h "name" "rig-cli" "title" "Rig" "author" wallet "kind" "app"
                                               "engine" (engine-name) "closed" 'yason:false))
                                    out))))
                (check "check: the author's token for its name passes" (eql 200 (check-door token "rig-cli")))
                (check "check: ... not for another name" (eql 403 (check-door token "rig-two")))
                (check "check: another session's credentials are refused"
                       (eql 403 (nth-value 1 (%cli-test-ask :post (format nil "~a/vm/cli-check" front)
                                                            :json (h "id" id "token" (new-owner-key) "cli_token" token "name" "rig-cli")))))
                (multiple-value-bind (answer status)
                    (%cli-test-ask :post (format nil "~a/vm/cli-token" front)
                                   :json (h "id" id "token" vm-token "name" "rig-cli" "action" "make"))
                  (let ((made (and answer (gethash "token" answer))))
                    (check "make: a new token, answered once" (and (eql status 200) (stringp made) (cli-token-entry made)))
                    (check "make: ... the old one gone" (eql 403 (check-door token "rig-cli")))
                    (check "revoke: refused for a deployment not the author's"
                           (eql 404 (nth-value 1 (%cli-test-ask :post (format nil "~a/vm/cli-token" front)
                                                                :json (h "id" id "token" vm-token "name" "rig-two" "action" "revoke")))))
                    (%cli-test-ask :post (format nil "~a/vm/cli-token" front)
                                   :json (h "id" id "token" vm-token "name" "rig-cli" "action" "revoke"))
                    (check "revoke: the token no longer passes" (eql 403 (check-door made "rig-cli"))))))
              (check "the lab's own command-line door, where it deploys nothing: 404"
                     (eql 404 (nth-value 1 (%cli-test-ask :post (format nil "~a/cli/deploy" lab) :bearer token
                                                          :json (h "name" "rig-cli" "source" "(define-object app ())")))))))))
      ;; --- a deployment's files: kept when none are named, and shown to a
      ;; browser only as the app lists them (deploy.lisp PUBLIC FILES)
      (setq results (append (reverse (cli-files-checks :base base)) results))
      (let ((failures (remove t results :key #'cdr)))
        (list :passed (- (length results) (length failures)) :failed (length failures)
              :failures (reverse failures))))))

(defparameter *cli-test-jpeg*
  (coerce '(255 216 255 224 0 16 74 70 73 70 0 1 1 0 0 1 0 1 0 0 255 217) '(vector (unsigned-byte 8)))
  "A JPEG's first bytes, enough for the lab to take it for one.")

(defun %cli-test-app (public-files)
  (format nil "(define-object app (prompt-lab:web-app)
  :input-slots ((title \"Rig\") (public-files (list ~{~s~^ ~})))
  :computed-slots ((body (with-lhtml-string () (:p \"rig\")))))" public-files))

(defun %cli-test-get (url &key headers)
  "Values: the status and the answer's headers (an alist)."
  (multiple-value-bind (body status headers)
      (net.aserve.client:do-http-request url :headers headers :format :binary)
    (declare (ignore body))
    (values status headers)))

(defun cli-files-checks (&key base)
  "The files checks, a list of (name . t-or-why).  A lab of its own deploys
here (*deploy-front* nil); with BASE its doors are asked over HTTP."
  (let ((results nil))
    (macrolet ((check (name form)
                 `(push (cons ,name (handler-case (and ,form t)
                                      (error (e) (format nil "error: ~a" e))))
                        results)))
      (let* ((*deploy-front* nil)
             (*deployments?* t)
             (session (make-session :address "127.0.0.1"))
             (name "rig-files")
             (folder (merge-pathnames "files/" (deployment-directory name)))
             (held (lambda () (sort (mapcar #'file-namestring (directory (merge-pathnames "*.*" folder))) #'string<))))
        (unwind-protect
             (progn
               (set-session-kind! session :app)
               (write-model session (%cli-test-app '("sky.jpg" "draw.svg")))
               (add-upload! session "sky.jpg" *cli-test-jpeg*)
               (add-upload! session "notes.txt" (babel:string-to-octets "some notes"))
               (add-upload! session "draw.svg" (babel:string-to-octets "<svg xmlns='http://www.w3.org/2000/svg'></svg>"))
               (check "deploy: the session's files go with it"
                      (and (deploy-session! session :name name :title "Rig")
                           (equal (funcall held) '("draw.svg" "notes.txt" "sky.jpg"))))
               ;; the session's files gone, a deploy that names none keeps the deployment's
               (dolist (file (session-files session)) (delete-file (the-object file file-path)))
               (check "carry-forward: a deploy naming no files keeps them"
                      (and (deploy-session! session :name name :title "Rig")
                           (equal (funcall held) '("draw.svg" "notes.txt" "sky.jpg"))))
               (check "carry-forward: :keep keeps them too"
                      (and (deploy-session! session :name name :title "Rig" :files :keep)
                           (= 3 (length (funcall held)))))
               (check "an empty list clears them"
                      (and (deploy-session! session :name name :title "Rig" :files :session)
                           (null (funcall held))))
               ;; back, for the doors
               (add-upload! session "sky.jpg" *cli-test-jpeg*)
               (add-upload! session "notes.txt" (babel:string-to-octets "some notes"))
               (add-upload! session "draw.svg" (babel:string-to-octets "<svg xmlns='http://www.w3.org/2000/svg'></svg>"))
               (deploy-session! session :name name :title "Rig")
               (let ((url (own-file-url-for (session-package-name (ensure-deployed name)) "sky.jpg")))
                 (check "own-file-url: the deployment's address with ?v= the hash"
                        (and url (search (format nil "/d/~a/files/sky.jpg?v=" name) url)
                             (= 8 (- (length url) (+ (search "?v=" url) 3)))))
                 (check "own-file-url: a session's preview address"
                        (search "/app-files?session=" (or (own-file-url-for (session-package-name session) "sky.jpg") "")))
                 (when base
                   (let ((at (lambda (file) (format nil "~a~a/d/~a/files/~a" base *url-prefix* name file))))
                     (multiple-value-bind (status headers) (%cli-test-get (concatenate 'string (funcall at "sky.jpg") (subseq url (search "?v=" url))))
                       (flet ((header (key) (cdr (assoc key headers :test #'string-equal))))
                         (check "door: a listed image is served" (eql status 200))
                         (check "door: as its kind, never sniffed"
                                (and (equal (header "content-type") "image/jpeg") (equal (header "x-content-type-options") "nosniff")
                                     (null (header "content-disposition"))))
                         (check "door: kept for good, named by its hash"
                                (and (search "immutable" (or (header "cache-control") ""))
                                     (equal (header "etag") (format nil "\"~a\"" (public-file-sha (merge-pathnames "sky.jpg" folder))))))
                         (check "door: asked again with its ETag, 304"
                                (eql 304 (%cli-test-get (funcall at "sky.jpg") :headers (list (cons "If-None-Match" (header "etag"))))))))
                     (check "door: a file the app does not list is 404" (eql 404 (%cli-test-get (funcall at "notes.txt"))))
                     (check "door: a listed SVG is 404 (not a kind it serves)" (eql 404 (%cli-test-get (funcall at "draw.svg"))))
                     (check "door: no way out of the files folder"
                            (and (eql 404 (%cli-test-get (funcall at "..%2Fmodel.lisp")))
                                 (eql 404 (%cli-test-get (funcall at "model.lisp")))
                                 (eql 404 (%cli-test-get (format nil "~a~a/d/~a/files/" base *url-prefix* name)))))
                     (check "preview door: the session's listed image"
                            (eql 200 (%cli-test-get (format nil "~a~a/app-files?session=~a&name=sky.jpg" base *url-prefix* (session-id session)))))
                     (check "preview door: an unlisted file is 404"
                            (eql 404 (%cli-test-get (format nil "~a~a/app-files?session=~a&name=notes.txt" base *url-prefix* (session-id session)))))
                     ;; an app that lists nothing serves nothing
                     (write-model session (%cli-test-app nil))
                     (deploy-session! session :name name :title "Rig")
                     (drop-deployed! name)
                     (check "door: an app with no public-files serves nothing"
                            (eql 404 (%cli-test-get (funcall at "sky.jpg"))))
                     (check "the command line's file door, where the lab deploys nothing: 404"
                            (eql 404 (nth-value 1 (%cli-test-ask :post (format nil "~a~a/api/cli/file" base *url-prefix*)
                                                                :bearer "pltok_00000000000000000000000000000000"
                                                                :json (h "name" name "file" "a.jpg" "data" "/9j/"))))))))
               (check "an asset is never the agent's"
                      (progn (note-asset! session "sky.jpg")
                             (notany #'(lambda (block) (search "sky.jpg" (encode block))) (pending-attachments session)))))
          (ignore-errors (undeploy! name (session-owner session)))
          (ignore-errors (delete-session session)))))
    (nreverse results)))
