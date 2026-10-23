;;
;; Unit checks for THE LOCK (source/guards.lisp): the password file,
;; the token file and where an open lab leaves them.  Load into a Lisp
;; that has prompt-lab loaded (a dev ship's bridge, never a public lab)
;; and call
;;
;;   (prompt-lab::lock-run-checks)             ; files under /tmp/prompt-lab-lock-unit/
;;
;; It returns (:passed N :failed M :failures (...)).  The lock's specials
;; are rebound for every check, so the running lab is untouched and
;; nothing under /projects is written.
;;

(in-package :prompt-lab)

(defun %lock-test-write (file text)
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line text out))
  file)

(defun %lock-test-first-line (file)
  (with-open-file (in file :if-does-not-exist nil)
    (and in (string-trim '(#\Space #\Tab #\Return #\Newline) (or (read-line in nil) "")))))

(defun lock-run-checks (&key (dir "/tmp/prompt-lab-lock-unit/"))
  (let ((results nil))
    (macrolet ((check (name form)
                 `(push (cons ,name (handler-case (and ,form t)
                                      (error (e) (format nil "error: ~a" e))))
                        results)))
      (ignore-errors (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore))
      (let* ((pw (format nil "~aprompt-lab-password" dir))
             (tok (format nil "~aprompt-lab-unlock-token" dir))
             (shelf (format nil "~ashelf/prompt-lab-unlock-token" dir)))
        (ensure-directories-exist shelf)

        ;; --- where the token lives
        (check "the token lives beside a named password file"
               (let ((*password-file* pw)) (equal (unlock-token-file) tok)))
        (check "... else on the shelf"
               (let ((*password-file* nil) (*unlock-token-file* shelf)) (equal (unlock-token-file) shelf)))

        ;; --- locked: the password read, the token minted beside it
        (%lock-test-write pw "secret-1")
        (let ((*password* nil) (*unlock-token* nil) (*password-file* pw) (*unlock-token-file* shelf))
          (check "a password file locks the lab" (load-password!))
          (check "the password is the file's first line" (equal *password* "secret-1"))
          (check "the token is minted beside the password file"
                 (and (probe-file tok) (stringp *unlock-token*) (plusp (length *unlock-token*))))
          (check "the token file holds the token" (equal (%lock-test-first-line tok) *unlock-token*))
          (check "the shelf is untouched while a password file is named" (null (probe-file shelf))))

        ;; --- locked again: the token read back, not minted anew
        (let ((before (%lock-test-first-line tok)))
          (let ((*password* nil) (*unlock-token* nil) (*password-file* pw) (*unlock-token-file* shelf))
            (load-password!)
            (check "a restart reads the token back rather than minting anew" (equal *unlock-token* before))))

        ;; --- the password file gone: open, and the stale token beside it goes
        (delete-file pw)
        (let ((*password* nil) (*unlock-token* "stale") (*password-file* pw) (*unlock-token-file* shelf))
          (check "no password file: the lab is open" (null (load-password!)))
          (check "... and the stale token beside it is gone" (null (probe-file tok)))
          (check "... and no token is held" (null *unlock-token*)))

        ;; --- opened by naming no password file (the public workshop's init): the shelf's token goes
        (%lock-test-write shelf "shelf-token")
        (let ((*password* nil) (*unlock-token* nil) (*password-file* nil) (*unlock-token-file* shelf))
          (check "no password file named: the lab is open" (null (load-password!)))
          (check "... and the shelf's token is removed" (null (probe-file shelf))))

        ;; --- open with nothing to remove: quiet
        (let ((*password* nil) (*unlock-token* nil) (*password-file* nil) (*unlock-token-file* shelf))
          (check "open with no token file anywhere is quiet" (null (load-password!))))

        ;; --- a password from the init, no file named: locked, the token on the shelf
        (let ((*password* "from-init") (*unlock-token* nil) (*password-file* nil) (*unlock-token-file* shelf))
          (check "a password from the init locks the lab" (load-password!))
          (check "... and mints the token on the shelf"
                 (and (probe-file shelf) (equal (%lock-test-first-line shelf) *unlock-token*))))

        ;; --- admitted? follows the lock
        (let ((*password* nil)) (check "an open lab admits anyone" (admitted? nil)))
        (let ((*password* "x") (*unlock-token* "t")) (check "a password set is the lab locked" (locked?)))))
    (let ((failures (remove t (reverse results) :key #'cdr)))
      (list :passed (count t results :key #'cdr) :failed (length failures) :failures failures))))
