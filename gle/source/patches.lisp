(in-package :gwl)

(excl:without-package-locks
  (defun publish-gwl-app (path string-or-symbol &key make-object-args (server *wserver*) host)
    "Void. Publishes an application, optionally with some initial arguments to be passed in as input-slots.

:arguments (path \"String. The URL pathname component to be published.\"
            string-or-symbol \"String or symbol. The object type to insantiate.\")

:&key (make-object-args \"Plist. Extra arguments to pass to make-object.\")
"

    (publish :path path
	     :host host 
	     :server server
             :function #'(lambda(req ent)
                           (gwl-make-object req ent 
                                            (format nil (if (stringp string-or-symbol) "~a" "~s")
                                                    string-or-symbol)
                                            :make-object-args make-object-args)))))
