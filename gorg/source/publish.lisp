;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :glsite)

(defparameter *gorg-hosts*
  
  (list "gendl.org" "www.gendl.org"
	"gendl.com" "www.gendl.com")

  #+nil
  (list "localhost" "xenie" 
	"gendlformacosx.com" "www.gendlformacosx.com"
	"gendlformacos.com" "www.gendlformacos.com"
	"gendlformacosx.org" "www.gendlformacosx.org"
	"gendlformacos.org" "www.gendlformacos.org"
	"macos.gendl.com" "www.macos.gendl.com"
	"macos.gendl.org" "www.macos.gendl.org"
	"macosx.gendl.com" "www.macosx.gendl.com"
	"macosx.gendl.org"  "www.macosx.gendl.org"
	"gendl.org" "www.gendl.org"
	"gendl.com" "www.gendl.com"))

;;
;; FLAG -- move to GWL supported
;;
(defun publish-redirect
    (&key host server source-path target-url (response-code *response-found*))
  (net.aserve:publish
   :path source-path :host host :server server
   :function #'(lambda(req ent)
                 (with-http-response (req ent :response response-code)
                   (setf (reply-header-slot-value req :cache-control) "no-cache")
                   (setf (reply-header-slot-value req :pragma) "no-cache")
                   (setf (reply-header-slot-value req :location) target-url)
                   (setf (reply-header-slot-value req :response) response-code)
                   (with-http-body (req ent))))))

(defun initialize ()


  (let ((dev (list :windows "/home/builder/genworks/manager/staging/gendl/dev-builds/gendl-devo-windows.zip"
		   :macos "/home/builder/genworks/manager/staging/gendl/dev-builds/gendl-devo-macos.zip"
		   :linux "/home/builder/genworks/manager/staging/gendl/dev-builds/gendl-devo-linux.zip")))

    (dolist (os (plist-keys dev))
      (let* ((file (getf dev os))
	     (path (format nil "/~a" (file-namestring file))))
	(net.aserve:publish-file :path path
				 :file file
				 :content-type "application/zip"
				 :headers (list (cons "Content-Disposition" "attachment"))))))
    
  (let ((static (or
		 (probe-file (merge-pathnames "gorg-static/" glisp:*gdl-program-home*))
		 (when (glisp:source-pathname)
		   (probe-file 
		    (make-pathname 
		     :name nil 
		     :type nil 
		     :defaults (merge-pathnames "../static/" 
						(translate-logical-pathname 
						 (glisp:source-pathname))))))
		 (probe-file (merge-pathnames "static/" *system-home*))
		 )))
    (if static 
	(progn (setq static (namestring static))
	       (setq *templates-folder* (merge-pathnames "templates/" static))
	       (dolist (host *gorg-hosts*)
		 (publish-directory :prefix "/gorgstat/" :destination static :host host)))
	(warn "static directory does not exist in gorg publish.")))


  ;;
  ;; FLAG Put this back in when we get cookies working and lose the ugly
  ;;   "/sessions/..." url
  ;; 
  ;;(publish-gwl-app "/" "glsite:landing")
  ;;

  ;; (publish-shared 'landing :host "gendl.org" :path "/")

  (with-all-servers
      (server)
      (dolist (host (list "www.gendl.org" "gendl.org" "www.gendl.com" "gendl.com"
                          "www.gendl.net" "gendl.net"))

        (publish-redirect
         :host host :server server :source-path "/"
         :target-url "https://gitlab.common-lisp.net/gendl/gendl/-/tree/devo")


        
        (publish-shared 'landing :server server :path "/gorg"))))


  
;;
;; FLAG -- arrange to call this on production startup. 
;;
;;(initialize)
