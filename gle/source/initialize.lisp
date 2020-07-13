(in-package :gle)

(defun initialize! ()
  (with-all-servers (server)
    (dolist (host (list "greenlightescrow.com" "www.greenlightescrow.com"))
      (publish-gwl-app  "/" 'landing :server server :host host))))

;;
;; FLAG -- do this conditionally upon loading for development (already done in production restart-init-function). 
;;
;;(initialize!)

