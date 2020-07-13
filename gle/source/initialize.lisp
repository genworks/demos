(in-package :gle)

(defun initialize! ()
  (with-all-servers (server)
    (dolist (host (list "greenlightescrow.com" "www.greenlightescrow.com"))
      (publish-gwl-app  "/" 'landing :server server :host host))))

(initialize!)

