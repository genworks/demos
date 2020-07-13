(in-package :gle)

(defun initialize! ()
  (with-all-servers (server)
    (publish-gwl-app "/gle" 'landing :server server)))


(initialize!)

