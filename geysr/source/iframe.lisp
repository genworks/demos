;;
;; Copyright (c) 2003, 2010 Genworks International, Bloomfield Hills,
;; MI, USA. The software, data and information contained herein are
;; proprietary to, and comprise valuable trade secrets of, Genworks
;; International and Genworks BV.  They are given in confidence
;; pursuant to a written nondisclosure/noncompete license agreement,
;; and may be stored and used only in accordance with the terms of
;; such license.
;;
;;


(in-package :geysr)

(define-object iframe (sheet-section)

  :input-slots (url)

  :computed-slots ((div-class "gwl-iframe-section")
		   (inner-html
		    (with-cl-who-string ()
				 (if (the url)
				   (htm ((:iframe :id "EmbeddedUI" :title "Embedded UI"
						  :height "100%" :width "100%"
						  :seamless :seamless
						  :src (the url))))
				   (htm
				    (:p (:h2 (fmt "Welcome to Geysr.")))

				    (:p (fmt
		    "Geysr is for inspecting Genworks GDL models
                   and user interfaces while you're developing them."))

				    (:p (fmt "No object is currently selected and you're 
in User Interface mode."))

				    (:p (fmt "Select a node from the tree atleft 
to display its User Interface, if one exists."))))))))
