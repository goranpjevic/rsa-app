(ql:quickload :ltk)

(load "src/generate-rsa-keys.lisp")

(defun main (*posix-argv*)
  ; gui main function
  (ltk:with-ltk
    ()
    ; set window title
    (ltk:wm-title ltk:*tk* "rsa")
    ; initialize gui widgets
    (let* ((number-of-bits-label (make-instance 'ltk:label :text "number of bits for keys"))
	   (private-key-label (make-instance 'ltk:label :text "private key file"))
	   (public-key-label (make-instance 'ltk:label :text "public key file"))

	   (number-of-bits-entry (make-instance 'ltk:entry))
	   (private-key-entry (make-instance 'ltk:entry))
	   (public-key-entry (make-instance 'ltk:entry))

	   (generate-keys-button
	     (make-instance 'ltk:button
			    :text "generate keys"
			    :command
			    (lambda ()
			      (generate-keys
				(read-from-string (ltk:text number-of-bits-entry)))))))

      ; put gui widgets on grid
      (ltk:grid private-key-label 0 0 :padx 5 :pady 5)
      (ltk:grid private-key-entry 0 1 :padx 5 :pady 5)
      (ltk:grid public-key-label 1 0 :padx 5 :pady 5)
      (ltk:grid public-key-entry 1 1 :padx 5 :pady 5)
      (ltk:grid number-of-bits-label 2 0 :padx 5 :pady 5)
      (ltk:grid number-of-bits-entry 2 1 :padx 5 :pady 5)
      (ltk:grid generate-keys-button 3 0 :padx 5 :pady 5))))
