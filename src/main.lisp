(ql:quickload :ltk)

(load "src/generate-rsa-keys.lisp")
(load "src/file-read-write-bits.lisp")

(defun rsa-encrypt (e n input-file-stream)
  ; return the encrypted list of bits from the input file stream, using the
  ; public rsa key pair (e n)
  ;todo
  ; NOTE: the output length of the encrypted list of bits must be divisible by 8

  (get-list-of-bits-from-file input-file-stream)

  (modular-exponentiation m e n))

(defun rsa-decrypt (d n input-file-stream)
  ; return the decrypted list of bits from the input file stream, using the
  ; private rsa key pair (d n)
  ;todo

  (get-list-of-bits-from-file input-file-stream)

  (modular-exponentiation c d n))
(defmacro rsa-to-file (key-file-entry input-file-entry output-file-entry)
  `(with-open-file (key-file (ltk:text ,key-file-entry))
     (with-open-file (input-file (ltk:text ,input-file-entry)
				 :element-type 'unsigned-byte)
       (with-open-file (output-file (ltk:text ,output-file-entry)
				    :direction :output
				    :if-exists :overwrite
				    :if-does-not-exist :create
				    :element-type 'unsigned-byte)
	 (write-bits-to-file
	   output-file
	   (rsa
	     (read key-file)
	     (read key-file)
	     (read-list-of-bits-from-file input-file)))))))

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
	   (unencrypted-file-label (make-instance 'ltk:label :text "unencrypted file"))
	   (encrypted-file-label (make-instance 'ltk:label :text "encrypted file"))

	   (number-of-bits-entry (make-instance 'ltk:entry))
	   (private-key-entry (make-instance 'ltk:entry))
	   (public-key-entry (make-instance 'ltk:entry))
	   (unencrypted-file-entry (make-instance 'ltk:entry))
	   (encrypted-file-entry (make-instance 'ltk:entry))

	   (generate-keys-button
	     (make-instance
	       'ltk:button :text "generate keys" :command
	       (lambda ()
		 (multiple-value-bind
		   (e d n)
		   (generate-keys
		     (read-from-string (ltk:text number-of-bits-entry)))
		   ; output private and public keys to files
		   (with-open-file (private-key-file
				     (ltk:text private-key-entry)
				     :direction :output)
		     (format private-key-file "~d ~d" e n))
		   (with-open-file (public-key-file
				     (ltk:text public-key-entry)
				     :direction :output)
		     (format public-key-file "~d ~d" d n))))))

	   (encrypt-file-button
	     (make-instance 'ltk:button :text "encrypt file" :command
			    (lambda () (rsa-to-file public-key-entry
						    unencrypted-file-entry
						    encrypted-file-entry))))

	   (decrypt-file-button
	     (make-instance 'ltk:button :text "decrypt file" :command
			    (lambda () (rsa-to-file private-key-entry
						    encrypted-file-entry
						    unencrypted-file-entry)))))

      ; put gui widgets on grid
      (ltk:grid private-key-label 0 0 :padx 5 :pady 5)
      (ltk:grid private-key-entry 0 1 :padx 5 :pady 5)
      (ltk:grid public-key-label 1 0 :padx 5 :pady 5)
      (ltk:grid public-key-entry 1 1 :padx 5 :pady 5)
      (ltk:grid number-of-bits-label 2 0 :padx 5 :pady 5)
      (ltk:grid number-of-bits-entry 2 1 :padx 5 :pady 5)
      (ltk:grid generate-keys-button 3 0 :padx 5 :pady 5)
      (ltk:grid unencrypted-file-label 4 0 :padx 5 :pady 5)
      (ltk:grid unencrypted-file-entry 4 1 :padx 5 :pady 5)
      (ltk:grid encrypted-file-label 5 0 :padx 5 :pady 5)
      (ltk:grid encrypted-file-entry 5 1 :padx 5 :pady 5)
      (ltk:grid encrypt-file-button 6 0 :padx 5 :pady 5)
      (ltk:grid decrypt-file-button 6 1 :padx 5 :pady 5))))
