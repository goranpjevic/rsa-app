(in-package #:rsa-app)

(defun rsa (k n list-of-bits number-of-bits-to-read number-of-bits-to-write)
  (mapcan #'(lambda (sublst)
              (number-to-list-of-bits
                (modular-exponentiation
                  (list-of-bits-to-number sublst)
                  k n)
                number-of-bits-to-write))
          (split-list-by-n list-of-bits number-of-bits-to-read)))

(defun rsa-encrypt (e n list-of-bits
                      &optional (number-of-bits (- (ceiling (log n 2)) 1)))
  (rsa e n
       (append list-of-bits
               (make-list (- number-of-bits (mod (length list-of-bits) number-of-bits))
                          :initial-element 0))
       number-of-bits (+ number-of-bits 1)))

(defun rsa-decrypt (d n list-of-bits
                      &optional (number-of-bits (- (ceiling (log n 2)) 1)))
  (let ((rsa-out (rsa d n
                      (subseq list-of-bits 0
                              (- (length list-of-bits)
                                 (let ((remainder (mod (length list-of-bits) (+ number-of-bits 1))))
                                   (if (= remainder 0)
                                     (+ number-of-bits 1)
                                     remainder))))
                      (+ number-of-bits 1) number-of-bits)))
    (subseq rsa-out 0 (- (length rsa-out) (mod (length rsa-out) 8)))))

(defmacro rsa-to-file (key-file-entry input-file-entry output-file-entry rsa-func)
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
           (,rsa-func
             (read key-file)
             (read key-file)
             (read-list-of-bits-from-file input-file)))))))

(defun main ()
  ; gui main function
  (ltk:with-ltk
    ()
    ; set window title
    (ltk:wm-title ltk:*tk* "rsa")
    ; initialize gui widgets
    (let* ((number-of-bits-label
             (make-instance 'ltk:label :text "number of bits for keys"))
           (private-key-label
             (make-instance 'ltk:label :text "private key file"))
           (public-key-label
             (make-instance 'ltk:label :text "public key file"))
           (unencrypted-file-label
             (make-instance 'ltk:label :text "unencrypted file"))
           (encrypted-file-label
             (make-instance 'ltk:label :text "encrypted file"))

           (number-of-bits-entry (make-instance 'ltk:entry))
           (private-key-entry (make-instance 'ltk:entry
                                             :text "keys/private.txt"))
           (public-key-entry (make-instance 'ltk:entry
                                            :text "keys/public.txt"))
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
                     (format private-key-file "~d ~d" d n))
                   (with-open-file (public-key-file
                                     (ltk:text public-key-entry)
                                     :direction :output)
                     (format public-key-file "~d ~d" e n))))))

           (encrypt-file-button
             (make-instance 'ltk:button :text "encrypt file" :command
                            (lambda () (rsa-to-file public-key-entry
                                                    unencrypted-file-entry
                                                    encrypted-file-entry
                                                    rsa-encrypt))))

           (decrypt-file-button
             (make-instance 'ltk:button :text "decrypt file" :command
                            (lambda () (rsa-to-file private-key-entry
                                                    encrypted-file-entry
                                                    unencrypted-file-entry
                                                    rsa-decrypt)))))

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
