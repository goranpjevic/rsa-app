; read and write a list bits from and to files

(in-package #:rsa-app)

(defun read-list-of-bits-from-file (input-file-stream &optional (lst '()))
  ; return list of bits from the input file
  (let ((byte-read (read-byte input-file-stream nil)))
    (if byte-read
      (read-list-of-bits-from-file
	input-file-stream
	(append lst (number-to-list-of-bits byte-read)))
      lst)))

(defun write-bits-to-file (output-file-stream list-of-bits)
  ; output list of bits to an output file
  (if (not (null list-of-bits))
    (progn
      ; write byte to the output file
      (let ((new-list-of-bits
	      (if (> 0 (- 8 (length list-of-bits)))
		list-of-bits
		(append list-of-bits (make-list (- 8 (length list-of-bits))
						:initial-element 0)))))
	(write-byte (list-of-bits-to-number (subseq new-list-of-bits 0 8))
		    output-file-stream)
	(write-bits-to-file output-file-stream (subseq new-list-of-bits 8))))))
