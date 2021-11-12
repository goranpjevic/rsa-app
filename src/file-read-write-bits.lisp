; read and write a list bits from and to files

(defun number-to-list-of-bits (byte-to-convert &optional (size 8))
  ; convert a byte to a list of bits
  (let ((bits '()))
    (dotimes (index size bits)
      (push (if (logbitp index byte-to-convert) 1 0) bits))))

(defun read-list-of-bits-from-file (input-file-stream &optional (lst '()))
  ; return list of bits from the input file
  (let ((byte-read (read-byte input-file-stream nil)))
    (if byte-read
      (read-list-of-bits-from-file
        input-file-stream
        (append lst (number-to-list-of-bits byte-read)))
      lst)))

(defun list-of-bits-to-number (list-of-bits &optional (index 0) (number 0))
  ; convert a list of bits to a number
  (if (null list-of-bits)
    number
    (list-of-bits-to-number
      (butlast list-of-bits)
      (1+ index)
      (+ number
         (if (= 1 (car (last list-of-bits)))
           (expt 2 index)
           0)))))

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

(defun split-list-by-n (list n)
  ; split a list to a list of lists of length n
  (do ((nn (1- n) (1- nn))
       (part '())
       (parts '()))
    ((endp list)
     (nreverse (if (endp part) parts
		 (list* (nreverse part) parts))))
    (push (pop list) part)
    (when (zerop nn)
      (push (nreverse part) parts)
      (setf part '()
	    nn n))))
