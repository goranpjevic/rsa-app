; functions used to work with lists

(in-package #:rsa-app)

(defun number-to-list-of-bits (byte-to-convert &optional (size 8))
  ; convert a byte to a list of bits
  (let ((bits '()))
    (dotimes (index size bits)
      (push (if (logbitp index byte-to-convert) 1 0) bits))))

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
