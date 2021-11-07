(ql:quickload :ltk)

; seed for the lcg
(defvar *r* 1)
; linear congruential generator
(defun lcg (m a b)
  (setf *r* (mod (+ (* a *r*) b) m)))

(defun random-number-in-range (a b)
  ; get random number in range [a, b]
  (mod (+ a (lcg (expt 2 32) 69069 0)) (- b (1+ a))))

(defun modular-exponentiation (a b n &optional (i (integer-length b)) (d 1))
  ; calculate a^b mod n
  (if (> i 0)
    (if (logbitp (1- i) b)
      (modular-exponentiation a b n (1- i) (mod (* (mod (expt d 2) n) a) n))
      (modular-exponentiation a b n (1- i) (mod (expt d 2) n)))
    d))

(defun miller-rabin (p s)
  ; miller-rabin primality test
  ; return p if it's prime, otherwise nil
  (if (<= p 3) p
    (if (not (evenp p))
      (labels ((find-d-k (d k)
			 ; return d and k, such that d*(2^k)=p-1
			 (if (evenp d)
			   (find-d-k (/ d 2) (1+ k))
			   (values d k)))
	       (update-x (x p k &optional (i 0))
			 ; if ∃i:a^(d*2^i)≡−1 (mod p), then p is probably prime
			 (if (or (> i (1- k))
				 (equal x (1- p)))
			   x
			   (update-x (mod (expt x 2) p) p k (1+ i)))))
	(multiple-value-bind (d k) (find-d-k (1- p) 0)
	  (dotimes (j s p)
	    (let ((x (modular-exponentiation
		       (random-number-in-range 2 (- p 2))
		       d p)))
	      (if (not (or (equal x 1)
			   (equal (update-x x p k) (1- p))))
		(return-from miller-rabin nil)))))))))

(defun generate-random-prime-miller-rabin (random-number s)
  ; use miller-rabin method for testing prime numbers
  (or (miller-rabin random-number s)
      ; r is not prime, check r+2
      (generate-random-prime-miller-rabin (+ random-number 2) s)))

(defun generate-primes (number-of-bits)
  ; return 2 prime numbers of number-of-bits bits
  (generate-random-prime-naive (+ (ash (lcg (expt 2 (- number-of-bits 2))
					    69069 0)
				       1)
				  (expt 2 (1- number-of-bits))
				  1)))

(defun extended-euclid (a b)
  ; return (d x y)
  ; d=gcd(a b)
  ; d=x*a+y*b
  (if (= b 0)
    (values a 1 0)
    (multiple-value-bind (d x y) (extended-euclid b (mod a b))
      (values d y (- x (* (floor a b) y))))))

(defun modular-linear-equation-solver (a b n)
  ; return x
  ; a*x≡b(mod n)
  (multiple-value-bind (d x) (extended-euclid a n)
    (if (= (mod d b) 0)
      (mod (mod (* x (floor b d)) n) n))))

(defun generate-random-odd-coprime (lower-bound upper-bound)
  ; return odd number e
  ; lower-bound < e < upper-bound
  ; gcd(e upper-bound)=1
  (let ((e (logior (+ lower-bound (random (- upper-bound lower-bound))) 1)))
    (if (= (extended-euclid e upper-bound) 1)
      e
      (generate-random-odd-coprime lower-bound upper-bound))))

(defun generate-keys ()
  (multiple-value-bind (p q) ((generate-primes 4) (generate-primes 4))
    (let* ((n (* p q))
	   (euler (* (1- p) (1- q)))
	   (e (generate-random-odd-coprime 1 euler))
	   (d (modular-linear-equation-solver e 1 euler)))
      (format t "public key: ~d ~d~%" e n)
      (format t "private key: ~d ~d~%" d n))))

(defun main (*posix-argv*)
  ; gui main function
  (ltk:with-ltk ()
    ; set window title
    (ltk:wm-title ltk:*tk* "rsa")
    ; initialize gui widgets
    (let ((rsa-button (make-instance 'ltk:button :text "rsa")))
      ; put gui widgets on grid
      (ltk:grid rsa-button 0 0 :padx 5 :pady 5))))
