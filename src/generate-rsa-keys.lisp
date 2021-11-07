; generate rsa keys

(load "src/generate-primes.lisp")

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

(defun generate-keys (number-of-bits)
  (multiple-value-bind (p q) (generate-primes number-of-bits)
    (let* ((n (* p q))
	   (euler (* (1- p) (1- q)))
	   (e (generate-random-odd-coprime 1 euler))
	   (d (modular-linear-equation-solver e 1 euler)))
      (format t "public key: ~d ~d~%" e n)
      (format t "private key: ~d ~d~%" d n))))

