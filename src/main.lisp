(ql:quickload :ltk)

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

(defun generate-primes (number-of-bits)
  ; return 2 prime numbers of number-of-bits bits
  ; todo
  )

(defun generate-random-odd-coprime (lower-bound upper-bound)
  ; return e
  ; lower-bound < e < upper-bound
  ; gcd(e upper-bound)=1
  (let ((e (+ lower-bound (random (- upper-bound lower-bound)))))
    (if (= (extended-euclid e upper-bound) 1)
      e
      (generate-random-odd-coprime lower-bound upper-bound))))

(defun generate-keys ()
  (multiple-value-bind (p q) (generate-primes number-of-bits)
    (let ((n (* p q))
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
