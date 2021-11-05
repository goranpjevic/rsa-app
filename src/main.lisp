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
  ; return solutions for x:
  ; a*x≡b(mod n)
  (multiple-value-bind (d x) (extended-euclid a n)
    (if (= (mod d b) 0)
      (let ((x0 (mod (* x (floor b d)) n)))
	(dotimes (i d)
	  (print (mod (+ x0 (* i (floor n d))) n))))
      (print "no solution"))))

(defun main (*posix-argv*)
  ; gui main function
  (ltk:with-ltk ()
    ; set window title
    (ltk:wm-title ltk:*tk* "rsa")
    ; initialize gui widgets
    (let ((rsa-button (make-instance 'ltk:button :text "rsa")))
      ; put gui widgets on grid
      (ltk:grid rsa-button 0 0 :padx 5 :pady 5))))
