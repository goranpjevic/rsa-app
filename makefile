run:
	sbcl --noinform --load $(HOME)/.sbclrc --load src/main.lisp --eval "(main ())" --eval "(quit)"
