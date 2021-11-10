src = $(wildcard src/*.lisp)

run: $(src)
	rlwrap sbcl --noinform --load $(HOME)/quicklisp/setup.lisp --load src/main.lisp --eval "(main ())" --eval "(quit)"

build: $(src)
	buildapp --load "$(HOME)/quicklisp/setup.lisp" --load "src/main.lisp" --entry main --output build/rsa
