LISP="sbcl"

build: ql copy

build-asdf: asdf copy

ql:
	$(LISP) --eval "(ql:quickload :deploy)" \
		--load "coaster.asd" \
                --eval "(ql:quickload :coaster)" \
                --eval "(asdf:make :coaster)"

asdf: # build without quicklisp 
	$(LISP) --load "coaster.asd" \
                --eval "(asdf:load-system :coaster)" \
                --eval "(asdf:make :coaster)"

copy:
	cp -r assets/ bin/
	cp -r shaders/ bin/

clean:
	rm -r bin
