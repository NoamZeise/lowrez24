LISP="sbcl"
build: ql copy

ql:
	$(LISP) --eval "(ql:quickload :deploy)" \
		--load "lowres24.asd" \
                --eval "(ql:quickload :lowres24)" \
                --eval "(asdf:make :lowres24)"

asdf: # build without quicklisp 
	$(LISP) --load "lowres24.asd" \
                --eval "(asdf:load-system :lowres24)" \
                --eval "(asdf:make :gficl-examples)"

copy:
	cp -r assets/ bin/
	cp -r shaders/ bin/

clean:
	rm -r bin
