#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage lowres24
	    (:use :cl)
	    (:export #:run))
