(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage coaster
	    (:use :cl)
	    (:export #:run))
