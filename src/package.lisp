(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage lowres24
	    (:use :cl)
	    (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront))
	    (:export #:run))
