(load "../gficl/gficl.asd")

(require 'asdf)
(in-package :asdf-user)

(defsystem :lowres24
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "lowres24"
  :entry-point "lowres24:run"
  :depends-on (:gficl
	       :cl-wavefront)
  :components ((:module "src"
                :components 
                ((:file "package")
		 (:file "main" :depends-on ("post" "offscreen" "object" "assets" "path"))
		 (:file "post")
		 (:file "offscreen")
		 (:file "object")
		 (:file "assets")
		 (:file "path")
		 (:file "cam")))))
