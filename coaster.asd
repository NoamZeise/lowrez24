(load "../gficl/gficl.asd")

(require 'asdf)
(in-package :asdf-user)

(defsystem :coaster
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "coaster"
  :entry-point "coaster:run"
  :depends-on (:gficl
	       :gficl/load)
  :components ((:module "src"
                :components 
                ((:file "package")
		 (:file "main" :depends-on ("post" "offscreen" "object" "assets" "path"))
		 (:file "post")
		 (:file "offscreen")
		 (:file "object")
		 (:file "assets")
		 (:file "path")
		 (:file "cam")
		 (:file "player")))))
