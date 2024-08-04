(in-package :lowres24)

(defun setup-offscreen ()
  (setf *offscreen-resolve-fb* nil)
  (let ((samples 0))
    (if (> (setf samples (min 32 (gl:get-integer :max-samples))) 1)
	(setf *offscreen-resolve-fb*
	      (gficl:make-framebuffer
	       (list (gficl:make-attachment-description :color-attachment0 :texture))
	       *target-width* *target-height*)))
    (setf *offscreen-fb*
	  (gficl:make-framebuffer
	   (list (gficl:make-attachment-description
		  :color-attachment0 (if *offscreen-resolve-fb* :renderbuffer :texture))
		 (gficl:make-attachment-description :depth-stencil-attachment))
	   *target-width* *target-height* samples)))
  (setf *main-shader* (gficl:make-shader
		       (alexandria:read-file-into-string #p"shaders/main.vert")
		       (alexandria:read-file-into-string #p"shaders/main.frag"))))

(defun cleanup-offscreen ()
  (if *offscreen-resolve-fb* (gficl:delete-gl *offscreen-resolve-fb*))
  (gficl:delete-gl *offscreen-fb*)
  (gficl:delete-gl *main-shader*))

(defmacro with-offscreen (&body body)
  `(progn
     (begin-offscreen)
     ,@body
     (end-offscreen)))

;;; --- Helpers ----

(defun begin-offscreen ()
  (gficl:bind-gl *offscreen-fb*)
  (gl:viewport 0 0 *target-width* *target-height*)
  (gl:clear-color 0.05 0.2 0.1 0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :depth-test)
  (gficl:bind-gl *main-shader*))

(defun end-offscreen ()
  (if *offscreen-resolve-fb*
      (gficl:blit-framebuffers *offscreen-fb* *offscreen-resolve-fb*
			       *target-width* *target-height*)))

;;; ---- Globals ----

(defparameter *vert-form*
	      (gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :float)
					    (gficl:make-vertex-slot 3 :float)
					    (gficl:make-vertex-slot 2 :float))))
(defparameter *main-shader* nil)

(defparameter *target-width* 64)
(defparameter *target-height* 64)

(defparameter *offscreen-fb* nil)
(defparameter *offscreen-resolve-fb* nil)
