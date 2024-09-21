(in-package :coaster)

;;; ---- Post Processing Render Pass ----

(defparameter *post-shader* nil)
(defparameter *dummy-data* nil)

(defun setup-post ()
  (setf *post-shader*
	(gficl:make-shader
	 (alexandria:read-file-into-string #p"shaders/post.vert")
	 (alexandria:read-file-into-string #p"shaders/post.frag")))
  (gficl:bind-gl *post-shader*)
  (gl:uniformi (gficl:shader-loc *post-shader* "screen") 0)
  (setf *dummy-data* (gficl:make-vertex-data
		      (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
		      '(((0))) '(0 0 0))))

(defun resize-post (offscreen-w offscreen-h w h)
  (gficl:bind-gl *post-shader*)
  (gficl:bind-matrix
   *post-shader* "transform"
   (gficl:target-resolution-matrix offscreen-w offscreen-h w h)))

(defun cleanup-post ()
  (gficl:delete-gl *post-shader*)
  (gficl:delete-gl *dummy-data*))

(defun draw-post (screen-texture-id)
  (gl:bind-framebuffer :framebuffer 0)
  (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (gl:disable :depth-test)
  (gficl:bind-gl *post-shader*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d screen-texture-id)
  (gficl:bind-gl *dummy-data*)
  (gl:draw-arrays :triangles 0 3))
