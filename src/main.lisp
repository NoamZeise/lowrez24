(in-package :lowres24)

(defun run ()
  (gficl:with-window
   (:title "lowres24" :width 500 :height 500 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))
    (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.3) *cam-up*))
    (gficl:bind-gl *main-shader*)
    (gficl:bind-matrix
     *main-shader* "view"
     (gficl:view-matrix *cam-pos* (gficl:-vec *cam-pos*) *cam-up*))
    (gficl:bind-vec *main-shader* "cam_pos" *cam-pos*)))

(defun draw ()
  (gficl:with-render
   (with-offscreen
    (gficl:draw-vertex-data *model*))
   (draw-post (gficl:framebuffer-texture-id
	       (if *offscreen-resolve-fb* *offscreen-resolve-fb* *offscreen-fb*) 0))))

(defun resize (w h)
  (resize-post *target-width* *target-height* w h))

(defun setup ()
  ;;(gl:enable :cull-face)
  (let ((mesh (car (obj:extract-meshes (obj:parse (probe-file #p"assets/fish.obj"))))))
    ;;(format t "~a~%" (obj:attributes mesh))
    (setf *model* (gficl:make-vertex-data-from-vectors
		   *vert-form* (obj:vertex-data mesh) (obj:index-data mesh))))
  (setf *cam-pos* (gficl:make-vec '(5 2 3)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))
  (setup-offscreen)
  (setup-post)
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "model" (gficl:make-matrix))
  (gficl:bind-matrix *main-shader* "normal_mat"
		     (gficl:transpose-matrix (gficl:inverse-matrix (gficl:make-matrix))))
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  (resize (gficl:window-width) (gficl:window-height)))

(defun cleanup ()
  (gficl:delete-gl *model*)
  (cleanup-offscreen)
  (cleanup-post))

;;; ---- Globals ----

(defparameter *cam-up* (gficl:make-vec '(0 1 0)))
(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)

(defparameter *model* nil)

