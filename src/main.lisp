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
    (draw-go *model*)
    (draw-go *model2*))
   (draw-post (gficl:framebuffer-texture-id
	       (if *offscreen-resolve-fb* *offscreen-resolve-fb* *offscreen-fb*) 0))))

(defun resize (w h)
  (resize-post *target-width* *target-height* w h))

(defun setup ()
  (setup-offscreen)
  (setup-post)
  (setup-assets)
  (setf *cam-pos* (gficl:make-vec '(5 2 3)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))

  (load-model 'fish #p"assets/fish.obj")
  (load-model 'sphere #p"assets/sphere.obj")
  (load-model 'cube #p"assets/cube.obj")
  
  (setf *model* (make-go *main-shader* (gficl:make-matrix) (get-asset 'cube)))
  (setf *model2* (make-go *main-shader*
			  (gficl:translation-matrix '(5 0 0)) (get-asset 'fish)))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  (resize (gficl:window-width) (gficl:window-height)))

(defun cleanup ()
  (cleanup-assets)
  (cleanup-offscreen)
  (cleanup-post))

;;; ---- Globals ----

(defparameter *cam-up* (gficl:make-vec '(0 1 0)))
(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)

(defparameter *model* nil)
(defparameter *model2* nil)
