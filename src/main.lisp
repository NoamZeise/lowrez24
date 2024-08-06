(in-package :lowres24)

(defun run ()
  (gficl:with-window
   (:title "lowres24" :width 640 :height 640 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw-main))
   (cleanup)))

(defun update ()
  (gficl:with-update (dt)		     
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))
    (let ((front (gficl:*vec (* dt 10)
			     (gficl:normalise *cam-pos*)))
	  (up (gficl:*vec (* dt 10) *cam-up*))
	  (pos-updated nil))      
      (gficl:map-keys-down
       ((:w (setf *cam-pos* (gficl:-vec *cam-pos* front)))
	(:s (setf *cam-pos* (gficl:+vec *cam-pos* front)))
	(:a (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.3) *local-left*)))
	(:d (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt -0.3) *local-left*)))
	(:space
	 (setf *cam-target* (gficl:+vec *cam-target* up)))
	(:left-shift
	 (setf *cam-target* (gficl:-vec *cam-target* up)))
	
	(:up (setf pos-updated t)
	     (setf *pos* (gficl:+vec *pos* (list 0 (* dt 10.0) 0))))
	(:down (setf pos-updated t)
	       (setf *pos* (gficl:+vec *pos* (list 0 (* dt -10.0) 0))))))
      (if pos-updated
	  (update-model *model2* (gficl:*mat (gficl:translation-matrix *pos*)))))
    
    ;;(setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.3) *cam-up*))
    ;(format t "fps: ~a~%" (/ 1 (float dt)))
    
    (gficl:bind-gl *main-shader*)
    (let ((pos (gficl:+vec *cam-target* *cam-pos*)))
      (multiple-value-bind (view up left) (gficl:view-matrix pos (gficl:-vec *cam-pos*) *cam-up*)
			   (gficl:bind-matrix *main-shader* "view" view)
			   (gficl:bind-vec *main-shader* "cam_pos" pos)
			   (setf *local-left* left)
			   (setf *local-up* up)))))

(defun draw-main ()
  (gficl:with-render
   (with-offscreen
    (draw *model*)
    (draw *model2*)
    (loop for ball in *balls* do (draw ball)))
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
  (setf *local-up* *cam-up*)
  (setf *local-left* (gficl:cross *cam-up* (gficl:-vec *cam-pos*)))

  (load-model 'fish #p"assets/fish.obj")
  (load-model 'sphere #p"assets/sphere.obj")
  (load-model 'cube #p"assets/cube.obj")
  (load-model 'torus #p"assets/torus.obj")

  (setf *pos* (gficl:make-vec '(0 0 0)))
  (setf *model* (make-go *main-shader* (get-asset 'cube) (gficl:make-matrix)))
  (setf *model2* (make-go *main-shader*
			  (get-asset 'torus)
			  (gficl:translation-matrix *pos*)))
  (setf *balls*
	(loop for i from 0 to 1000 collecting
	      (make-go *main-shader* (get-asset 'sphere) (gficl:translation-matrix
							  (list 0 (* 2 i) 0)))))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  (resize (gficl:window-width) (gficl:window-height)))

(defun cleanup ()
  (cleanup-assets)
  (cleanup-offscreen)
  (cleanup-post))

(defun next-point (time)
  ())

;;; ---- Globals ----

(defparameter *cam-up* (gficl:make-vec '(0 1 0)))
(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)

(defparameter *local-up* nil)
(defparameter *local-left* nil)

(defparameter *model* nil)
(defparameter *model2* nil)

(defparameter *balls* nil)
(defparameter *pos* nil)

(defparameter *points* nil)
