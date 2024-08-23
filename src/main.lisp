(in-package :coaster)

(defun run ()
  (gficl:with-window
   (:title "coaster" :width 640 :height 640 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw-main))
   (cleanup)))

(defun update ()
  (gficl:with-update (dt)		     
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen)))
    (let ((pos-updated nil))      
      (gficl:map-keys-down
       (:w
	(setf *dist* (+ *dist* (* 10.0 dt))))
       (:s
	(setf *dist* (- *dist* (* 10.0 dt))))
	
       (:up (setf pos-updated t)
	    (setf *pos* (gficl:+vec *pos* (list 0 (* dt 10.0) 0))))
       (:down (setf pos-updated t)
	      (setf *pos* (gficl:+vec *pos* (list 0 (* dt -10.0) 0)))))
      (if pos-updated
	  (update-model *model2* (gficl:*mat (gficl:translation-matrix *pos*)))))

    (update-path *dist*)
    
    (multiple-value-bind (pos up forward) (get-pos *dist*) (update-cam *cam* dt pos up forward))
    
;;    (format t "fps: ~a~%" (/ 1 (float dt)))
  ;;  (format t "dist: ~a~%" *dist*)
    
    (gficl:bind-gl *main-shader*)
    (gficl:bind-matrix *main-shader* "view" (view-matrix *cam*))
    (gficl:bind-vec *main-shader* "cam_pos" (pos *cam*))))

(defun draw-main ()
  (gficl:with-render
   (with-offscreen
    (draw *model*)
    (draw *model2*)
    (draw-points))
   (draw-post (gficl:framebuffer-texture-id
	       (if *offscreen-resolve-fb* *offscreen-resolve-fb* *offscreen-fb*) 0))))

(defun resize (w h)
  (resize-post *target-width* *target-height* w h))

(defun setup ()
  (setup-offscreen)
  (setup-post)
  (setup-assets)

  (load-model 'fish #p"assets/fish.obj")
  (load-model 'sphere #p"assets/sphere.obj")
  (load-model 'cube #p"assets/cube.obj")
  (load-model 'torus #p"assets/torus.obj")

  (setf *pos* (gficl:make-vec '(0 0 0)))
  (setf *model* (make-go *main-shader* (get-asset 'sphere) (gficl:make-matrix)))
  (setf *model2* (make-go *main-shader*
			  (get-asset 'torus)
			  (gficl:translation-matrix *pos*)))

  (path-setup)
  (setf *dist* 0)
  (setf *cam* (make-camera (gficl:make-vec '(0 0 0))
			   (gficl:make-vec '(0 1 0))
			   (gficl:make-vec '(1 0 0))))
  
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  (resize (gficl:window-width) (gficl:window-height)))

(defun cleanup ()
  (cleanup-assets)
  (cleanup-offscreen)
  (cleanup-post))

(defun draw-points ()
  (loop for p across *path* do
	(progn
	  (update-model *model* (gficl:translation-matrix (pos p)))
	  (draw *model*)
	  ;(update-model *model* (gficl:*mat (gficl:translation-matrix (gficl:+vec (pos p) (up p))) (gficl:scale-matrix '(0.1 0.1 0.1))))
	  ;(draw *model*)
	  )))

;;; ---- Globals ----

(defparameter *cam* nil)

(defparameter *model* nil)
(defparameter *model2* nil)

(defparameter *pos* nil)

(defparameter *dist* nil)
