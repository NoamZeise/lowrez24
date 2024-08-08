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
    (let ((pos-updated nil))      
      (gficl:map-keys-down
       ((:w (setf *cam-r* (- *cam-r* (* 10 dt))))
	(:s (setf *cam-r* (+ *cam-r* (* 10 dt))))
	(:a (setf *cam-x* (+ *cam-x* dt)))
	(:d (setf *cam-x* (- *cam-x* dt)))
	(:q (setf *cam-y* (+ *cam-y* dt)))
	(:e (setf *cam-y* (- *cam-y* dt)))
	
	(:space
	 (setf *dist* (+ *dist* (* 10.0 dt))))
	(:left-shift
	 (setf *dist* (- *dist* (* 10.0 dt))))
	
	(:up (setf pos-updated t)
	     (setf *pos* (gficl:+vec *pos* (list 0 (* dt 10.0) 0))))
	(:down (setf pos-updated t)
	       (setf *pos* (gficl:+vec *pos* (list 0 (* dt -10.0) 0))))))
      (if pos-updated
	  (update-model *model2* (gficl:*mat (gficl:translation-matrix *pos*)))))

    (update-path *dist*)
    
    (multiple-value-bind
     (pos up forward) (get-pos *dist*)
     (setf *cam-target* pos)
     (setf *cam-up* forward)
     (setf *cam-pos* (calc-cam-pos forward up)))
    
;;    (format t "fps: ~a~%" (/ 1 (float dt)))
  ;;  (format t "dist: ~a~%" *dist*)
    
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
    (draw-points))
   (draw-post (gficl:framebuffer-texture-id
	       (if *offscreen-resolve-fb* *offscreen-resolve-fb* *offscreen-fb*) 0))))

(defun resize (w h)
  (resize-post *target-width* *target-height* w h))

(defun setup ()
  (setup-offscreen)
  (setup-post)
  (setup-assets)
  (setf *cam-target* (gficl:make-vec '(0 0 0)))
  (setf *cam-up* (gficl:make-vec'(1 0 0)))
  (setf *cam-pos* (gficl:make-vec'(0 1 0)))
  (setf *local-up* *cam-up*)
  (setf *local-left* (gficl:make-vec '(1 0 0)))

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
  (setf *cam-x* 0)
  (setf *cam-y* 0)
  (setf *cam-r* 5)
  
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
	  (update-model *model* (gficl:*mat 
					    (gficl:translation-matrix (gficl:+vec (pos p) (up p)))
					    (gficl:scale-matrix '(0.1 0.1 0.1))))
	  (draw *model*))))

(defun calc-cam-pos (forward-vec up-vec)
  (let* ((forward (gficl:normalise forward-vec))
	 (up (gficl:normalise up-vec))
	 (left (gficl:cross forward up))
	 (qx (gficl:make-unit-quat *cam-x* left))
	 (qy (gficl:make-unit-quat *cam-y* forward))
	 (q (gficl:*quat qy qx)))
    (gficl:*vec *cam-r* (gficl:quat-conjugate-vec q up))))

;;; ---- Globals ----

(defparameter *cam-up* (gficl:make-vec '(0 1 0)))
(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)

(defparameter *cam-x* nil)
(defparameter *cam-y* nil)
(defparameter *cam-r* nil)

(defparameter *local-up* nil)
(defparameter *local-left* nil)

(defparameter *model* nil)
(defparameter *model2* nil)

(defparameter *pos* nil)

(defparameter *dist* nil)
