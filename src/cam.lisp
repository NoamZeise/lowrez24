(in-package :coaster)

(defclass camera ()
  ((rot-x :accessor rot-x :type number :initarg :rot-y)
   (rot-y :accessor rot-y :type number :initarg :rot-x)
   (radius :accessor radius :type number :initform 5)
   (target :accessor target :type gficl:vec :initarg :target)
   (view-matrix :accessor view-matrix :type gficl:matrix :initform (gficl:make-matrix))
   (pos :accessor pos :type gficl:vec :initarg :pos)
   (ideal-pos :accessor ideal-pos :type gficl:vec :initarg :ideal-pos)
   (up :accessor up :type gficl:vec :initarg :up)
   (ideal-up :accessor ideal-up :type gficl:vec :initarg :ideal-up)
   (forward :accessor forward :type gficl:vec :initarg :forward)
   (ideal-forward :accessor ideal-forward :type gficl:vec :initarg :ideal-forward)))

(defun make-camera (pos up forward)  
  (make-instance 'camera
		 :pos pos :ideal-pos pos
		 :up up :ideal-up up
		 :forward forward :ideal-forward forward
		 :rot-x 0 :rot-y 0))

(defun update-cam-ideal (cam pos up forward-dir)
  (let ((relative-pos (relative-cam-pos cam forward-dir up)))
    (setf (ideal-pos cam) (gficl:+vec relative-pos pos))
    (setf (ideal-forward cam) (gficl:+vec (gficl:*vec 2 forward-dir) (gficl:-vec relative-pos)))
    (setf (ideal-up cam) forward-dir)))

(defun cam-interp (dt current ideal)
  (let ((s (min (* dt 3.4) 1)))
    (gficl:+vec (gficl:*vec (- 1 s) current) (gficl:*vec s ideal))))

(defun update-cam-current (cam dt)
  (setf (pos cam) (cam-interp dt (pos cam) (ideal-pos cam)))
  (setf (up cam) (cam-interp dt (up cam) (ideal-up cam)))
  (setf (forward cam) (cam-interp dt (forward cam) (ideal-forward cam)))
  (setf (view-matrix cam) (gficl:view-matrix (pos cam) (forward cam) (up cam))))

(defun cam-controls (cam dt)  
  (gficl:map-keys-down
   (:a (setf (rot-x cam) (+ (rot-x cam) (* dt 1.0))))
   (:d (setf (rot-x cam) (+ (rot-x cam) (* dt -1 1.0))))
   (:space (setf (rot-y cam) (+ (rot-y cam) (* dt 1.0))))
   (:left-shift (setf (rot-y cam) (+ (rot-y cam) (* dt -1 1.0))))
   (:equal (setf (radius cam) (+ (radius cam) (* dt 1.0))))
   (:minus (setf (radius cam) (+ (radius cam) (* dt -1 1.0))))))

(defun update-cam (cam dt pos up forward-dir)
  (cam-controls cam dt)
  (setf (target cam) pos)
  (update-cam-ideal cam pos up forward-dir)
  (update-cam-current cam dt))

(defun relative-cam-pos (cam forward-vec up-vec)
  (let* ((forward (gficl:normalise forward-vec))
	 (up (gficl:normalise up-vec))
	 (left (gficl:cross forward up))
	 (qx (gficl:make-unit-quat (rot-x cam) forward))
	 (qy (gficl:make-unit-quat (rot-y cam) left))	 
	 (q (gficl:*quat qx qy)))
    (gficl:*vec (radius cam) (gficl:quat-conjugate-vec q up))))
