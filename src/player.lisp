(in-package :coaster)

(defparameter *player* nil)

(defclass player ()
  ((position :type number :initform 0 :accessor pos)
   (velocity :type number :initform 0 :accessor velocity)))

(defun player-setup ()
  (setf *player* (make-instance 'player)))

(defun update-player (dt)
  (let ((acceleration (* -1 (signum (velocity *player*)))))
      (gficl:map-keys-down
       (:w (setf acceleration 1))
       (:s (setf acceleration -1)))
      (setf (velocity *player*)
	    (alexandria:clamp (+ (velocity *player*) (* acceleration dt))
			      -3 3))
      (setf (pos *player*)
	    (+ (pos *player*) (* (velocity *player*) dt 10.0)))))

(defun player-pos ()
  (pos *player*))
