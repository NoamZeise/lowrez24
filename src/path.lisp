(in-package :coaster)

(defclass point ()
  ((pos :accessor pos :initarg :pos :type gficl:vec)
   (up :accessor up :initarg :up :type (gficl:vec))))

(defmethod print-object ((obj point) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "pos: ~a up: ~a" (pos obj) (up obj))))

(defun make-point (prev-point diff-vec)
  (let ((pos (gficl:+vec (pos prev-point) diff-vec))
	(up (gficl:normalise (gficl:cross (gficl:cross diff-vec (up prev-point)) diff-vec))))
    (make-instance 'point :pos pos :up up)))

(defun path-setup ()
  (setf *path-start* 0)
  (setf *path-end* 1)
  (setf *path-offset* 0)
  (let ((p (make-instance 'point :pos (gficl:make-vec'(0 0 0)) :up (gficl:make-vec '(0 1 0)))))
    (setf *path* (make-array +path-size+ :initial-element p))
    (add-point 1 0 (gficl:make-vec '(1 0 0)))
    (loop for i from 0 to +path-size+ do (update-path 0))))

(defun rn (val)
  (gficl:make-vec (list (- (random 1) 0.5) (cos (* val 0.1)) (sin (* val 0.1)))))

(defun path-fn (dir dist)
  (gficl:+vec dir (rn dist)))

(defun update-path (dist)
  (let* ((start-dist *path-offset*)
	 (end-pos (if (> *path-start* *path-end*)
		      (+ *path-end* +path-size+) *path-end*))
	 (end-dist (+ start-dist (- end-pos *path-start*)))
	 (mid (/ (+ start-dist end-dist) 2))
	 (dist-off (- mid dist))
	 (dist-from-start (- dist start-dist))
	 (dist-from-end (- end-dist dist)))
    ;;  (format t "start: ~a mid: ~a end: ~a~%" start-dist mid end-dist)
    ;;  (format t "off start: ~a mid: ~a end: ~a~%" dist-from-start dist-off dist-from-end)

    (if (or (< dist-from-start 20) (> dist-off 10))
	(add-point-to-start (path-fn (start-path-diff) dist)))
    
    (if (or (< dist-from-end 20) (< dist-off -10))
	(add-point-to-end (path-fn (end-path-diff) dist)))))

(defun get-pos (dist)
  (multiple-value-bind
   (n s) (floor (- dist *path-offset*))
   (if (or (>= (+ n 1) +path-size+) (< n 0)) (error "dist out of range"))   
   (let* ((p0 (aref *path* (path-index n)))
	  (p1 (aref *path* (path-index (+ n 1))))
	  (pos (gficl:+vec
		(gficl:*vec (- 1 s) (pos p0))
		(gficl:*vec s (pos p1))))
	 (up (gficl:+vec (gficl:*vec (- 1 s) (up p0))
			 (gficl:*vec s (up p1))))
	 (forward (gficl:-vec (pos p1) (pos p0))))
     (values pos up forward))))

(defun path-index (index)
  (mod (+ *path-start* index) +path-size+))

(defun add-point (current prev diff-vec)
  (let ((diff (gficl:*vec +diff-size+ (gficl:normalise diff-vec))))
    (setf (aref *path* current)
	  (make-point (aref *path* prev) diff))))

(defun start-path-diff ()
  (path-diff *path-start* (add-to-index *path-start*)))

(defun end-path-diff ()
  (path-diff *path-end* (remove-from-index *path-end*)))

(defun path-diff (current prev)
  (gficl:-vec (pos (aref *path* current))
	      (pos (aref *path* prev))))

(defun add-point-to-end (diff-vec)
  (let ((prev *path-end*))
    (add-to-end-index)
    (add-point *path-end* prev diff-vec)))

(defun add-point-to-start (diff-vec)
  (let ((prev *path-start*))
    (add-to-start-index)
    (add-point *path-start* prev diff-vec)))

(defun add-to-end-index ()
  (setf *path-end* (add-to-index *path-end*))
  (if (= *path-end* *path-start*) (remove-from-start)))

(defun add-to-start-index ()
  (setf *path-start* (remove-from-index *path-start*))
  (if (= *path-end* *path-start*) (remove-from-end))
  (setf *path-offset* (- *path-offset* 1)))

(defun remove-from-start ()
  (setf *path-start* (add-to-index *path-start*))
  (setf *path-offset* (+ *path-offset* 1)))

(defun remove-from-end ()
  (setf *path-end* (remove-from-index *path-end*)))

(defun add-to-index (index)
  (let ((i (+ index 1)))
    (if (>= i +path-size+) 0 i)))

(defun remove-from-index (index)
  (let ((i (- index 1)))
    (if (< i 0) (- +path-size+ 1) i)))

(defun print-path () 
  (loop for i from 0 below +path-size+ do
	(if t
	    (progn
	      (format t "~a" (aref *path* i))
	      (if (= i *path-start*) (format t " <- start"))
	      (if (= i *path-end*) (format t " <- end"))
	      (format t "~%")))))

;;; ---- Globals ----

(defconstant +diff-size+ 1.5)
(defconstant +path-size+ 100)
(defparameter *path* nil)
(defparameter *path-offset* nil)
(defparameter *path-start* nil)
(defparameter *path-end* nil)
