(in-package :coaster)

(defun setup-assets ()
  (setf *assets* (make-hash-table)))

(defun cleanup-assets ()
  (loop for vd being the hash-value of *assets* do
	(if (listp vd)
	    (loop for v in vd do (gficl:delete-gl v))
	    (gficl:delete-gl vd))))

(defun add-asset (key vertex-data)
  (setf (gethash key *assets*) vertex-data))

(defun load-model (key filename)
  (add-asset key (gficl/load:model filename :vertex-form '(:position :normal))))

(defun get-asset (key)
  (gethash key *assets*))

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")
