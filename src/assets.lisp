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
  (let ((meshes (obj:extract-meshes (obj:parse (probe-file filename)))))
    (add-asset
     key (loop for mesh in meshes collecting
	       (gficl:make-vertex-data-from-vectors
		(get-vertex-form mesh) (obj:vertex-data mesh) (obj:index-data mesh))))))

(defun get-asset (key)
  (gethash key *assets*))

;;; ---- Helpers ----

(defun get-vertex-form (mesh)
  (gficl:make-vertex-form
   (loop for a in (obj:attributes mesh) collecting
	 (ecase a (:position (gficl:make-vertex-slot 3 :float))
		(:normal (gficl:make-vertex-slot 3 :float))
		(:uv (gficl:make-vertex-slot 2 :float))))))		

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")
