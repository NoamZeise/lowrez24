(in-package :lowres24)

(defclass game-object ()
  ((meshes :initarg :meshes :type gficl:vertex-data)
   (shader :type gficl:shader :initarg :shader)
   (model :type gficl:matrix)
   (normal :type gficl:matrix)))

(defun make-go (shader meshes model-matrix)
  (let ((model (make-instance 'game-object :meshes meshes :shader shader)))
    (update-model model model-matrix)
    model))

(defun update-model (game-obj model-matrix)
  (setf (slot-value game-obj 'model) model-matrix)
  (setf (slot-value game-obj 'normal) (gficl:transpose-matrix (gficl:inverse-matrix model-matrix))))

(defgeneric draw (obj)
   (:documentation "Draw the object using the supplied shader."))

(defmethod draw ((obj game-object))
  (with-slots (meshes shader model normal) obj
    (gficl:bind-gl shader)
    (gficl:bind-matrix shader "model" model)
    (gficl:bind-matrix shader "normal_mat" normal)
    (if (listp meshes)
	(loop for mesh in meshes do
	      (gficl:draw-vertex-data mesh))
      (gficl:draw-vertex-data meshes))))
