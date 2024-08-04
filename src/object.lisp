(in-package :lowres24)

(defclass game-object ()
  (mesh-data :accessor mesh-data :type gficl:vertex-data)
  (model :accessor model :type gficl:matrix)
  (normal :accessor normal :type gficl:matrix))
