

(in-package :report.geometry)
(annot:enable-annot-syntax)


@export
(defclass 3dshape () ())

(defmethod contains-p ((shape 3dshape) (v 3dvector))
  (zerop (funcall (implicit-function shape) v)))

