

(in-package :report.geometry)
(annot:enable-annot-syntax)

(defclass 2dcurve-1p (2dshape)
  ((fn :type (function (*desired-type*) 2dvector)
       :initarg :fn
       :accessor fn-of)))

(defgeneric parametric-at (curve &rest params))

(defmethod parametric-at ((c 2dcurve-1p) &rest params)
  (apply (fn-of c) params))

