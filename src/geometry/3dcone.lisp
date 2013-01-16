

(in-package :report.geometry)
(annot:enable-annot-syntax)

;; 対角線上の二点をスロットに持つ

;; あんまり役に立たない気がする
;; obsolete

@export
(defclass 3dcone (3dsegment)
  ((from :type 3dvector :initarg :vertex :reader vertex-of)
   (tan :type *desired-type* :initarg :vertex-tangent :reader vertex-tangent-of)))

(defmethod initialize-instance ((c 3dcone) &key (angle 0 angle-supplied-p))
  (if (angle-supplied-p)
	  (setf (vertex-tangent-of c)
			(tan angle))))

(defmethod print-object ((v 3dsphere) stream)
  (print-unreadable-object (v stream :type t)
	(with-slots (r c) v
	  (format stream "RADIUS: ~A CENTER: ~A" r c))))

(defmethod dimension ((s 3dsphere))
  (with-slots (c r) s
	(let ((d (d* r 2.0d0)))
	  (3dv d d d))))

(defmethod congruent-p ((s1 3dsphere) (s2 3dsphere))
  (d= (radius s1) (radius s2)))

(defmethod boundary ((s 3dsphere))
  (with-slots (c r) s
	(let ((d (3dv r r r)))
	  (make-instance 'hexahedron
					 :bottom-left (sub c d)
					 :top-right (add c d)))))


