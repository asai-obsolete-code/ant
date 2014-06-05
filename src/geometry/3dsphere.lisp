

(in-package :report.geometry)
(annot:enable-annot-syntax)

;; 対角線上の二点をスロットに持つ

@export
(defclass 3dsphere (3dshape)
  (@required
   (c :type 3dvector :initarg :center :reader center-of)
   @required
   (r :type *desired-type* :initarg :radius :reader radius-of)))

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


