

(in-package :report.geometry)
(annot:enable-annot-syntax)

;; 対角線上の二点をスロットに持つ

@export
@export-accessors
@doc "a class that represent a hexahedron."
(defclass hexahedron (2drectangle 3dshape)
  ((v0 :type 3dvector :initarg v0)
   (v1 :type 3dvector :initarg v1)
   (dimension :type 3dvector)))

@export
(defun hexa-coerce (x0 y0 z0 x1 y1 z1)
  (make-instance 'hexahedron
                 :bottom-left (3dv-coerce x0 y0 z0)
                 :top-right (3dv-coerce x1 y1 z1)))

@export
(defun hexa (x0 y0 z0 x1 y1 z1)
  (make-instance 'hexahedron
                 :bottom-left (3dv x0 y0 z0)
                 :top-right (3dv x1 y1 z1)))


(defmethod print-object ((v hexahedron) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (v0 v1) v
      (format stream "[~A,~A,~A] [~A,~A,~A]"
              (x-of v0)
              (y-of v0)
              (z-of v0)
              (x-of v1)
              (y-of v1)
              (z-of v1)))))

;; @inherited dimension
;; @inherited boundary
;; @inherited center
;; @inherited x-range-of
;; @inherited z-range-of

(defmethod z-range-of ((r 2drectangle))
  (with-slots (v0 v1) r
    (make-range (z-of v0) (z-of v1))))

(defmethod intersects-p ((r1 hexahedron) (r2 hexahedron))
  (and (intersects-p (x-range-of r1) (x-range-of r2))
       (intersects-p (y-range-of r1) (y-range-of r2))
       (intersects-p (z-range-of r1) (z-range-of r2))))

;; @inherited top-left-of
;; @inherited bottom-right-of
;; @inherited congruent-p


(defmethod add ((r1 hexahedron) (r2 hexahedron))
  (with-slots ((v01 v0) (v11 v1)) r1
    (with-slots ((v02 v0) (v12 v1)) r2
      (hexa (dmin (x-of v01) (x-of v02))
            (dmin (y-of v01) (y-of v02))
            (dmin (z-of v01) (z-of v02))
            (dmax (x-of v11) (x-of v12))
            (dmax (y-of v11) (y-of v12))
            (dmax (z-of v11) (z-of v12))))))

(defmethod volume ((r hexahedron))
  (with-slots (v0 v1) r
    (d* (d- (x-of v1)
            (x-of v0))
        (d- (y-of v1)
            (y-of v0))
        (d- (z-of v1)
            (z-of v0)))))
