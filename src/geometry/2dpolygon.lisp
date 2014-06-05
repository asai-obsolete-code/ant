
(in-package :report.geometry)
(annot:enable-annot-syntax)


@export
@doc "an interface for VERTICES-OF."
(defclass 2dpolygon (2dshape)
  ())

(defmethod intersects-p ((po1 2dpolygon) (po2 2dpolygon))
  (some (lambda (v) (intersects-p v po1))
        (vertices-of po2)))

@export
(defgeneric separating-axes (shape))

@export
(defgeneric compute-ranges-for-separating-axes (axis-shape other-shape))
(defmethod compute-ranges-for-separating-axes ((obb-axis 2dpolygon) (obb-other 2dpolygon))
  (iter (for axis in (separating-axes obb-axis))
        (for projections = (mapcar (lambda (v) (dot-vector v axis))
                                   (vertices-of obb-other)))
        (collect
         (make-range
          (reduce #'max projections)
          (reduce #'min projections)))))

(define-permutation-methods intersects-p ((obb1 2dpolygon) (obb2 2dpolygon))
  (and (every #'intersects-p
              (compute-ranges-for-separating-axes obb1 obb2)
              (compute-ranges-for-separating-axes obb1 obb1))
       (every #'intersects-p
              (compute-ranges-for-separating-axes obb2 obb1)
              (compute-ranges-for-separating-axes obb2 obb2))))
