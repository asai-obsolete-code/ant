

(in-package :report.geometry)
(annot:enable-annot-syntax)
(speed*)
;; 対角線上の二点をスロットに持つ

@export
(defclass 2dcircle (deep-copyable radius-based-mixin 2dshape)
  ((c :type 2dvector :initarg :center :reader center-of)
   (r :type *desired-type* :initarg :radius :reader radius)))

@export
(defun circle (center radius)
  @type 2dvector center
  @type *desired-type* radius
  (new '2dcircle :center center :radius radius))

@export
(defun circle-coerce (center radius)
  @type 2dvector center
  (new '2dcircle :center center :radius (desired radius)))

(alias circle* circle-coerce)

@export #'circle*

(defmethod print-object ((v 2dcircle) stream)
  (print-unreadable-object (v stream :type t)
	(with-slots (r c) v
	  (format stream "RADIUS: ~A CENTER: ~A" r c))))

(defmethod dimension ((s 2dcircle))
  (with-slots (c r) s
	(let ((d (d* r 2.0d0)))
	  (2dv d d))))

(defmethod congruent-p ((s1 2dcircle) (s2 2dcircle))
  (d= (radius s1) (radius s2)))

(defmethod boundary ((s 2dcircle))
  (with-slots (c r) s
	(let ((d (2dv r r)))
	  (make-instance '2drectangle
					 :bottom-left (sub c d)
					 :top-right (add c d)))))

(defmethod distance ((circle 2dcircle) (v 2dvector))
  (with-slots (c) circle
	(distance c v)))

(defmethod minimum-distance ((circle 2dcircle) (v 2dvector))
  (with-slots (c r) circle
	(d- (distance c v) r)))

(defmethod contains-p ((circle 2dcircle) (v 2dvector))
  (minusp (minimum-distance circle v)))

(defmethod intersects-p ((ci1 2dcircle) (ci2 2dcircle))
  (with-slots ((c1 c) (r1 r)) ci1
	(with-slots ((c2 c) (r2 r)) ci2
	  (d< (distance c1 c2) (d+ r1 r2)))))

(define-permutation-methods intersects-p ((circle 2dcircle) (po 2dpolygon))
  (with-slots (c r) circle
	(let ((axes (separating-axes po)))
	  (every (lambda (axis)
			   (let* ((axis (normalize axis))
					  (projections (mapcar (lambda (v) (dot-vector v axis)) (vertices-of po))))
				 (intersects-p (make-range (reduce #'max projections)
										   (reduce #'min projections))
							   (let ((projection (dot-vector c axis)))
								 (make-range (d+ projection r)
											 (d- projection r))))))
			 axes))))

(defmethod translate ((circle 2dcircle) (v 2dvector))
  (deep-copy circle :center (add (center-of circle) v)))
(defmethod scale ((circle 2dcircle) (n double-float))
  (deep-copy circle :radius (d* (radius circle) n)))