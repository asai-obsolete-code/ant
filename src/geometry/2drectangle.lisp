(declaim (optimize (speed 3)
                   (safety 0)
                   (debug 0)
                   (space 0)))

(in-package :report.geometry)
(annot:enable-annot-syntax)

;; 対角線上の二点をスロットに持つ

@export
@export-accessors
@doc "a class that represent a rectangle."
(defclass 2drectangle (copyable diameter-based-mixin 2dpolygon)
  ((v0 :type 2dvector :initarg :bottom-left :accessor bottom-left-of)
   (v1 :type 2dvector :initarg :top-right :accessor top-right-of)
   (dimension :type 2dvector)))

@export
(defun rect-coerce (x0 y0 x1 y1)
  (make-instance '2drectangle
                 :bottom-left (2dv-coerce x0 y0)
                 :top-right (2dv-coerce x1 y1)))

@export
(defun rect (x0 y0 x1 y1)
  (make-instance '2drectangle
                 :bottom-left (2dv x0 y0)
                 :top-right (2dv x1 y1)))

@export
(defun dimension-rect (v)
  (make-instance '2drectangle
                 :bottom-left (2dv 0.0d0 0.0d0)
                 :top-right v))

@export
(defun 2dv-rect (v1 v2)
  (make-instance '2drectangle
                 :bottom-left v1
                 :top-right v2))

(defmethod print-object ((v 2drectangle) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (v0 v1) v
      (format stream "[~A,~A] [~A,~A]"
              (x-of v0)
              (y-of v0)
              (x-of v1)
              (y-of v1)))))

(defmethod dimension ((h 2drectangle))
  (with-memoising-slot (dimension h)
    (with-slots (v0 v1) h
      (sub-vector v1 v0))))

(defmethod diameter ((h 2drectangle))
  (length (dimension h)))

(defmethod boundary ((h 2drectangle))
  h)

(defmethod center-of ((h 2drectangle))
  (with-slots (v0 v1) h
    (nscale-vector (add v1 v0) 0.5)))

(defmethod x-range-of ((r 2drectangle))
  (with-slots (v0 v1) r
    (make-range (x-of v0) (x-of v1))))
(defmethod y-range-of ((r 2drectangle))
  (with-slots (v0 v1) r
    (make-range (y-of v0) (y-of v1))))

(defmethod intersects-p ((r1 2drectangle) (r2 2drectangle))
  (and (intersects-p (x-range-of r1) (x-range-of r2))
       (intersects-p (y-range-of r1) (y-range-of r2))))

(defmethod congruent-p ((r1 2drectangle) (r2 2drectangle))
  (congruent-p (dimension r1)
               (dimension r2)))

(defmethod contains-p ((r 2drectangle) (v 2dvector))
  (with-slots (v0 v1) r
    (and (< (x-of v0) (x-of v) (x-of v1))
         (< (y-of v0) (y-of v) (y-of v1)))))

(defmethod top-left-of ((r 2drectangle))
  (with-slots (v0 v1) r
    (2dv (x-of v0) (y-of v1))))

(defmethod bottom-right-of ((r 2drectangle))
  (with-slots (v0 v1) r
    (2dv (x-of v1) (y-of v0))))

(defmethod region-sum ((r1 2drectangle) (r2 2drectangle))
  (with-slots ((v01 v0) (v11 v1)) r1
    (with-slots ((v02 v0) (v12 v1)) r2
      (rect (dmin (x-of v01) (x-of v02))
            (dmin (y-of v01) (y-of v02))
            (dmax (x-of v11) (x-of v12))
            (dmax (y-of v11) (y-of v12))))))

(defmethod region-difference ((r1 2drectangle) (r2 2drectangle))
  (with-slots ((v01 v0) (v11 v1)) r1
    (with-slots ((v02 v0) (v12 v1)) r2
      (rect (dmax (x-of v01) (x-of v02))
            (dmax (y-of v01) (y-of v02))
            (dmin (x-of v11) (x-of v12))
            (dmin (y-of v11) (y-of v12))))))

(defmethod translate ((r 2drectangle) (v 2dvector))
  (with-slots (v0 v1) r
    (2dv-rect (add v0 v)
              (add v1 v))))

(defmethod scale ((r 2drectangle) (n double-float))
  (with-slots (v0 v1) r
    (2dv-rect (scale-vector v0 n)
              (scale-vector v1 n))))

(define-permutation-methods minkowski-sum ((v 2dvector) (r 2drectangle))
  (translate r v))

;; (define-permutation-methods minkowski-difference ((v 2dvector) (r 2drectangle)))

(defmethod volume ((r 2drectangle))
  (with-slots (v0 v1) r
    (d* (d- (x-of v1)
            (x-of v0))
        (d- (y-of v1)
            (y-of v0)))))

(defmethod distance ((r 2drectangle) (v 2dvector))
  (distance (center-of r) v))

(defmethod minimum-distance ((r 2drectangle) (v 2dvector))
  (with-slots (v0 v1) r
    (with-slots (x y) v
      (cond
        ((d< x (x-of v0))
         (cond
           ((d< y (y-of v0))            (distance v v0))
           ((d< (y-of v0) y (y-of v1))  (d- (x-of v0) x))
           (t                           (distance v (top-left-of r)))))
        ((d< (x-of v0) x (x-of v1))
         (cond
           ((d< y (y-of v0))            (d- (y-of v0) y))
           ((d< (y-of v0) y (y-of v1))  0.0d0)
           (t                           (d- y (y-of v1)))))
        (t
         (cond
           ((d< y (y-of v0))            (distance v v1))
           ((d< (y-of v0) y (y-of v1))  (d- x (x-of v1)))
           (t                           (distance v (bottom-right-of r)))))))))


(defmethod vertices-of ((r 2drectangle))
  (list (top-right-of r)
        (top-left-of r)
        (bottom-left-of r)
        (bottom-right-of r)))

;; (define-permutation-methods add ((r1 2drectangle) (r2 2drectangle) )
;;   (2dv-rect 


