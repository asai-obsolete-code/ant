(in-package :report.geometry)
(speed*)
(annot:enable-annot-syntax)

@export
@export-slots
(defclass 2dvector (deep-copyable)
  ((x :type *desired-type* :initarg :x)
   (y :type *desired-type* :initarg :y)))

(defmethod print-object ((v 2dvector) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "[~4f,~4f]"
            (x-of v) (y-of v))))

(declaim (inline x-of (setf x-of)
                 y-of (setf y-of)
                 2dv))

@export
(defun x-of (v)
  @type 2dvector v
  (dslot-value v 'x))
@export
(defun y-of (v)
  @type 2dvector v
  (dslot-value v 'y))
@export
(defun (setf x-of) (x v)
  @type number x
  @type 2dvector v
  (dsetf (slot-value v 'x) x))
@export
(defun (setf y-of) (y v)
  @type number y
  @type 2dvector v
  (dsetf (slot-value v 'y) y))

@export
(defun 2dv (x y)
  @type *desired-type* x y
  (make-instance '2dvector :x x :y y))

@export
(defun 2dv-coerce (x y)
  (make-instance '2dvector
                 :x (desired x)
                 :y (desired y)))

@export
(defun make-random-2dv (x0 y0 x1 y1)
  (2dv (drandom-between x0 x1)
       (drandom-between y0 y1)))

@export
(defun make-random-2dv-coerce (x0 y0 x1 y1)
  (2dv-coerce (random-between x0 x1)
              (random-between y0 y1)))


(alias 2dv* 2dv-coerce)
(alias make-random-2dv* make-random-2dv-coerce)

@export #'2dv*
@export #'make-random-2dv*


@export
(defvar +2origin+ (2dv 0.0d0 0.0d0))
@export
(defvar +2ex+ (2dv 1.0d0 0.0d0))
@export
(defvar +2ey+ (2dv 0.0d0 1.0d0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  functions  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@doc "the vector product of 2 vectors.
for 2dvector, it just returns the area of parallelogram
 made by those vectors."
(defun perp-dot (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (d- (d* (x-of v1)
          (y-of v2))
      (d* (x-of v2)
          (y-of v1))))

@export
@doc "create a new vector by rotating the original by pi/2 radian
 (counterclockwise)"
(defun rotate90 (v)
  @type 2dvector v
  (make-instance (class-of v)
                 :x (d- (y-of v))
                 :y (x-of v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; vector methods

@export
(defun add-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (make-instance '2dvector
                 :x (d+ (x-of v1) (x-of v2))
                 :y (d+ (y-of v1) (y-of v2))))

(defmethod add ((v1 2dvector) (v2 2dvector))
  (add-vector v1 v2))

@export
(defun sub-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (make-instance '2dvector
                 :x (d- (x-of v1) (x-of v2))
                 :y (d- (y-of v1) (y-of v2))))

(defmethod sub ((v1 2dvector) (v2 2dvector))
  (sub-vector v1 v2))

@export
(defun nadd-vector (v-modified v2)
  (with-slots (x y) v-modified
    (setf x (d+ x (x-of v2))
          y (d+ y (y-of v2)))
    v-modified))

@export
(defun nsub-vector (v-modified v2)
  @type 2dvector v-modified
  @type 2dvector v2
  (with-slots (x y) v-modified
    (setf x (d- x (x-of v2))
          y (d- y (y-of v2)))
    v-modified))

@export
(defun dot-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (d+ (d* (x-of v1) (x-of v2))
      (d* (y-of v1) (y-of v2))))

(defmethod dot ((v1 2dvector) (v2 2dvector))
  (dot-vector v1 v2))

@export
(defun norm2-vector (v1)
  @type 2dvector v1
  (d+ (d^2 (x-of v1))
      (d^2 (y-of v1))))

(defmethod norm2 ((v1 2dvector))
  (norm2-vector v1))

(defmethod normalize ((v1 2dvector))
  (scale-vector v1 (d/ 1.0d0 (norm v1))))


(defmethod resize ((v1 2dvector) (length double-float))
  (scale-vector v1 (d/ length (norm v1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; shape methods

@export
(defun scale-vector (v1 c)
  @type 2dvector v1
  @type *desired-type* c
  (make-instance '2dvector
                 :x (d* c (x-of v1))
                 :y (d* c (y-of v1))))

@export
(defun nscale-vector (v1 c)
  @type 2dvector v1
  @type *desired-type* c
  (with-slots (x y) v1
    (setf x (d* c x)
          y (d* c y))
    v1))

(defmethod scale ((v1 2dvector) (c double-float))
  (scale-vector v1 c))

(defmethod translate ((v1 2dvector) (v2 2dvector))
  (add v1 v2))

(defmethod distance ((v1 2dvector) (v2 2dvector))
  (norm (sub-vector v1 v2)))

(defmethod parallel-p ((v1 2dvector) (v2 2dvector))
  (zerop (perp-dot v1 v2)))

(defmethod perpendicular-p ((v1 2dvector) (v2 2dvector))
  (zerop (dot-vector v1 v2)))

(defmethod congruent-p ((v1 2dvector) (v2 2dvector))
  (and (d=~ (x-of v1) (x-of v2))
       (d=~ (y-of v1) (y-of v2))))

(defmethod ->list ((v 2dvector))
  (list (x-of v) (y-of v)))

(defmethod dimension ((v 2dvector))
  (make-instance (class-of v) :x 0.0d0 :y 0.0d0))

(defmethod boundary ((v 2dvector))
  (make-instance '2drectangle :bottom-left v :top-right v))

(defmethod angle ((v 2dvector))
  (with-slots (x y) v
    (datan y x)))

(defmethod slope ((v 2dvector))
  (with-slots (x y) v
    (d/ y x)))

(defmethod center-of ((v 2dvector)) v)


(defmethod projection-of ((v1 2dvector) (v2 2dvector))
  (scale-vector v2 (d/ (dot-vector v1 v2) (norm2-vector v2))))

(defmethod radius ((v 2dvector))
  0.0d0)
