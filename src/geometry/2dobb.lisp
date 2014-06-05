
(in-package :report.geometry)
(annot:enable-annot-syntax)
(speed*)
@export
@export-accessors
(defclass 2dobb (deep-copyable
                 radius-based-mixin
                 recursive-print-mixin
                 2dpolygon)
  ((c
    :type 2dvector
    :accessor center-of
    :initform (2dv 0.0d0 0.0d0)
    :initarg :c
    :initarg :center)
   (x/2 :type *desired-type* :accessor x/2 :initarg :x/2)
   (y/2 :type *desired-type* :accessor y/2 :initarg :y/2)
   (angle
    :type *desired-type*
    :accessor angle
    :initarg :angle
    :initform 0.0d0)
   (matrix :type 2dmatrix)))

(defmethod shallow-copy ((o 2dobb) &rest args &key &allow-other-keys)
  @ignore args
  (let ((new (allocate-instance (find-class '2dobb))))
    (with-slots ((c1 c) (x1 x/2) (y1 y/2) (angl1e angle) (matrix1 matrix)) o
      (with-slots ((c2 c) (x2 x/2) (y2 y/2) (angle2 angle) (matrix2 matrix)) new
        (setf c2 c1
              x2 x1
              y2 y1
              angle2 angl1e
              matrix2 matrix1)
        new))))

(defmethod radius ((o 2dobb))
  (with-slots (x/2 y/2) o
    (dsqrt (d+ (d^2 x/2) (d^2 y/2)))))

(defmethod initialize-instance :after ((obb 2dobb)
                                       &key
                                         dimension
                                         &allow-other-keys)
  @type (or null 2dvector) dimension
  (when dimension
    (with-slots (x/2 y/2) obb
      (setf x/2 (d* (x-of dimension) 0.5d0)
            y/2 (d* (y-of dimension) 0.5d0))))
  (with-slots (matrix angle) obb
    (setf matrix (rotation-matrix angle))))

(defmethod (setf angle) :after ((angle double-float) (obb 2dobb))
  (with-slots (matrix) obb
    (setf matrix (rotation-matrix angle))))

(defmethod (setf dimension) ((dim 2dvector) (obb 2dobb))
  (with-slots (x/2 y/2) obb
    (setf x/2 (d* (x-of dim) 0.5d0)
          y/2 (d* (y-of dim) 0.5d0))))

(defmethod dimension ((obb 2dobb))
  (with-slots (x/2 y/2) obb
    (2dv (d* x/2 2.0d0) (d* y/2 2.0d0))))

(defmethod update-instance-for-different-class :after
    ((r 2drectangle) (obb 2dobb) &rest args)
  @ignore args
  (with-accessors ((c center-of)
                   (dim dimension)) r
    (with-slots (x/2 y/2) obb
      (setf (center-of obb) c
            x/2 (d/ (x-of dim) 2.0d0)
            y/2 (d/ (y-of dim) 2.0d0)))))

(defmethod rotate ((obb 2dobb) (rad double-float))
  (with-slots (angle) obb
    (deep-copy obb :angle (d+ angle rad))))

(defmethod translate ((obb 2dobb) (v 2dvector))
  (with-slots (c) obb
    (shallow-copy obb :c (add-vector c v))))

(defmethod contains-p ((obb 2dobb) (v 2dvector))
  (with-slots (c angle x/2 y/2) obb
    (let ((relative (rotate (sub-vector v c) (d- angle))))
      (and (d< (dabs (x-of relative)) x/2)
           (d< (dabs (y-of relative)) y/2)))))

(define-permutation-methods intersects-p ((obb 2dobb) (v 2dvector))
  (contains-p obb v))

(defmethod separating-axes ((obb 2dobb))
  (with-accessors ((angle angle)) obb
    (let ((s (dsin angle))
          (c (dcos angle)))
      (list (2dv c s)
            (2dv (d- s) c)))))

(defmethod vertices-of ((obb 2dobb))
  (with-slots (c x/2 y/2 matrix) obb
    (let* ((r+ (rotate (2dv x/2 y/2) matrix))
           (r- (2dv (d- (x-of r+)) (y-of r+))))
      (list (add-vector c r+) (add-vector c r-)
            (sub-vector c r+) (sub-vector c r-))))) ;x/2,y/2正のとき半時計回り
