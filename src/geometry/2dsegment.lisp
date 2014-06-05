(in-package :report.geometry)
(annot:enable-annot-syntax)
(speed*)

@export
@export-accessors
(defclass 2dsegment (deep-copyable 
                     diameter-based-mixin
                     directionable
                     2dpolygon)
  ((from :type 2dvector :initarg :from :reader directional-from)
   (to   :type 2dvector :initarg :to :reader directional-to)))

(defmethod print-object ((v 2dsegment) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (from to) v
      (format stream "[~A,~A] [~A,~A]"
              (x-of from)
              (y-of from)
              (x-of to)
              (y-of to)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  functions  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun 2dsegment (fx fy tx ty)
  (make-instance '2dsegment
                 :from (2dv fx fy)
                 :to (2dv tx ty)))

@export
(defun 2dsegment-coerce (fx fy tx ty)
  (make-instance '2dsegment
                 :from (2dv-coerce fx fy)
                 :to (2dv-coerce tx ty)))

(alias 2dsegment* 2dsegment-coerce)

@export
(defun 2dv-segment (v1 v2)
  (make-instance '2dsegment
                 :from v1
                 :to v2))


@export
@doc "gives a signed value"
(defun signed-distance (seg p)
  @type 2dsegment seg
  @type 2dvector p
  (with-slots (from to) seg
    (dot (rotate90 (normalize (sub to from))) ;normal
         (sub p from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  methods  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod diameter ((seg 2dsegment))
  (with-slots (from to) seg
    (norm (sub to from))))

;; ;; this suits to 3dvector, but is not efficient in 2d
;; (defmethod distance ((seg 2dsegment) (p 2dvector))
;;   (with-slots (from to) seg
;; 	(let ((dir (normalize (sub to from))))
;; 	  (norm (sub p (dot dir (dot dir (sub p from))))))))

;; this suits to 3dvector, but is not efficient in 2d
(defmethod distance ((seg 2dsegment) (p 2dvector))
  (abs (signed-distance seg p)))

(defmethod distance ((p 2dvector) (seg 2dsegment))
  (distance seg p))

(defmethod dimension ((s 2dsegment))
  (with-slots (from to) s
    (sub from to)))

(defmethod center-of ((s 2dsegment))
  (with-slots (from to) s
    (nscale-vector (add from to) 0.5d0)))

(defmethod congruent-p ((l1 2dsegment) (l2 2dsegment))
  (congruent-p (dimension l1)
               (dimension l2)))

(defmethod parallel-p ((l1 2dsegment) (l2 2dsegment))
  (parallel-p (direction l1)
              (direction l2)))

(defmethod perpendicular-p ((l1 2dsegment) (l2 2dsegment))
  (perpendicular-p (direction l1)
                   (direction l2)))

(defmethod contains-p ((seg 2dsegment) (p 2dvector))
  (with-slots (from to) seg
    (let* ((dir (sub to from))
           (len (norm dir)))
      (and (< 0
              (dot-vector (normalize dir)
                          (sub p from))
              len)
           (zerop (distance seg p))))))

(define-permutation-methods intersects-p ((s 2dsegment) (p 2dvector))
  (contains-p s p))

(defmethod projection-of ((v 2dvector) (s 2dsegment))
  (projection-of v (direction s)))

(defmethod translate ((s 2dsegment) (v 2dvector))
  (with-slots (from to) s
    (2dv-segment (add from v) (add to v))))

(defmethod separating-axes ((s 2dsegment))
  (with-accessors ((dir direction)) s
    (list dir (rotate90 dir))))

(defmethod vertices-of ((s 2dsegment))
  (with-slots (from to) s
    (list from to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; minkowski- stuffs

(define-permutation-methods minkowski-sum ((v 2dvector) (s 2dsegment))
  (with-slots (from to) s
    (2dv-segment (add from v) (add to v))))

(define-permutation-methods minkowski-difference ((v 2dvector) (s 2dsegment))
  (with-slots (from to) s
    (2dv-segment (sub from v) (sub to v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; region stuffs

(define-permutation-methods region-sum ((v 2dvector) (s 2dsegment))
  (if (contains-p s v)
      s nil))
