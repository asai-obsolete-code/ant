(in-package :report.geometry)
(annot:enable-annot-syntax)

(speed*)

;; @export
;; (deftype range ()
;;   `(simple-array *desired-type* 2))
;; (defun range (from to)
;;   (let ((r (make-array 2 :element-type '*desired-type*)))
;; 	(setf (aref range 0) (coerce (dmin from to) '*desired-type*)
;; 		  (aref range 1) (coerce (dmax from to) '*desired-type*))
;; 	r))
;; (defun lbound-of (range)
;;   (aref range 0))
;; (defun ubound-of (range)
;;   (aref range 1))

@export
@export-slots
(defclass range (diameter-based-mixin)
  ((from :type *desired-type* :initarg :from :reader range-from)
   (to :type *desired-type* :initarg :to :reader range-to)))

(defmethod print-object ((v range) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (from to) v
      (format stream "[~A ~A]" from to))))

@export
(defun make-range (from to)
  @type *desired-type* from to
  (make-instance 'range
                 :from (dmin from to)
                 :to (dmax from to)))

@export
(defun make-range-coerce (from to)
  (make-range (desired from)
              (desired to)))

(defmethod diameter ((rng range))
  (with-slots (from to) rng
    (d- to from)))

(defmethod center-of ((rng range))
  (with-slots (from to) rng
    (d* (d+ to from) 0.5d0)))

(defmethod contains-p ((rng range) (n number))
  (with-slots (from to) rng
    (cond ((d< from n to) :in)
          ((d= from n) :min)
          ((d= n to) :max)
          (t nil))))


(defmethod congruent-p ((r1 range) (r2 range))
  (and (d=~ (range-from r1) (range-from r2))
       (d=~ (range-to r1) (range-to r2))))

(define-permutation-methods congruent-p ((r1 range) (r2 null))
  nil)

(declaim (inline %intersects-p))
(defun %intersects-p (r1 r2)
  @type range r1 r2
  (or (and (d< (range-from r1) (range-to r2))
           (d<= (range-to r2) (range-to r1)))
      (and (d<= (range-from r1) (range-from r2))
           (d< (range-from r2) (range-to r1)))))

(defmethod intersects-p ((r1 range) (r2 range))
  (or (%intersects-p r1 r2)
      (%intersects-p r2 r1)))


(define-permutation-methods intersects-p
    ((r1 range) (r2 (eql nil)))
  nil)

(defmethod region-product ((r1 range) (r2 range))
  (if (intersects-p r1 r2)
      (make-range (max (range-from r1)
                       (range-from r2))
                  (min (range-to r1)
                       (range-to r2)))
      nil))

(defmethod region-sum ((r1 range) (r2 range))
  (make-range (min (range-from r1)
                   (range-from r2))
              (max (range-to r1)
                   (range-to r2))))

(define-permutation-methods region-sum
    ((r1 range) (r2 (eql nil)))
  r1)

(define-permutation-methods region-product
    ((r1 range) (r2 (eql nil)))
  nil)
