
(in-package :report.geometry)
(annot:enable-annot-syntax)

@export
(defclass 3dsegment (2dsegment)
  ((from :type 3dvector
		 :initarg :from
		 :reader from
		 :reader from-of)
   (to   :type 3dvector
		 :initarg :to
		 :reader to
		 :reader to-of)))

(defmethod print-object ((v 3dsegment) stream)
  (print-unreadable-object (v stream :type t)
	(with-slots (from to) v
	  (format stream "[~A,~A,~A] [~A,~A,~A]"
			  (x-of from)
			  (y-of from)
			  (z-of from)
			  (x-of to)
			  (y-of to)
			  (z-of to)))))

@export
(defun 3dsegment (fx fy fz tx ty tz)
  (make-instance '3dsegment
				 :from (3dv fx fy fz)
				 :to (3dv tx ty tz)))


@export
(defun 3dsegment-coerce (fx fy fz tx ty tz)
  (make-instance '3dsegment
				 :from (3dv-coerce fx fy fz)
				 :to (3dv-coerce tx ty tz)))

;; different from 2dvector
(defmethod distance ((seg 3dsegment) (p 3dvector))
  (with-slots (from to) seg
	(let ((dir (sub to from)))
	  (norm (sub p (dot dir (dot dir (sub p from))))))))

