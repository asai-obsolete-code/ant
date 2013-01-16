

(in-package :cl-user)
(annot:enable-annot-syntax)

(defpackage ant
  (:use :cl
		:anaphora
		:iterate
		:alexandria
		:report.utilities
		:annot.eval-when))
(in-package :ant)


(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *ant-max-food* 5)
(defvar *field* (make-array (list *width* *height*)))

(defstruct field food pheromon)

(defun iter-array-row-major (array fn)
  (iter (for i below (array-total-size array))
		(funcall fn i)))

@eval-always
(defmacro with-iter-array-row-major ((i array) &body body)
  `(iter-array-row-major ,array (lambda (,i) ,@body)))


(defun set-array-row-major (array fn)
  (with-iter-array-row-major (i array)
	(setf (row-major-aref array i)
		  (funcall fn i))))

(defun setup-field (rate)
  (set-array-row-major
   *field*
   (lambda (i)
	 @ignore i
	 (make-field
	  :pheromon 0
	  :food (bias-if rate (random 100) 0)))))

(defstruct ant x y food)

(defparameter *colony-x* (random *width*))
(defparameter *colony-y* (random *height*))

(defparameter *ants* nil)

(defun setup-ants (n)
  (setf *ants* nil)
  (iter (repeat n)
		(push (make-ant :x *colony-x* :y *colony-y*)
			  *ants*)))

(defun step-ants ()
  (mapc #'step-ant *ants*))

(defun current-field (ant)
  (with-slots (x y) ant
	(aref *field* x y)))

(defun find-food (ant)
  (field-food (current-field ant)))

(defun food-found-p (ant)
  (plusp (find-food ant)))

(defun collect-food (ant)
  (when (food-found-p ant)
	(setf (field-food (current-field ant))
		  (max (- (field-food (current-field ant))
				  *ant-max-food*)
			   0))))

(defun have-food-p (ant)
  (plusp (ant-food ant)))

(defun step-ant (ant)
  (if (have-food ant)
	  (back-home ant)
	  (search-food ant)))



;; (defun search (ant)
;;   (with-slots (x y) ant
;; 	(map-permutation
;; 	 (list -1 0 1)
;; 	 (lambda (dx dy)
;; 	   (unless (= dx dy 0)
		 
	   

;; (defun go-back (ant)
;;   )
  