

(in-package :cl-user)
(annot:enable-annot-syntax)

(defpackage ant
  (:use :cl
		:anaphora
		:iterate
		:alexandria
		:report.utilities
		:report.geometry
		:gtk
		:gdk
		:gobject
		:annot.eval-when)
  (:shadow :scale :rotate :range))
(in-package :ant)

(defparameter *width* 100)
(defparameter *height* 100)
(defvar *field* (make-array (list *width* *height*)))
(defparameter *colony-x* (random *width*))
(defparameter *colony-y* (random *height*))
(defparameter *stored-food* 0)
(defparameter *ants* nil)
(defparameter *ant-max-food* 5)
(defparameter *field-max-food* 20)
(defparameter *field-max-pheromon* 100)
(defparameter *initial-pheromon* 20)
(defparameter *pheromon-evaporation-rate* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utility

@eval-always
(defmacro with-iter-array-row-major ((&optional instance i) array
									 &body body)
  (once-only (array)
	(unless i (setf i (gensym "I")))
	(unless instance (setf instance (gensym "INSTANCE")))
	`(iter (for ,i below (array-total-size ,array))
		   (declare (ignorable ,i))
		   (symbol-macrolet ((,instance (row-major-aref ,array ,i)))
			 ,@body))))

(defun form-iteration (subscript array axis-number body)
  `(iter (for ,subscript below (array-dimension ,array ,axis-number))
		 ,body))

(defun form-iter-array (subscripts array body)
  (iter (for subscript in (reverse subscripts))
		(for axis-number downfrom (1- (length subscripts)))
		(for inner-body previous formed-body initially body)
		(for formed-body = 
			 (form-iteration subscript array axis-number inner-body))
		(finally (return formed-body))))

@eval-always
(defmacro with-iter-array ((instance &rest subscripts)
						   array &body body)
  (once-only (array)
	(unless instance (setf instance (gensym "INSTANCE")))
	(form-iter-array
	 subscripts
	 array
	 `(symbol-macrolet ((,instance (aref ,array ,@subscripts)))
		,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance/dot functions

(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun euclid-distance2 (x1 y1 x2 y2)
  (+ (^2 (- x1 x2)) (^2 (- y1 y2))))

(defun euclid-distance (x1 y1 x2 y2)
  (sqrt (euclid-distance2 x1 y1 x2 y2)))

(defun euclid-distance2-ants (a1 a2)
  (with-slots ((x1 x) (y1 y)) a1
	(with-slots ((x2 x) (y2 y)) a2
	  (euclid-distance2 x1 y1 x2 y2))))

(defun euclid-distance-ants (a1 a2)
  (sqrt (euclid-distance2-ants a1 a2)))

(defun easy-dot (x1 y1 x2 y2)
  (+ (* x1 x2) (* y1 y2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes

(defstruct field food pheromon)
(defun food-at (x y)
  (field-food (aref *field* x y)))
(defun (setf food-at) (value x y)
  (setf (field-food (aref *field* x y)) value))
(defun pheromon-at (x y)
  (field-pheromon (aref *field* x y)))
(defun (setf pheromon-at) (value x y)
  (setf (field-pheromon (aref *field* x y)) value))

(defun field-valid-p (x y)
  @ignore x y
  ;; (array-in-bounds-p *field* x y)
  t
  )

(defun field-not-valid-p (x y)
  (not (field-valid-p x y)))


(defstruct ant x y vx vy food)
(defun at-colony-p (ant)
  (with-slots (x y) ant
	(> 4 (euclid-distance x y *colony-x* *colony-y*))))

(defun ant-field (ant)
  (with-slots (x y) ant
	(aref *field* x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; setup

(defun setup ()
  (setup-field 0.1d0)
  (setup-ants 10))

(defun setup-field (rate)
  (setf *colony-x* (random *width*))
  (setf *colony-y* (random *height*))
  (with-iter-array-row-major (field) *field*
	(setf field (make-field
				 :pheromon 0
				 :food (bias-if rate
								(random *field-max-food*)
								0)))))

(defun setup-ants (n)
  (setf *ants* nil)
  (iter (repeat n)
		(push (make-ant :x *colony-x*
						:y *colony-y*
						:vx 0
						:vy 0
						:food 0)
			  *ants*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; global stepping

(defun stepping ()
  (step-ants)
  (step-fields))

(defun step-ants ()
  (mapc #'step-ant *ants*))

(defun step-fields ()
  (with-iter-array-row-major (f) *field*
	(when (plusp (field-pheromon f))
	  (decf (field-pheromon f) *pheromon-evaporation-rate*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant stepping

(defun step-ant (ant)
  (if (have-food-p ant)
	  (back-home ant)
	  (search-food ant)))

(defun have-food-p (ant)
  (plusp (ant-food ant)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant searching food 

(defun find-food (ant)
  (field-food (ant-field ant)))

(defun food-found-p (ant)
  (plusp (find-food ant)))

(defun collect-food (ant)
  (decf (field-food (ant-field ant)) *ant-max-food*)
  (setf (ant-food ant) *ant-max-food*)
  (when (minusp (field-food (ant-field ant)))
	(setf (field-food (ant-field ant)) 0)))

(defun search-food (ant)
  (if (food-found-p ant)
	  (collect-food ant)
	  (with-slots (x y vx vy) ant
		(destructuring-bind (nx ny)
			(or (position-with-highest-pheromon-around ant)
				(position-most-away-from-other-ants ant)
				(random-elt (valid-next-positions ant)))
		  (setf vx (- nx x) vy (- ny y) x nx y ny)))))


(defun valid-next-positions (ant)
  (with-slots (x y vx vy) ant
	(iter
	  (for dx from -1 to 1)
	  (when (= dx (- vx))
		(next-iteration))
	  (appending
	   (iter
		 (for dy from -1 to 1)
		 (when (or (= dx dy 0)
				   (= dy (- vy)))
		   (next-iteration))
		 (collect (list (mod (+ x dx) *width*)
						(mod (+ y dy) *height*))))))))

(defun position-with-highest-pheromon-around (ant)
  (iter (for lst in (valid-next-positions ant))
		(for p = (apply #'pheromon-at lst))
		(when (< 0 p)
		  (finding lst maximizing p))))

(defun position-most-away-from-other-ants (ant)
  (iter (for lst in (valid-next-positions ant))
		(for (x y) = lst)
		(finding
		 lst
		 maximizing
		 (iter
		   (for another in (remove ant *ants*))
		   (minimizing
			(euclid-distance
			 x y (ant-x another) (ant-y another)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant going back home

(defun no-pheromon-in-front (ant)
  (iter (for (x y) in (valid-next-positions ant))
		(always (= (pheromon-at x y) 0))))

(defun store-into-colony (ant)
  (incf *stored-food* (ant-food ant))
  (setf (ant-food ant) 0))

(defun distance-from-colony (dlst)
  (manhattan-distance
   (first dlst) (second dlst)
   *colony-x* *colony-y*))

(defun position-nearest-to-colony (ant)
  (iter (for lst in (valid-next-positions ant))
		(finding lst minimizing (distance-from-colony lst))))

(defun back-home (ant)
  (if (at-colony-p ant)
	  (store-into-colony ant)
	  (with-slots (x y vx vy) ant
	   (destructuring-bind (nx ny)
		   (or ;;(if (no-pheromon-in-front ant)
			(position-nearest-to-colony ant)
			;;(position-with-highest-pheromon-around ant))
			(random-elt (valid-next-positions ant)))
		 (setf vx (- nx x) vy (- ny y) x nx y ny))
	   (incf (pheromon-at x y)
			 (* (ant-food ant) *initial-pheromon*)))))

