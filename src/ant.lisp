

(in-package :cl-user)
(annot:enable-annot-syntax)

(defpackage ant
  (:use :cl
		:annot.doc
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

(defvar *step-ms* 100)

(defparameter *width* 100)
(defparameter *height* 100)
(defvar *field* (make-array (list *width* *height*)))
(defparameter *colony-x* (random *width*))
(defparameter *colony-y* (random *height*))
(defparameter *stored-food* 0)
(defparameter *ants* nil)
(defparameter *ant-max-food* 5)
(defparameter *ant-sight* 5)
(defparameter *ant-smelling* 10)
(defparameter *field-max-food* 200)
(defparameter *field-max-pheromon* 200)
(defparameter *initial-pheromon* 25)
(defparameter *pheromon-evaporation-rate* 1)
(defparameter *pheromon-appoximation-unit* 5)
(defparameter *pheromon-detectable-limit* 5)

(defparameter *default-food-rate* 0.0d0)
(defparameter *default-ants-number* 50)

(defparameter *obstacles* (make-array (list *width* *height*)
									  :element-type 'boolean
									  :initial-element nil))

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

(defun approximate (x divisor)
  (* divisor (floor x divisor)))

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

(defun ant-move-to (ant x-y-pos)
  (with-slots (x y vx vy) ant
	(destructuring-bind (nx ny) x-y-pos
	  (setf vx (- nx x) vy (- ny y) x nx y ny))))

(defun emit-pheromon (ant &optional (strength 1) (diffusion 0.5))
  (iter (for (x y) in (positions-backward ant 1))
		(incf (pheromon-at x y)
			  (* diffusion
				 strength
				 (ant-food ant)
				 *initial-pheromon*))
		(when (> (pheromon-at x y) *field-max-pheromon*)
		  (setf (pheromon-at x y) *field-max-pheromon*)))
  (with-slots (x y) ant
	(incf (pheromon-at x y)
		  (* strength
			 (ant-food ant)
			 *initial-pheromon*))
	(when (> (pheromon-at x y) *field-max-pheromon*)
		  (setf (pheromon-at x y) *field-max-pheromon*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; obstacle


(defun obstacle-at (x y)
  (aref *obstacles* x y))
(defun (setf obstacle-at) (value x y)
  (setf (aref *obstacles* x y) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; setup

(defun setup ()
  (setup-field *default-food-rate*)
  (setup-ants *default-ants-number*)
  (setup-obstacles))

(defun setup-obstacles ()
  (with-iter-array-row-major (o) *obstacles*
	(setf o nil)))

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
;;;; ant collecting and storing food

(defun find-food (ant)
  (field-food (ant-field ant)))

(defun food-found-p (ant)
  (plusp (find-food ant)))

(defun collect-food (ant)
  (decf (field-food (ant-field ant)) *ant-max-food*)
  (setf (ant-food ant) *ant-max-food*)
  (when (minusp (field-food (ant-field ant)))
	(setf (field-food (ant-field ant)) 0))
  (emit-pheromon ant 3.0))

(defun store-into-colony (ant)
  (incf *stored-food* (ant-food ant))
  (setf (ant-food ant) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant looking around

(defun apply-to-pos (fn)
  (lambda (pos) ;; x y list of position
	(destructuring-bind (x y) pos
	  (funcall fn x y))))

(defun remove-collision (lst)
  (remove-if
   (apply-to-pos #'obstacle-at)
   lst))

(defun ensure-collision (pos)
  (when pos
	(unless (funcall (apply-to-pos #'obstacle-at) pos)
	  pos)))

(defun positions-around-native (x y radius)
  (iter
	(for dx from (- radius) to radius)
	(appending
	 (iter
	   (for dy from (- radius) to radius)
	   (when (= dx dy 0) (next-iteration))
	   (collect (list (mod (+ x dx) *width*)
					  (mod (+ y dy) *height*)))))))

(defun positions-around (ant radius)
  (with-slots (x y vx vy) ant
	(remove-collision
	 (positions-around-native x y radius))))

(defun positions-forward (ant radius)
  (with-slots (x y vx vy) ant
	(remove-if
	 (lambda (x-y)
	   (destructuring-bind (x1 y1) x-y
		 (not (plusp (easy-dot vx vy (- x1 x) (- y1 y))))))
	 (positions-around ant radius))))

(defun positions-backward (ant radius)
  (with-slots (x y vx vy) ant
	(remove-if
	 (lambda (x-y)
	   (destructuring-bind (x1 y1) x-y
		 (not (minusp (easy-dot vx vy (- x1 x) (- y1 y))))))
	 (positions-around ant radius))))

(defun positions-intelligent (ant radius)
  (with-slots (x y) ant
	(if (plusp (pheromon-at x y))
		(positions-forward ant radius)
		(positions-around ant radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; various searching scheme


(defun position-with-largest-food (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (let ((fn (apply-to-pos #'food-at)))
		(destructuring-bind (tx ty)
			(car (sort (remove-if-not #'plusp
									  (positions-intelligent ant radius)
									  :key fn)
					   #'> :key fn))
		  (ensure-collision
		   (list (mod (+ x (signum (- tx x))) *width*)
				 (mod (+ y (signum (- ty y))) *width*))))))))

(defun position-with-pheromon (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (let ((fn (apply-to-pos #'pheromon-at)))
		(destructuring-bind (tx ty)
			(random-elt (remove-if-not #'plusp
									   (positions-intelligent ant radius)
									   :key fn))
		  (ensure-collision
		   (list (mod (+ x (signum (- tx x))) *width*)
				 (mod (+ y (signum (- ty y))) *width*))))))))

(defun position-with-least-but-decent-pheromon (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (let ((fn (apply-to-pos #'pheromon-at)))
		(destructuring-bind (tx ty)
			(car (sort (remove-if-not #'plusp
									  (positions-intelligent ant radius)
									  :key fn)
					   #'<  :key fn))
		  (ensure-collision
		   (list (mod (+ x (signum (- tx x))) *width*)
				 (mod (+ y (signum (- ty y))) *width*))))))))

(defun stdev (lst)
  (let* ((m (mean lst))
		 (var (reduce #'+ (mapcar (lambda (x) (^2 (- x m))) lst))))
	(values (sqrt var)
			m var)))

(defparameter *irregular-rate* 3)

(defun least-or-wave-top (poslst)
  (let ((scores (mapcar (apply-to-pos #'pheromon-at) poslst)))
	(multiple-value-bind (sigma mean) (stdev scores)
	  (if-let ((n (position-if (lambda (x) (> (/ (- x mean) sigma)
									   *irregular-rate*))
						scores)))
		(nth n poslst)
		(iter (for pos in poslst)
			  (finding pos
					   minimizing
					   (funcall (apply-to-pos #'pheromon-at) pos)))))))
						
(defun position-with-least-or-wave-top (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (destructuring-bind (tx ty)
		  (least-or-wave-top
		   (remove-if-not #'plusp
						  (positions-intelligent ant radius)
						  :key (apply-to-pos #'pheromon-at)))
		(ensure-collision
		 (list (mod (+ x (signum (- tx x))) *width*)
			   (mod (+ y (signum (- ty y))) *width*)))))))


(defun position-with-most-pheromon (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (let ((fn (apply-to-pos #'pheromon-at)))
		(destructuring-bind (tx ty)
			(car (sort (remove-if-not #'plusp
									  (positions-intelligent ant radius)
									  :key fn)
					   #'>  :key fn))
		  (ensure-collision
		   (list (mod (+ x (signum (- tx x))) *width*)
				 (mod (+ y (signum (- ty y))) *width*))))))))

(defun position-when-go-straight (ant)
  (with-slots (x y vx vy) ant
	(ensure-collision
	 (list (mod (+ x vx) *width*)
		   (mod (+ y vy) *height*)))))

(defun position-most-away-from-other-ants (ant minimum-distance)
  (ensure-collision
   (iter (for lst in (positions-intelligent ant 1))
		 (for (x y) = lst)
		 (for mindist =
			  (iter
				(for another in (remove ant *ants*))
				(minimizing
				 (euclid-distance
				  x y (ant-x another) (ant-y another)))))
		 (when (< mindist minimum-distance)
		   (finding lst maximizing mindist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant going back home

(defun distance-from-colony (dlst)
  (manhattan-distance
   (first dlst) (second dlst)
   *colony-x* *colony-y*))

(defun position-nearest-to-colony (ant)
  (ensure-collision
   (iter (for lst in (positions-intelligent ant 1))
		 (finding lst minimizing (distance-from-colony lst)))))

(defun within-radius-of-colony (radius)
  (lambda (pos)
	(< (distance-from-colony pos) radius)))

(defun position-of-colony-within-sight (ant radius)
  (ignore-errors
	(with-slots (x y) ant
	  (destructuring-bind (tx ty)
		  (car (sort (remove-if-not (within-radius-of-colony radius)
									(positions-intelligent ant radius))
					 #'< :key #'distance-from-colony))
		(ensure-collision
		 (list (mod (+ x (signum (- tx x))) *width*)
			   (mod (+ y (signum (- ty y))) *width*)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ant behaviors

(defun search-food (ant)
  (if (food-found-p ant)
	  (collect-food ant)
	  (ant-move-to 
	   ant 
	   (or (position-with-largest-food ant *ant-sight*)
		   (bias-cond
			 (20 (position-with-pheromon ant *ant-smelling*))
			 (0 (position-with-least-or-wave-top ant *ant-smelling*))
			 (0 (position-with-least-but-decent-pheromon
				 ant *ant-smelling*))
			 (0 (position-with-most-pheromon ant *ant-smelling*))
			 (1 nil))
		   (bias-cond
			 (10   (position-most-away-from-other-ants ant *ant-sight*))
			 (50   (position-when-go-straight ant))
			 (1    (when-let ((fw (positions-forward ant 1)))
					 (random-elt fw))))
		   (when-let ((fw (positions-forward ant 1)))
			 (random-elt fw))
		   (random-elt (positions-around ant 1))))))


(defun back-home (ant)
  (emit-pheromon ant)
  (if (at-colony-p ant)
	  (store-into-colony ant)
	  (ant-move-to 
	   ant 
	   (or (position-of-colony-within-sight ant *ant-sight*)
		   (bias-if 0.2d0
					(position-nearest-to-colony ant))
		   (bias-cond
			 (20 (position-with-pheromon ant *ant-smelling*))
			 (20 (position-with-least-but-decent-pheromon
				 ant *ant-smelling*))
			 (0 (position-with-most-pheromon ant *ant-smelling*))
			 (20 nil))
		   (bias-cond
			 (20  (position-when-go-straight ant))
			 (5  (when-let ((fw (positions-forward ant 1)))
				   (random-elt fw))))
		   (when-let ((fw (positions-forward ant 1)))
			 (random-elt fw))
		   (random-elt (positions-around ant 1))))))
