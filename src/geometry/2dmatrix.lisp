
(in-package :report.geometry)
(annot:enable-annot-syntax)
(speed*)
@export
@doc "2-dimentional affine conversion matrix."
(deftype 2dmatrix ()
  '(simple-array *desired-type* (2 3)))

(declaim (inline make-2dmatrix))

@export
(defun make-2dmatrix (&optional lst)
  (if lst
	  (make-array '(2 3)
				  :initial-contents lst
				  :element-type '*desired-type*)
	  (make-array '(2 3)
				  :element-type '*desired-type*)))

@export
(defun make-identity-matrix ()
  (make-2dmatrix
   '((1.0d0 0.0d0 0.0d0)
	 (0.0d0 1.0d0 0.0d0))))

@export
(defun rotation-matrix (rad)
  (let ((s (dsin rad))
		(c (dcos rad)))
	(make-2dmatrix
	 `((,c ,(- s) 0.0d0)
	   (,s ,c     0.0d0)))))

(defmethod rotate ((v 2dvector) (rad double-float))
  (apply-matrix (rotation-matrix rad) v))

(defmethod rotate ((v 2dvector) (m array))
  (apply-matrix m v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; methods

;;;;;;;;;;;;;;;; M * M ;;;;;;;;;;;;;;;;

@export
(defun matrix-add (m1 m2)
  @type 2dmatrix m1
  @type 2dmatrix m2
  (let ((m (make-2dmatrix)))
	(loop for i from 0 below 6
	   do (setf (row-major-aref m i)
				(d+ (row-major-aref m1 i)
					(row-major-aref m2 i)))
	   finally (return m))))

@export
(defun matrix-sub (m1 m2)
  @type 2dmatrix m1
  @type 2dmatrix m2
  (let ((m (make-2dmatrix)))
	(loop for i from 0 below 6
	   do (setf (row-major-aref m i)
				(d- (row-major-aref m1 i)
					(row-major-aref m2 i)))
	   finally (return m))))

;; | A b |   | A' b' |   | AA' Ab'+b |
;; |     | x |       | = |           |
;; | 0 1 |	 | 0  1  |	 | 0     1   |

@export
(defun matrix-dot (m1 m2)
  @type 2dmatrix m1
  @type 2dmatrix m2
  (let ((m (make-2dmatrix)))
	(loop
	   for i from 0 below 2
	   ;; AA'
	   do (loop for j from 0 below 2
			 do (setf (daref m i j)
					  (the *desired-type*
						(loop for k from 0 below 2
						 sum (d* (daref m1 i j) (daref m2 j k))))))
	   do (setf (daref m i 2)
				(d+ (loop for j from 0 below 2
					   sum (d* (daref m1 i j)
							   (daref m2 j 2)))
					(daref m1 i 2)))
	   finally (return m))))

@export
(defun matrix-neg (m1)
  @type 2dmatrix m1
  (let ((m (make-2dmatrix)))
	(loop for i from 0 below 6
	   do (setf (row-major-aref m i)
				(- (row-major-aref m1 i)))
	   finally (return m))))

;; (defun nadd (m1 m2)
;;   (let ((m1 (a-of m1))
;; 		(m2 (a-of m2)))
;; 	(loop for i from 0 below 6
;; 	   do (setf (row-major-aref m1 i)
;; 				(d+ (row-major-aref m1 i)
;; 					(row-major-aref m2 i)))
;; 	   finally (return m1))))

;; (defun nsub (m1 m2)
;;   (let ((m1 (a-of m1))
;; 		(m2 (a-of m2)))
;; 	(loop for i from 0 below 6
;; 	   do (setf (row-major-aref m1 i)
;; 				(d- (row-major-aref m1 i)
;; 					(row-major-aref m2 i)))
;; 	   finally (return m1))))

;;;;;;;;;;;;;;;; M * vector ;;;;;;;;;;;;;;;;

@export
(defun apply-matrix (matrix vector)
  @type 2dmatrix matrix
  @type 2dvector vector
  (2dv (d+ (d* (daref matrix 0 0) (x-of vector))
		   (d* (daref matrix 0 1) (y-of vector))
		   (daref matrix 0 2))
	   (d+ (d* (daref matrix 1 0) (x-of vector))
		   (d* (daref matrix 1 1) (y-of vector))
		   (daref matrix 1 2))))

