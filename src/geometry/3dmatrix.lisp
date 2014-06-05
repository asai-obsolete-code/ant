
(in-package :report.geometry)
(annot:enable-annot-syntax)

@export
@doc "3-dimentional affine conversion matrix."
(defclass 3dmatrix (recursive-print-mixin)
  ((array :type (array *desired-type* (3 4))
          :initarg :array
          :initform (make-array '(3 4) :element-type '*desired-type*)
          :accessor a-of
          :accessor m-of)))

(declaim (inline make-3dm-array))

@export
(defun make-3dm-array (&optional lst)
  (if lst
      (make-array '(3 4)
                  :initial-contents lst
                  :element-type '*desired-type*)
      (make-array '(3 4)
                  :element-type '*desired-type*)))

@export
(defun rot-x (rad)
  (let ((s (sin rad))
        (c (cos rad)))
    (make-instance
     '3dmatrix
     :array (make-3dm-array  
             `((1 0  0      0)
               (0 ,c ,(- s) 0)
               (0 ,s ,c     0))))))

@export
(defun rot-y (rad)
  (let ((s (sin rad))
        (c (cos rad)))
    (make-instance
     '3dmatrix
     :array (make-3dm-array  
             `((,c     0    ,s   0)
               (0      1    0    0)
               (,(- s) 0    ,c   0))))))

@export
(defun rot-z (rad)
  (let ((s (sin rad))
        (c (cos rad)))
    (make-instance
     '3dmatrix
     :array (make-3dm-array
             `((,c ,(- s) 0 0)
               (,s ,c 0 0)
               (0 0 1 0))))))

@export
@doc "x, y, z is in radians"
(defun rotate-rad (x y z)
  (dot (dot (rot-x x)
            (rot-y y))
       (rot-z z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; methods

(defmethod add ((m1 3dmatrix) (m2 3dmatrix))
  (let ((m (make-3dm-array))
        (m1 (m-of m1))
        (m2 (m-of m2)))
    (loop for i from 0 below 12
       do (setf (row-major-aref m i)
                (d+ (row-major-aref m1 i)
                    (row-major-aref m2 i)))
       finally (return (make-instance '3dmatrix :array m)))))

(defmethod sub ((m1 3dmatrix) (m2 3dmatrix))
  (let ((m (make-3dm-array))
        (m1 (m-of m1))
        (m2 (m-of m2)))
    (loop for i from 0 below 12
       do (setf (row-major-aref m i)
                (d- (row-major-aref m1 i)
                    (row-major-aref m2 i)))
       finally (return (make-instance '3dmatrix :array m)))))

(defmethod dot ((m1 3dmatrix) (m2 3dmatrix))
  (loop
     with a1 = (a-of m1)
     with a2 = (a-of m2)
     with a = (make-3dm-array)
     for i from 0 below 3
     do (loop for j from 0 below 3
           do (setf (aref a i j)
                    (+ (loop for k from 0 below 3
                          sum (* (aref a1 i j) (aref a2 j k)))
                       (aref a i 3))))
     do (setf (aref a i 3)
              (+ (loop for j from 0 below 3
                    sum (* (aref a1 i j) (aref a2 j 3)))
                 (aref a1 i 3)))
     finally (return (make-instance '3dmatrix :array a))))


;; (defmethod dot ((m 3dmatrix) (v1 3dvector))
;;   (let ((a (a-of m))
;; 		(v (v-of v1))
;; 		(v2 (make-3dv-vector)))
;; 	(loop for i from 0 to 2
;; 	   do (setf (aref v2 i)
;; 				(+ (loop for j from 0 to 2
;; 					  sum (* (aref a i j) (aref v j)))
;; 				   (aref a i 3))))
;; 	(make-instance '3dvector :vector v2)))

(defmethod neg ((m1 3dmatrix))
  (let ((m (make-3dm-array))
        (m1 (m-of m1)))
    (loop for i from 0 below 12
       do (setf (row-major-aref m i)
                (- (row-major-aref m1 i)))
       finally (return (make-instance '3dmatrix :array m)))))

(defmethod nadd ((m1 3dmatrix) (m2 3dmatrix))
  (let ((a1 (a-of m1))
        (a2 (a-of m2)))
    (loop for i from 0 below 12
       do (setf (row-major-aref a1 i)
                (d+ (row-major-aref a1 i)
                    (row-major-aref a2 i)))
       finally (return m1))))

(defmethod nsub ((m1 3dmatrix) (m2 3dmatrix))
  (let ((a1 (a-of m1))
        (a2 (a-of m2)))
    (loop for i from 0 below 12
       do (setf (row-major-aref a1 i)
                (d- (row-major-aref a1 i)
                    (row-major-aref a2 i)))
       finally (return m1))))

