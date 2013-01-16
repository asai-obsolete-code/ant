
(in-package :report.geometry)
(annot:enable-annot-syntax)
(speed*)

@export
@export-slots
(defclass 2dline (infinity-shape 2dsegment)
  ((direction :type 2dvector :initarg :direction)))

(defmethod direction ((line 2dline))
  (with-memoising-slot (direction line)
	(call-next-method)))

;; @inherited length :from infinity-shape
;; @inherited diameter :from infinity-shape

(defmethod congruent-p ((l1 2dline) (l2 2dline))
  (parallel-p (direction l1)
			  (direction l2)))

(defmethod congruent-p ((l 2dline) (s 2dsegment))
  (parallel-p l s))

(defmethod congruent-p ((s 2dsegment) (l 2dline))
  @ignore l s
  nil)

(defmethod parallel-p ((l1 2dline) (l2 2dline))
  (parallel-p (direction l1)
			  (direction l2)))

(defmethod perpendicular-p ((l1 2dline) (l2 2dline))
  (perpendicular-p (direction l1)
				   (direction l2)))

