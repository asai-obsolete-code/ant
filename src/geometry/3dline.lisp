

(in-package :report.geometry)
(annot:enable-annot-syntax)

@export
@export-slots
(defclass 3dline (infinity-shape 3dsegment)
  ((direction :type 3dvector :initarg :direction)))

(defmethod direction ((line 3dline))
  (with-memoising-slot (direction line)
	(call-next-method)))

;; @inherited length :from infinity-shape
;; @inherited diameter :from infinity-shape


