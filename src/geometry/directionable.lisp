
(in-package :report.geometry)
(annot:enable-annot-syntax)

@export
(defclass directionable ()
  ())

(defmethod direction ((dable directionable))
  (with-slots (from to) dable
	(sub to from)))

(defmethod direction-1 ((dable directionable))
  (normalize (direction dable)))

(defmethod parallel-p ((d1 directionable)
					   (d2 directionable))
  (parallel-p (direction d1)
			  (direction d2)))

(defmethod perpendicular-p ((d1 directionable)
					   (d2 directionable))
  (perpendicular-p (direction d1)
				   (direction d2)))

(delegate-method angle ((dable directionable (direction dable))))