

(in-package :report.geometry)
(annot:enable-annot-syntax)


@export
@export-slots
(defclass infinity-shape ()
  ((diameter :initarg MOST-POSITIVE-DOUBLE-FLOAT)))

(defmethod diameter ((inf infinity-shape))
  (with-slots (diameter) inf
	diameter))
		
(defmethod dimension ((inf infinity-shape))
  (2dv MOST-POSITIVE-DOUBLE-FLOAT
	   MOST-POSITIVE-DOUBLE-FLOAT))

(defmethod boundary ((inf infinity-shape))
  (rect LEAST-NEGATIVE-DOUBLE-FLOAT
		LEAST-NEGATIVE-DOUBLE-FLOAT
		MOST-POSITIVE-DOUBLE-FLOAT
		MOST-POSITIVE-DOUBLE-FLOAT))