
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; generic functions for resolving the conflicts.
;;;;
;;;;

(in-package :report.utilities)
(enable-annot-syntax)

@export
@doc "stepping the object"
(defgeneric stepping (object))

(defmethod stepping (o) nil)

@export
@doc "reset the simulater to its initial state"
(defgeneric reset (simulator))

@export
@doc "start the stepping of the simulator"
(defgeneric start (simulator))

@export
@doc "start logging the simulator"
(defgeneric start-log (simulator))

@export
@doc "end logging the simulator"
(defgeneric end-log (simulator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

@export
@doc "register an object into the superior container. NOTE: it DOES NOT
 and MUST NOT register the object recursively."
(defgeneric register (object container)
  (:method-combination progn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collection functions (plural)

@export
(defgeneric segments-of (obj))
@export
(defgeneric crossings-of (obj))
@export
(defgeneric connectors-of (obj))
@export
(defgeneric segment-connectors-of (obj))
@export
(defgeneric lanes-of (obj))
@export
(defgeneric agents-of (obj))
@export
(defgeneric equipments-of (obj))

@export
(defgeneric (setf segments-of) (new-value obj))
@export
(defgeneric (setf crossings-of) (new-value obj))
@export
(defgeneric (setf connectors-of) (new-value obj))
@export
(defgeneric (setf segment-connectors-of) (new-value obj))
@export
(defgeneric (setf lanes-of) (new-value obj))
@export
(defgeneric (setf agents-of) (new-value obj))
@export
(defgeneric (setf equipments-of) (new-value obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; upward reference. setf-able


@export
(defgeneric simulator-of (obj))
@export
(defgeneric segment-of (obj))
@export
(defgeneric segment-connector-of (obj))
@export
(defgeneric lane-of (obj))


@export
(defgeneric (setf simulator-of) (new-value obj))
@export
(defgeneric (setf segment-of) (new-value obj))
@export
(defgeneric (setf segment-connector-of) (new-value obj))
@export
(defgeneric (setf lane-of) (new-value obj))

