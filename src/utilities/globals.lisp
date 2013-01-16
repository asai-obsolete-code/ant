
(in-package :report.utilities)
(annot:enable-annot-syntax)

@export
@doc "report singleton object."
(defvar *simulator* nil)

@export
@doc "dt in milliseconds"
(defparameter +dt+ 1.0d-1)

@export
(defparameter *main-thread-output* *standard-output*)
