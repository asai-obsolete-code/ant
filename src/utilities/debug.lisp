
(in-package :report.utilities)
(enable-annot-syntax)

@export
(defun break+ (&rest args)
  (break "~{~a~%~}" args))

@export
(defun break* (&rest args)
  (iter (for arg in args)
		(break "~a" arg)))