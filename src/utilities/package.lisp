
(in-package :cl-user)
(defpackage report.utilities
  (:use :cl
		:anaphora
		:annot
		:annot.class
		:annot.eval-when
		:annot.doc
		:annot.slot
		:iterate
		:alexandria)
  (:shadowing-import-from
   :closer-mop
   :class-slots
   :class-precedence-list
   :class-direct-subclasses
   :slot-definition-name
   ))

