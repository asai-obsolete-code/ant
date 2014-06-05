
(in-package :cl-user)
(defpackage report.geometry
  (:use :cl
        :annot.class
        :annot.eval-when
        :annot.doc
        :annot.slot
        :anaphora
        :iterate
        :alexandria
        :report.utilities)
  (:shadow :rotate))

(in-package :report.geometry)
(annot:enable-annot-syntax)

