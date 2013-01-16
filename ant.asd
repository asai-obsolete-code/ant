#|
  This file is a part of ant project.
  Copyright (c) 2013 guicho
 ()
|#

#|
  

  Author: guicho
 ()
|#

(in-package :cl-user)
(defpackage ant-asd
  (:use :cl :asdf))
(in-package :ant-asd)

(defsystem ant
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (
			   :cl-annot
			   :closer-mop
			   :vecto
               :anaphora
               :iterate
               :alexandria)
  :components ((:module "src" :serial t
                :components
                ((:module :utilities :serial t :components
						  ((:file :package)
						   (:file :utilities)
						   (:file :macros)
						   (:file :random)
						   (:file :object)
						   (:file :classes)
						   (:file :debug)
						   (:file :declaim)))
				 (:file :ant))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op ant-test))))
