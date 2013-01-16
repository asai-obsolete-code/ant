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
			   :cl-gtk2-gtk
			   :cl-gtk2-pango
			   :cl-gtk2-gdk
			   :cl-gtk2-glib
			   :cl-gtk2-cairo
               :anaphora
               :iterate
               :alexandria)
  :components ((:module "src" :serial t
                :components
                ((:module :utilities :serial t :components
						  ((:file :package)
						   (:file :declaim)
						   (:file :typed-ops)
						   (:file :utilities)
						   (:file :macros)
						   (:file :random)
						   (:file :object)
						   (:file :generics)
						   (:file :globals)
						   (:file :classes)
						   (:file :debug)))
				 (:module :geometry :serial t :components
						  ((:file :package)
						   (:file :generics)
						   (:file :generics-vector)
						   ;; basic classes
						   (:file :range) (:file :2dvector) 
						   (:file :2+1dvector)
						   (:file :2dshape) (:file :2dmatrix)
						   ;; mixins
						   (:file :infinite-shape)
						   (:file :directionable)
						   (:file :2dpolygon)
						   (:file :radius-diameter)
						   ;; classes
						   (:file :2dsegment) (:file :2drectangle)
						   (:file :2dline) (:file :2dcircle)
						   (:file :2dobb)
						   ;; (:file :3dsegment) (:file :hexahedron)
						   ;; (:file :3dplane) (:file :3dline)
						   ;; (:file :3dsphere) ;; (:file :3+1dvector)	 
						   (:file :intersects-p)))
				 (:file :ant)
				 (:file :events)
				 (:file :graphics))))
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
