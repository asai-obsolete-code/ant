#|
  This file is a part of ant project.
  Copyright (c) 2013 guicho
 ()
|#

(in-package :cl-user)
(defpackage ant-test-asd
  (:use :cl :asdf))
(in-package :ant-test-asd)

(defsystem ant-test
  :author "guicho
"
  :license "LLGPL"
  :depends-on (:ant
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "ant"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
