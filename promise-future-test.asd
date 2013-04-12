#|
  This file is a part of promise-future project.
|#

(in-package :cl-user)
(defpackage promise-future-test-asd
  (:use :cl :asdf))
(in-package :promise-future-test-asd)

(defsystem promise-future-test
  :author ""
  :license ""
  :depends-on (:promise-future
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "promise-future"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
