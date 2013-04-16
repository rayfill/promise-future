(in-package :cl-user)
(defpackage promise-future-asd
  (:use :cl :asdf))
(in-package :promise-future-asd)

(defsystem promise-future
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:alexandria :thread-pool)
  :components ((:module "src"
                :components
                ((:file "promise-future"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op promise-future-test))))
