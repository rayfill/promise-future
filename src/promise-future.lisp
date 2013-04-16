(in-package :cl-user)
(defpackage :promise-future
  (:use :cl)
  (:import-from :thread-pool :thread-pool :execute :*default-thread-pool*)
  (:import-from :sb-thread :join-thread
		:make-semaphore :signal-semaphore :wait-on-semaphore)
  (:import-from :alexandria :curry)
  (:export :promise :threaded-promise :thread-pool-promise
	   :create-promise :get-future :get-value))

(in-package :promise-future)

(defmacro with-critical-section ((semaphore) &body body)
  (let ((sem (gensym)))
  `(let ((,sem ,semaphore))
     (unwind-protect
	(progn
	  (wait-on-semaphore ,sem)
	  ,@body)
     (signal-semaphore ,sem)))))

(defclass promise ()
  ((func :accessor func :initarg :func)))

(defclass future ()
  ((promise :accessor promise :initarg :promise)
   (result :accessor result)))

(defclass threaded-promise (promise)
  ((worker :accessor thread :initarg :thread)))

(defun canonicalize-result-funcall (func)
  (handler-case
      (append '(nil) (multiple-value-list (funcall func)))
    (error (e) (list e))))

(defgeneric create-promise (func class))
(defmethod create-promise (func (class (eql 'promise)))
  (make-instance 'promise
		 :func (func-guard-generator func)))

(defun func-guard-generator (func)
  (curry #'canonicalize-result-funcall func))

(defmethod create-promise (func (class (eql 'threaded-promise)))
  (let ((thread (sb-thread:make-thread 
		 (func-guard-generator func))))
    (make-instance 'threaded-promise
		   :func (curry #'join-thread thread) :thread thread)))

(defgeneric get-future (promise))
(defmethod get-future ((inst promise))
  (make-instance 'future :promise inst))

(defun compute-result (future)
  (let* ((promise-func (func (promise future)))
	 (result (funcall promise-func)))
    (setf (slot-value future 'result) result)))

(defgeneric get-value (future))
(defmethod get-value ((inst future))
  (destructuring-bind (err . val-list)
      (handler-case
	  (slot-value inst 'result)
	(unbound-slot () (compute-result inst)))
    (when err
      (error err))
    (apply #'values val-list)))

(defclass thread-pool-promise (promise)
  ((is-finished :initform (make-semaphore :name "finish barrior"))
   (future :type future)
   (result :initarg :result :type list)))
   
(defun finish-notify (tp-promise)
  (with-slots (is-finished)
      tp-promise
    (signal-semaphore is-finished)))
      
(defmethod create-promise (func (class (eql 'thread-pool-promise)))
  (unless (typep *default-thread-pool* 'thread-pool:thread-pool)
    (error 'simple-error :format-control "not set thread pool source."))
  (let ((inst (make-instance 'thread-pool-promise :func func :result nil)))
    (thread-pool:execute
     *default-thread-pool*
     (lambda ()
       (setf (slot-value inst 'result) (funcall (curry #'canonicalize-result-funcall func)))
       (finish-notify inst)))
    inst))

(defmethod initialize-instance :after ((instance thread-pool-promise) &key)
  (with-slots (is-finished func result)
      instance
    (setf func
	  (lambda ()
	    (with-critical-section (is-finished)
	      result)))))

(defmethod get-future ((inst thread-pool-promise))
  (make-instance 'future :promise inst))
