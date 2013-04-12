(in-package :cl-user)
(defpackage :promise-future
  (:use :cl)
  (:export :promise  :threaded-promise :create-promise :get-future :get-value))

(in-package :promise-future)

(defclass promise ()
  ((func :accessor func :initarg :func)))

(defclass future ()
  ((promise :accessor promise :initarg :promise)
   (result :accessor result)))

(defclass threaded-promise (promise)
  ((worker :accessor thread :initarg :thread)))

(defgeneric create-promise (func class))
(defmethod create-promise (func (class (eql 'promise)))
  (make-instance 'promise :func func))

(defun thread-func-guard-generator (func)
  (lambda ()
    (handler-case
	(multiple-value-list (funcall func))
      (error (e) (values nil e)))))

(defun thread-result-func-generator (thread)
  (lambda () 
    (multiple-value-bind (mv-list error)
	(sb-thread:join-thread thread)
      (when error
	(error error))
      (apply #'values mv-list))))

(defmethod create-promise (func (class (eql 'threaded-promise)))
  (let* ((thread (sb-thread:make-thread 
		  (thread-func-guard-generator func)))
	 (result-func (thread-result-func-generator thread)))
  (make-instance 'threaded-promise
		 :func result-func :thread thread)))

(defgeneric get-future (promise))
(defmethod get-future ((inst promise))
  (make-instance 'future :promise inst))

(defun compute-result (future)
  (let* ((promise-func (func (promise future)))
	 (result (funcall promise-func)))
    (setf (slot-value future 'result) result)))

(defgeneric get-value (future))
(defmethod get-value ((inst future))
  (let* ((results (multiple-value-list 
		   (ignore-errors (slot-value inst 'result))))
	 (error (second results)))
    (cond
      ((typep error 'unbound-slot) (compute-result inst))
      ((typep error 'condition) (error error))
      (t (apply #'values results)))))
