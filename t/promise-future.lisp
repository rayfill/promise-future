(in-package :cl-user)
(defpackage :promise-future-test
  (:use :cl
	:promise-future
	:cl-test-more))
(in-package :promise-future-test)

(plan nil)

(defun test1 (class)
  (let* ((promise (create-promise (lambda ()
				    (values
				     (+ 1)
				     (+ 1 1)
				     (+ 1 1 1)
				     (+ 1 1 1 1))) class))
	 (future (get-future promise)))

    (let ((result (multiple-value-list
		   (ignore-errors (slot-value future 'promise-future::result)))))
      (ok (and (null (first result)) (typep (second result) 'unbound-slot)))
      (ok (multiple-value-list (get-value future)) '(1 2 3 4))
      (ok (multiple-value-list (get-value future)) '(1 2 3 4))
      (ok (slot-value future 'promise-future::result) '(1 2 3 4))))

  (loop for i from 10 downto 0
     do (let* ((future (get-future (create-promise (lambda ()
						     (/ 1 i)) class)))
	       (result (handler-case
			   (get-value future)
			 (division-by-zero (e) e))))
	  (if (zerop i)
	      (ok (typep result 'division-by-zero))
	      (ok (= result (/ 1 i)))))))

(test1 'promise)
(test1 'threaded-promise)
(test1 'thread-pool-promise)

(finalize)
