(defpackage overmind-agents.log
  (:use #:cl)
  (:export #:stack
	   )
  (:nicknames #:omage.log))
(in-package :overmind-agents.log)

(defun stack (c)
  (with-open-file (str (merge-pathnames #P"omage-stack.log" #P"~/")
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (format str "==============~%~%~a~%" (local-time:now))
    (format str "~%~a~%~%" c)
    (loop for obj in (cdr (dissect:stack))
	  when (dissect:line obj)
	    do (format str "~a:~a ~a~%"
		       (dissect:file obj)
		       (dissect:line obj)
		       (dissect:call obj)))))
