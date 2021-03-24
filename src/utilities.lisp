(defpackage overmind-agents.utilities
  (:use :cl :json)
  (:export #:*json-false*
	   #:*defparameters*
	   #:dbg)
  (:nicknames #:omage.utils))
(in-package :overmind-agents.utilities)

(defmacro defparameters (exprs)
  `(progn ,@(loop for (doc name exp) in exprs
                  collect `(defparameter ,name ,exp ,doc))))

(defun dbg (&rest args)
  (format t "~{~a~^, ~}~%" args))

(defclass json-false ()
  ())

(defmethod json:encode-json ((object json-false) &optional stream)
  (princ "false" stream)
  nil)

(defvar *json-false* (make-instance 'json-false))

(defun json-bool (val)
  (if val t *json-false*))

(defun json-bool-handler (token)
  (or (string= token "true")
      (and (string= token "false") *json-false*)))

(defmacro preserving-json-boolean (opts &body body)
  (declare (ignore opts))
  `(let ((json:*boolean-handler* #'json-bool-handler))
     ,@body))

;; (define-alien-variable ("dynamic_space_size" dynamic-space-size-bytes)
;; unsigned-long)
;; (defun heap-n-bytes ()
;;   (+ dynamic-space-size-bytes
;;      (- sb-vm::read-only-space-end sb-vm::read-only-space-start)
;;      (- sb-vm::static-space-end sb-vm::static-space-start)))
;; (heap-n-bytes)
