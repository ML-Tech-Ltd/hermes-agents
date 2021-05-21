(defpackage hermes-agents.utils
  (:use :cl :json :alexandria)
  (:import-from #:hsinp.rates
		#:unix-from-nano)
  (:import-from #:fare-mop
		#:collect-slots)
  (:export #:*json-false*
	   #:is-market-close
	   #:read-str
	   #:refresh-memory
	   #:sorted-indexes
	   #:prepare-agents-properties
	   #:format-rr)
  (:nicknames #:hsage.utils))
(in-package :hermes-agents.utils)

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

(defun is-market-close ()
  (let ((day-of-week (local-time:timestamp-day-of-week (local-time:now) :timezone local-time:+utc-zone+))
	(hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (and hscom.all:*is-production*
	 (or
	  ;; Friday
	  (and (= day-of-week 5)
	       (>= hour 20))
	  ;; Saturday
	  (= day-of-week 6)
	  ;; Sunday
	  (and (= day-of-week 0)
	       (< hour 22))))))
;; (is-market-close)

(defun read-str (str)
  (read-from-string str))

(defun refresh-memory ()
  (fare-memoization:unmemoize 'read-str)
  (sb-ext:gc :full t)
  (fare-memoization:memoize 'read-str))

(defun sorted-indexes (list &optional (sort-fn #'<))
  (loop
    for i from 0
    with result = (make-list (length list))
    for element in (stable-sort
		    (loop
		      for index from 0
		      for element in list
		      collect (list index element))
		    sort-fn
		    :key #'second)
    do (setf (nth (first element) result) i)
    finally (return result)))

(defun prepare-agents-properties (agents)
  (loop for agent in agents
	collect (loop for (key value) on (collect-slots agent) by #'cddr
		      unless (or (string= key "STDEV-MAX-POS")
				 (string= key "AVG-MAX-POS")
				 (string= key "STDEV-MAX-NEG")
				 (string= key "AVG-MAX-NEG")
				 ;; (string= key "CREATION-BEGIN-TIME")
				 ;; (string= key "CREATION-END-TIME")
				 ;; (string= key "BEGIN-TIME")
				 ;; 	 (string= key "END-TIME")

				 (string= key "MIN-TP")
				 (string= key "MIN-SL")
				 (string= key "MAX-TP")
				 (string= key "MAX-SL")

				 (string= key "PERCEPTION-FNS")
				 (string= key "LOOKAHEAD-COUNT")
				 (string= key "LOOKBEHIND-COUNT")
				 (string= key "ANTECEDENTS")
				 (string= key "CONSEQUENTS")
				 (string= key "REVENUES")
				 (string= key "ENTRY-TIMES")
				 (string= key "EXIT-TIMES")
				 (string= key "ENTRY-PRICES")
				 (string= key "EXIT-PRICES")
				 (string= key "TPS")
				 (string= key "SLS")
				 (string= key "RETURNS"))
			collect (let ((value (cond ((or (string= key "CREATION-BEGIN-TIME")
							(string= key "CREATION-END-TIME")
							(string= key "BEGIN-TIME")
							(string= key "END-TIME")
							(string= key "TRAIN-BEGIN-TIME")
							(string= key "TRAIN-END-TIME"))
						    (unix-from-nano value))
						   ((string= key "ACTIVATIONS")
						    (if (and value (not (eq value :null)) (/= (length value) 0)) (format nil "~6$" (mean value)) "0"))
						   ((floatp value) (format nil "~6$" value))
						   (t (format nil "~a" value)))))
				  ;; (format nil "~a: ~a~%" key value)
				  `(,key . ,value)))))
;; (prepare-agents-properties (get-agents-some :AUD_USD hsage.config:*train-tf* '(:stagnated)))

(defun format-rr (risk reward)
  (format nil "~a / ~2$"
	  (if (= risk 0)
	      0
	      (/ (* 10000 (abs risk))
		 (* 10000 (abs risk))))
	  (if (= reward 0)
	      0
	      (/ (* 10000 (abs reward))
		 (* 10000 (abs risk))))))
;; (format-rr 10 30)

