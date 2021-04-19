(defpackage hermes-agents.log
  (:use #:cl #:alexandria)
  (:import-from #:hscom.utils
		#:format-table)
  (:import-from #:hsage.utils
		#:format-rr)
  (:export #:log-stack
	   #:push-to-log
	   #:read-log
	   #:clear-log
	   #:push-to-agents-log
	   #:read-agents-log
	   #:clear-agents-log
	   #:push-to-agent-directions-log
	   #:read-agent-directions-log
	   #:clear-agent-directions-log
	   #:clear-logs
	   #:log-agent)
  (:nicknames #:hsage.log))
(in-package :hermes-agents.log)

(defun log-stack (c)
  (with-open-file (str (merge-pathnames #P"hsage-stack.log" #P"~/")
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

(defun get-clerk-jobs ()
  (with-open-stream (s (make-string-output-stream))
    (clerk:calendar s)
    (get-output-stream-string s)))
;; (get-clerk-jobs)

;; General log.
(let (log)
  (defun push-to-log (msg &key (add-newline? t) (size 5000))
    (when hscom.all:*is-log*
      (if add-newline?
	  (push (format nil "~a<br/>" msg) log)
	  (push (format nil "~a" msg) log))
      (when (> (length log) size)
	(setf log (butlast log)))))
  (defun read-log ()
    (format nil "<h3>CRON JOBS.</h3><hr/><br/>~a<h3>LOG.</h3><hr/><br/>~{~a~%~}"
	    (get-clerk-jobs)
	    (reverse log)
	    ))
  (defun clear-log ()
    (setf log nil)))
;; (push-to-log (random 10) :size 10)
;; (read-log)

;; Agents log.
(let (log)
  (defun push-to-agents-log (msg &key (add-newline? t) (size 5000))
    (when hscom.all:*is-log*
      (if add-newline?
	  (push (format nil "~a<br/>" msg) log)
	  (push (format nil "~a" msg) log))
      (when (> (length log) size)
	(setf log (butlast log)))))
  (defun read-agents-log ()
    (format nil ;; "~a<h3>AGENTS LOG.</h3><hr/><br/>~{~a~%~}"
	    ;; (describe-agents)
	    "~{~a~%~}"
	    (reverse log)
	    ))
  (defun clear-agents-log ()
    (setf log nil)))

;; (read-agents-log)
;; (read-log)

;; Agents' directions log.
(let ((log (make-hash-table :test 'equal)))
  (defun push-to-agent-directions-log (instrument timeframe types direction &key (size 1000))
    (when hscom.all:*is-log*
      (let ((pairing-directions (gethash instrument log))
	    (pattern-directions (gethash (list instrument timeframe types) log))
	    (global-directions (gethash :global log)))
	(push direction pairing-directions)
	(push direction pattern-directions)
	(push direction global-directions)
	(when (> (length pairing-directions) size)
	  (setf pairing-directions (butlast pairing-directions)))
	(when (> (length pattern-directions) size)
	  (setf pattern-directions (butlast pattern-directions)))
	(when (> (length global-directions) size)
	  (setf global-directions (butlast global-directions)))
	(setf (gethash instrument log) pairing-directions)
	(setf (gethash (list instrument timeframe types) log) pattern-directions)
	(setf (gethash :global log) global-directions))))
  (defun read-agent-directions-log ()
    (format nil "<pre>~{~a~%~}</pre>"
	    (loop for key being each hash-key of log
		  for value being each hash-value of log
		  collect (format nil "~{~a~^, ~}: BULL: ~a, BEAR: ~a~%"
				  (flatten key)
				  (length (remove-if-not #'plusp value))
				  (length (remove-if-not #'minusp value))))))
  (defun clear-agent-directions-log ()
    (setf log (make-hash-table :test 'equal))))

;; (push-to-agent-directions-log :AUD_USD hscom.hsage:*train-tf* '(:bullish) 1.1)
;; (read-agent-directions-log)

(defun clear-logs ()
  (clear-log)
  (clear-agents-log)
  (clear-agent-directions-log))
