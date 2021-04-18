;; (ql:quickload :hermes-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (loop-optimize-test)
;; (clerk:calendar)
;; (progn (drop-database) (init-database) (init-patterns) (clear-logs) (when omcom.all:*is-production* (clear-jobs)))

(defpackage hermes-agents
  (:use #:cl
	#:local-time
	#:access
	#:lparallel
	#:postmodern
	#:alexandria
	#:computable-reals
	#:defenum
	#:fare-mop
	#:hermes-input
	#:hermes-perception
	#:hermes-intuition
	#:hermes-agents.db
	#:hermes-agents.km
	#:hermes-agents.utils
	#:hermes-agents.log)
  (:import-from #:omcom.utils
		#:assoccess)
  (:import-from #:omage.log
		#:log-stack
		#:clear-logs)
  (:import-from #:omage.trading
		#:agent
		#:optimization
		#:gen-agent
		#:test-agents
		#:evaluate-agent
		#:evaluate-agents
		#:test-most-activated-agents
		#:add-agent
		#:sync-agents
		#:remove-agent
		#:is-agent-dominated?
		#:get-agents-count
		#:wipe-agents
		#:get-agent
		#:update-agent-fitnesses
		#:update-agents-fitnesses
		#:init-patterns
		#:validate-trades
		#:get-trade-result)
  (:import-from #:ominp.db
		#:conn)
  (:import-from #:omage.db
		#:init-database
		#:drop-database)
  (:import-from #:ominp.rates
		#:get-rates-chunk-of-types
		#:fracdiff
		#:sync-datasets-to-database
		#:sync-datasets-from-database
		#:get-rates-count-big
		#:get-rates-random-count-big)
  (:export #:loop-optimize-test)
  (:nicknames #:omage))
(in-package :hermes-agents)

;; FARE-MEMOIZATION configuration.
(setf fare-memoization::*memoized* (make-hash-table :test #'equal :synchronized t))

;; (describe fare-memoization::*memoized*)
;; (omage.utils:refresh-memory)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
			    (if (/= ideal-cores-count 0)
				ideal-cores-count 1))))

(defun clear-jobs ()
  (when (> (length clerk:*jobs*) 0)
    (ignore-errors
     (clerk:stop)
     (clerk:empty-jobs-queue))))

(defun debug-trade ()
  (let* ((trade (nth 5 (conn (query (:select 'trades.* 'patterns.*
					     :from 'trades
					     :inner-join 'patterns-trades
					     :on (:= 'trades.id 'patterns-trades.trade-id)
					     :inner-join 'patterns
					     :on (:= 'patterns.id 'patterns-trades.pattern-id)
					     :where (:= 'patterns.instrument "USD_CNH")
					     ) :alists))))
	 (from (print (* (assoccess trade :creation-time) 1000000)))
	 (from-timestamp (local-time:unix-to-timestamp (/ from 1000000))))
    (let* ((instrument (make-keyword (assoccess trade :instrument)))
	   (timeframe :M1)
	   (to (* (local-time:timestamp-to-unix (local-time:timestamp+ from-timestamp 3 :day)) 1000000))
	   (rates (ominp.rates:get-rates-range instrument timeframe from to :provider :oanda :type :fx))
	   (result (get-trade-result (assoccess trade :entry-price)
				     (assoccess trade :tp)
				     (assoccess trade :sl)
				     rates)))

      (format t "Result: ~a~%" result)
      (format t "Entry: ~a~%" (assoccess trade :entry-price))
      (format t "TP: ~a~%" (assoccess trade :tp))
      (format t "SL: ~a~%" (assoccess trade :sl))
      (if (plusp (assoccess trade :tp))
	  (loop for rate in rates do (print (assoccess rate :high-ask)))
	  (loop for rate in rates do (print (assoccess rate :high-bid)))))))
;; (->diff-close-frac-bid *rates* :offset 1)

(defun create-signals-job (seconds)
  (eval `(clerk:job "Creating signals" every ,(read-from-string (format nil "~a.seconds" seconds)) (-loop-test-all)))
  (clerk:start))

;; (require 'sb-sprof)
;; (sb-sprof:start-profiling)
;; (sb-sprof:reset)
;; (sb-sprof:report :type :flat)
;; (sb-sprof:report)
;; (sb-sprof:stop-profiling)

(defun init ()
  (bt:make-thread
   (lambda ()
     (swank:create-server :port 4444))))
;; (init)

(defun -loop-validate ()
  (when omcom.all:*is-production*
    (push-to-log "<b>VALIDATION.</b><hr/>")
    (push-to-log "Validating trades older than 24 hours.")
    (validate-trades)))

(defun -loop-test (instrument timeframe type-groups testing-dataset)
  (push-to-log "<b>SIGNAL.</b><hr/>")
  (push-to-log (format nil "Trying to create signal for ~a ~a." instrument timeframe))
  (if omcom.omage:*use-nested-signals-p*
      (test-most-activated-agents instrument timeframe type-groups testing-dataset :test-size omcom.omage:*test-size*)
      (test-agents instrument timeframe type-groups testing-dataset :test-size omcom.omage:*test-size*)))

;; TODO: Rename everywhere from TYPE -> STAGE where applicable (type being :creation, :training or :testing).
;; Most of the time TYPE is '(:BULLISH), for example.
;; TODO: Rename everywhere from TYPE/TYPES to PATTERN/PATTERNS.
(defun -loop-get-dataset (instrument timeframe types stage dataset)
  "Stage can be :training or :creation."
  (let ((type (first (flatten types))))
    (if-let ((begin-end (gethash (list instrument timeframe type stage) ominp.rates:*creation-training-datasets*)))
      (let ((begin-time (first begin-end))
	    (end-time (second begin-end)))
	(ominp.rates:get-rates-range-big instrument timeframe begin-time end-time))
      (multiple-value-bind (from to)
	  (get-rates-chunk-of-types dataset types
				    :slide-step (if (eq stage :training)
						    omcom.omage:*train-slide-step*
						    omcom.omage:*creation-slide-step*)
				    :min-chunk-size (if (eq stage :training)
							omcom.omage:*train-min-chunk-size*
							omcom.omage:*creation-min-chunk-size*)
				    :max-chunk-size (if (eq stage :training)
							omcom.omage:*train-max-chunk-size*
							omcom.omage:*creation-max-chunk-size*)
				    :stagnation-threshold omcom.omage:*stagnation-threshold*)
	(let ((ds (subseq dataset from to)))
	  (push-to-log (format nil "~a dataset created successfully. Size: ~s. Dataset from ~a to ~a."
			       stage
			       (length ds)
			       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first ds) :time)) 1000000))
			       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt ds) :time)) 1000000))))
	  ds)))))

(defun -loop-optimize (instrument timeframe types full-creation-dataset full-training-dataset agents-count)
  (let* ((training-dataset (-loop-get-dataset instrument timeframe types :training full-training-dataset))
	 (creation-dataset (-loop-get-dataset instrument timeframe types :creation full-creation-dataset)))
    (push-to-log "<b>OPTIMIZATION.</b><hr/>")
    (push-to-log (format nil "~a agents retrieved for pattern ~s." agents-count types))
    (optimization instrument timeframe types
		  (lambda () (let ((beliefs (gen-random-perceptions omcom.omage:*number-of-agent-inputs*)))
			       (gen-agent omcom.omage:*number-of-agent-rules*
					  instrument
					  creation-dataset
					  (assoccess beliefs :perception-fns)
					  (assoccess beliefs :lookahead-count)
					  (assoccess beliefs :lookbehind-count))))
		  training-dataset
		  omcom.omage:*seconds-to-optimize-per-pattern*)
    (push-to-log "Optimization process completed.")
    (push-to-log "Syncing agents.")
    (sync-agents instrument timeframe types)))

(defun -loop-log-testing-dataset (dataset)
  (push-to-log (format nil "Testing dataset created successfully. Size: ~s. Dataset from ~a to ~a."
		       (length dataset)
		       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
		       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000)))))

(defun -loop-get-rates (instrument timeframe)
  ;; We don't want our data feed provider to ban us.
  (sleep 1)
  (fracdiff
   (if omcom.all:*is-production*
       (get-rates-count-big instrument timeframe
			    (+ omcom.omage:*max-creation-dataset-size* omcom.omage:*max-training-dataset-size* omcom.omage:*max-testing-dataset-size*))
       (get-rates-random-count-big instrument timeframe
				   (+ omcom.omage:*max-creation-dataset-size* omcom.omage:*max-training-dataset-size* omcom.omage:*max-testing-dataset-size*)))))

(defun -loop-test-all ()
  "We run this every `omcom.omage:*seconds-interval-testing*` seconds."
  (dolist (instrument omcom.omage:*instruments*)
    (dolist (timeframe omcom.omage:*timeframes*)
      omcom.omage:*max-testing-dataset-size*
      (unless (is-market-close)
	(let ((agents-count (get-agents-count instrument timeframe omcom.omage:*type-groups*)))
	  (when (> agents-count 0)
	    (let* ((testing-dataset (get-rates-count-big instrument timeframe
							 omcom.omage:*max-testing-dataset-size*)))
	      (-loop-test instrument timeframe omcom.omage:*type-groups* testing-dataset))))
	;; We don't want our data feed provider to ban us.
	;; We also want to go easy on those database reads (`agents-count`).
	(sleep 1)))))

(defun -loop-optimize-test-validate ()
  (pmap nil (lambda (instrument)
	      (dolist (timeframe omcom.omage:*timeframes*)
		(unless (is-market-close)
		  (push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
		  (let* ((rates (-loop-get-rates instrument timeframe))
			 (dataset-size (length rates)))
		    (let ((full-training-dataset (subseq rates
							 (- dataset-size
							    omcom.omage:*max-testing-dataset-size*
							    omcom.omage:*max-training-dataset-size*)
							 (- dataset-size
							    omcom.omage:*max-testing-dataset-size*)))
			  (full-creation-dataset (subseq rates
							 0
							 (- dataset-size
							    omcom.omage:*max-testing-dataset-size*
							    omcom.omage:*max-training-dataset-size*)))
			  (testing-dataset (when (not omcom.all:*is-production*)
					     (let ((dataset (subseq rates
								    (- dataset-size
								       omcom.omage:*max-testing-dataset-size*))))
					       (-loop-log-testing-dataset dataset)
					       dataset)))
			  (agents-count (get-agents-count instrument timeframe omcom.omage:*type-groups*)))
		      ;; Optimization.
		      (loop for types in omcom.omage:*type-groups*
			    do (-loop-optimize instrument timeframe types full-creation-dataset full-training-dataset agents-count))
		      ;; Signal creation. Development.
		      (when (not omcom.all:*is-production*)
			(-loop-test instrument timeframe omcom.omage:*type-groups* testing-dataset))))
		  (-loop-validate))
		(omage.utils:refresh-memory)
		(sync-datasets-to-database)))
	omcom.omage:*instruments*)
  (unless omcom.all:*is-production*
    (wipe-agents)))

(defun loop-optimize-test ()
  (handler-bind ((error (lambda (c)
			  (log-stack c))))
    (when (is-market-close)
      (format t "~%===============================================~%MARKET IS CLOSED~%SWITCH TO DEVELOPMENT MODE IF YOU WANT TO RUN EXPERIMENTS.~%==============================================="))
    (clear-logs)
    (omage.utils:refresh-memory)
    (sync-datasets-from-database)
    ;; Signal creation. Production. We create a cron job for this to be
    ;; run every `omcom.omage:*seconds-interval-testing*` seconds.
    (when omcom.all:*is-production*
      (create-signals-job omcom.omage:*seconds-interval-testing*))
    (loop (unless (is-market-close))
	  (-loop-optimize-test-validate))))
