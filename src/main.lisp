;; (ql:quickload :hermes-agents)
;; (time (loop-optimize-test))
;; (clerk:calendar)
;; (progn (drop-database) (init-database) (init-patterns) (clear-logs) (when hscom.all:*is-production* (clear-jobs)))
;; (progn (drop-database) (init-database))

;; (ql-dist:enable (ql-dist:find-dist "ultralisp"))
;; (ql:system-apropos "genetic")

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
  (:import-from #:hscom.utils
		#:assoccess
		#:dbg)
  (:import-from #:hscom.hsage
		#:*cores-count*
		#:*iterations*
		#:*print-hypothesis-test*
		#:*test-size-human-strategies-signals*
		#:*test-size-human-strategies-metrics*)
  (:import-from #:hsper
		#:get-human-strategies)
  (:import-from #:hsage.log
		#:log-stack
		#:clear-logs)
  (:import-from #:hsage.trading
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
		#:get-agents-count
		#:wipe-agents
		#:get-agent
		#:update-agent-fitnesses
		#:update-agents-fitnesses
		#:init-patterns
		#:validate-trades
		#:get-trade-result
		#:test-human-strategy)
  (:import-from #:hscom.db
		#:conn)
  (:import-from #:hsage.db
		#:init-database
		#:drop-database)
  (:import-from #:hsinp.rates
		#:get-rates-chunk-of-types
		#:fracdiff
		#:sync-datasets-to-database
		#:sync-datasets-from-database
		#:get-rates-count-big
		#:get-rates-random-count-big)
  (:export #:loop-optimize-test)
  (:nicknames #:hsage))
(in-package :hermes-agents)

;; FARE-MEMOIZATION configuration.
(setf fare-memoization::*memoized* (make-hash-table :test #'equal :synchronized t))

;; (describe fare-memoization::*memoized*)
;; (hsage.utils:refresh-memory)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (if (or (<= *cores-count* 0)
				  (>= *cores-count* (1- (cl-cpus:get-number-of-processors))))
			      (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
				(if (/= ideal-cores-count 0)
				    ideal-cores-count 1))
			      *cores-count*)))

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
	   (rates (hsinp.rates:get-rates-range instrument timeframe from to :provider :oanda :type :fx))
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

(defun -loop-test-hybrid-strategies (&key (testingp nil))
  (let ((human-strategies (get-human-strategies))
	(test-size (if testingp
		       *test-size-human-strategies-metrics*
		       *test-size-human-strategies-signals*)))
    (dolist (human-strategy human-strategies)
      (dolist (instrument (assoccess human-strategy :instruments))
	(dolist (timeframe (assoccess human-strategy :timeframes))
	  (unless (is-market-close)
	    (let* ((human-rates (-get-rates instrument
					    timeframe
					    test-size)))
	      ;; Human strategies metrics.
	      (test-human-strategy instrument timeframe
				   ;; (assoccess human-strategy :types)
				   '((:single))
				   human-rates
				   (assoccess human-strategy :model)
				   (assoccess human-strategy :lookbehind-count)
				   :test-size test-size
				   :label (assoccess human-strategy :label)
				   :testingp testingp))
	    ;; We don't want our data feed provider to ban us.
	    ;; We also want to go easy on those database reads (`agents-count`).
	    (when hscom.all:*is-production*
	      (sleep 1))))))))

(defun -loop-test-human-strategies (&key (testingp nil))
  (let ((human-strategies (get-human-strategies))
	(test-size (if testingp
		       *test-size-human-strategies-metrics*
		       *test-size-human-strategies-signals*)))
    (dolist (human-strategy human-strategies)
      (dolist (instrument (assoccess human-strategy :instruments))
	(dolist (timeframe (assoccess human-strategy :timeframes))
	  (unless (is-market-close)
	    (let* ((human-rates (-get-rates instrument
					    timeframe
					    test-size)))
	      ;; Human strategies metrics.
	      (test-human-strategy instrument timeframe
				   ;; (assoccess human-strategy :types)
				   '((:single))
				   human-rates
				   (lambda (input-dataset)
				     (funcall (assoccess human-strategy :model)
					      input-dataset
					      (assoccess human-strategy :args-default)))
				   (assoccess human-strategy :lookbehind-count)
				   :test-size test-size
				   :label (assoccess human-strategy :label)
				   :testingp testingp))
	    ;; We don't want our data feed provider to ban us.
	    ;; We also want to go easy on those database reads (`agents-count`).
	    (when hscom.all:*is-production*
	      (sleep 1))))))))

(defun create-job-human-strategies-metrics (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  ;; Let's run it once immediately.
  (eval `(clerk:job "Creating human strategies metrics" every ,(read-from-string (format nil "~a.seconds" seconds))
		    (-loop-test-human-strategies :testingp t))))
;; (create-human-strategies-metrics-job 10)

(defun create-job-signals (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing*` seconds."
  (eval `(clerk:job "Creating signals" every ,(read-from-string (format nil "~a.seconds" seconds)) (-loop-test-all))))

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
  (when hscom.all:*is-production*
    (push-to-log "<b>VALIDATION.</b><hr/>")
    (push-to-log "Validating trades older than 24 hours.")
    (validate-trades)))

(defun -loop-test (instrument timeframe type-groups testing-dataset)
  (push-to-log "<b>SIGNAL.</b><hr/>")
  (push-to-log (format nil "Trying to create signal for ~a ~a." instrument timeframe))
  (if hscom.hsage:*use-nested-signals-p*
      (test-most-activated-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size*)
      (progn
	(let ((hscom.hsage:*consensus-threshold* 1))
	  (test-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size* :label (format nil "hermes.consensus-~a" hscom.hsage:*consensus-threshold*)))
	(when (> hscom.hsage:*consensus-threshold* 1)
	  (test-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size* :label (format nil "hermes.consensus-~a" hscom.hsage:*consensus-threshold*))))))

;; TODO: Rename everywhere from TYPE -> STAGE where applicable (type being :creation, :training or :testing).
;; Most of the time TYPE is '(:BULLISH), for example.
;; TODO: Rename everywhere from TYPE/TYPES to PATTERN/PATTERNS.
(defun -loop-get-dataset (instrument timeframe types stage dataset)
  "Stage can be :training or :creation."
  (let* ((type (first (flatten types)))
	 (begin-end (gethash (list instrument timeframe type stage) hsinp.rates:*creation-training-datasets*)))
    (cond (begin-end (let ((begin-time (first begin-end))
			   (end-time (second begin-end)))
		       (hsinp.rates:get-rates-range-big instrument timeframe begin-time end-time)))
	  ((eq type :single) dataset)
	  (t (multiple-value-bind (from to)
		 (get-rates-chunk-of-types dataset types
					   :slide-step (if (eq stage :training)
							   hscom.hsage:*train-slide-step*
							   hscom.hsage:*creation-slide-step*)
					   :min-chunk-size (if (eq stage :training)
							       hscom.hsage:*train-min-chunk-size*
							       hscom.hsage:*creation-min-chunk-size*)
					   :max-chunk-size (if (eq stage :training)
							       hscom.hsage:*train-max-chunk-size*
							       hscom.hsage:*creation-max-chunk-size*)
					   :stagnation-threshold hscom.hsage:*stagnation-threshold*)
	       (let ((ds (subseq dataset from to)))
		 (push-to-log (format nil "~a dataset created successfully. Size: ~s. Dataset from ~a to ~a."
				      stage
				      (length ds)
				      (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first ds) :time)) 1000000))
				      (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt ds) :time)) 1000000))))
		 ds))))))

(defun -loop-optimize (instrument timeframe types full-creation-dataset full-training-dataset agents-count)
  (let* ((training-dataset (-loop-get-dataset instrument timeframe types :training full-training-dataset))
	 (creation-dataset (-loop-get-dataset instrument timeframe types :creation full-creation-dataset)))
    (push-to-log "<b>OPTIMIZATION.</b><hr/>")
    (push-to-log (format nil "~a agents retrieved for pattern ~s." agents-count types))
    (let ((hscom.hsage:*consensus-threshold* 1))
      (optimization instrument timeframe types
		    (lambda () (let ((beliefs (gen-random-perceptions hscom.hsage:*number-of-agent-inputs*)))
				 (gen-agent hscom.hsage:*number-of-agent-rules*
					    instrument
					    creation-dataset
					    (assoccess beliefs :perception-fns)
					    (assoccess beliefs :lookahead-count)
					    (assoccess beliefs :lookbehind-count))))
		    training-dataset
		    (if (eq hscom.hsage:*stop-criteria* :time)
			hscom.hsage:*seconds-to-optimize-per-pattern*
			hscom.hsage:*optimization-agent-evaluations*)
		    hscom.hsage:*stop-criteria*))
    (push-to-log "Optimization process completed.")
    (push-to-log "Syncing agents.")
    (sync-agents instrument timeframe types)))

(defun -loop-log-testing-dataset (dataset)
  (push-to-log (format nil "Testing dataset created successfully. Size: ~s. Dataset from ~a to ~a."
		       (length dataset)
		       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
		       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000)))))

(defun -get-rates (instrument timeframe size)
  (if hscom.all:*is-production*
      (progn
	;; We don't want our data feed provider to ban us.
	(sleep 1)
	(get-rates-count-big instrument timeframe size))
      (get-rates-random-count-big instrument timeframe size)))

(defun -loop-get-rates (instrument timeframe)
  (let ((size (max (+ hscom.hsage:*max-creation-dataset-size*
		      hscom.hsage:*max-training-dataset-size*
		      hscom.hsage:*max-testing-dataset-size*)
		   ;; For fracdiff.
		   5000)))
    (fracdiff
     (-get-rates instrument timeframe size))))

(defun -loop-test-all ()
  "We run this every `hscom.hsage:*seconds-interval-testing*` seconds."
  (dolist (instrument hscom.hsage:*instruments*)
    (dolist (timeframe hscom.hsage:*timeframes*)
      (unless (is-market-close)
	(let ((agents-count (get-agents-count instrument timeframe hscom.hsage:*type-groups*)))
	  (when (> agents-count 0)
	    (let* ((testing-dataset (get-rates-count-big instrument timeframe
							 hscom.hsage:*max-testing-dataset-size*)))
	      (-loop-test instrument timeframe hscom.hsage:*type-groups* testing-dataset))))
	;; We don't want our data feed provider to ban us.
	;; We also want to go easy on those database reads (`agents-count`).
	(sleep 1)))))

(defun -loop-optimize-test-validate ()
  ;; Let's start by gathering the human strategies metrics when in development mode.
  (when (not hscom.all:*is-production*)
    (-loop-test-human-strategies :testingp t))
  ;; Human strategies signals. Let's evaluate human strategies first regardless of development or production mode.
  (-loop-test-human-strategies :testingp nil)
  (pmap nil (lambda (instrument)
	      (dolist (timeframe hscom.hsage:*timeframes*)
		(unless (is-market-close)
		  (push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
		  (let* ((rates (-loop-get-rates instrument timeframe))
			 (human-rates (-get-rates instrument
						  timeframe
						  *test-size-human-strategies-signals*))
			 (dataset-size (length rates)))
		    (let* ((full-training-dataset (subseq rates
							  (- dataset-size
							     hscom.hsage:*max-testing-dataset-size*
							     hscom.hsage:*max-training-dataset-size*)
							  (- dataset-size
							     hscom.hsage:*max-testing-dataset-size*)))
			   (full-creation-dataset (subseq rates
							  0
							  (- dataset-size
							     hscom.hsage:*max-testing-dataset-size*
							     hscom.hsage:*max-training-dataset-size*)))
			   (testing-dataset (when (not hscom.all:*is-production*)
					     (let ((dataset (subseq rates
								    (- dataset-size
								       hscom.hsage:*max-testing-dataset-size*))))
					       (-loop-log-testing-dataset dataset)
					       dataset)))
			   (agents-count (get-agents-count instrument timeframe hscom.hsage:*type-groups*)))
		      ;; Optimization.
		      (loop for types in hscom.hsage:*type-groups*
			    do (-loop-optimize instrument timeframe types full-creation-dataset full-training-dataset agents-count))
		      ;; Signal creation. Development.
		      (when (not hscom.all:*is-production*)
			(-loop-test instrument timeframe hscom.hsage:*type-groups* testing-dataset))
		      ))
		  (-loop-validate))
		(hsage.utils:refresh-memory)
		(sync-datasets-to-database)
		(when *print-hypothesis-test*
		  (hsage.trading::hypothesis-test))))
	hscom.hsage:*instruments*)
  (unless hscom.all:*is-production*
    (wipe-agents)))

(defun loop-optimize-test ()
  (handler-bind ((error (lambda (c)
			  (log-stack c))))
    (when (is-market-close)
      (format t "~%===============================================~%MARKET IS CLOSED~%SWITCH TO DEVELOPMENT MODE IF YOU WANT TO RUN EXPERIMENTS.~%==============================================="))
    (clear-logs)
    (hsage.utils:refresh-memory)
    (sync-datasets-from-database)
    ;; Signal creation. Production. We create a cron job for this to be
    ;; run every `hscom.hsage:*seconds-interval-testing*` seconds.
    (when hscom.all:*is-production*
      (create-job-signals hscom.hsage:*seconds-interval-testing*)
      (create-job-human-strategies-metrics hscom.hsage:*seconds-interval-testing-human-strategies-metrics*)
      (clerk:start))
    (if (< *iterations* 0)
	(loop (unless (is-market-close))
	      (-loop-optimize-test-validate))
	(loop repeat *iterations*
	      do (-loop-optimize-test-validate)))))
