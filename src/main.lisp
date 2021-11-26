;; (ql:quickload :hermes-agents)
;; (time (loop-optimize-test))
;; (ql:quickload :hu.dwim.def)
;; (clerk:calendar)
;; (progn (hsage.db:drop-database) (hsage.db:init-database) (hsage.trading:init-patterns) (hsage.log:clear-logs) (when hscom.all:*is-production* (hsage::clear-jobs)))
;; (progn (drop-database) (init-database))
;; (ql-dist:disable (ql-dist:find-dist "ultralisp"))
;; (ql-dist:enable (ql-dist:find-dist "ultralisp"))

(defpackage hermes-agents
  (:use #:cl
	#:local-time
	#:access
	#:lparallel
	#:postmodern
	#:alexandria
	#:computable-reals
	#:hu.dwim.def
	#:defenum
	#:fare-mop
	#:hermes-input
	#:hermes-perception
	#:hermes-intuition
	#:hermes-agents.db
	#:hermes-agents.km
	#:hermes-agents.utils
	#:hermes-agents.log
	#:hscom.log)
  (:import-from #:hscom.utils
		#:assoccess
		#:dbg
		#:whole-reals-to-integers)
  (:import-from #:hscom.hsage
		#:*hybrid-maximize-p*
		#:*hybrid-population-size*
		#:*hybrid-iterations*
		#:*hybrid-mutation-rate*
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
		#:update-agent-fitnesses
		#:update-agents-fitnesses
		#:init-patterns
		#:validate-trades
		#:get-trade-result
		#:test-human-strategy
		#:optimize-human-strategy
		#:get-hybrid
		#:get-human-name
		#:get-hybrid-name
		#:best-individual
		#:test-hybrid-strategy
		#:get-hybrid-id
		#:cache-agents-from-db
                #:uncache-agents-from-db)
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

(defmacro -loop-human-strategies (testingp &rest body)
  `(let ((human-strategies (get-human-strategies))
	 (dataset-size (if ,testingp
			   (+ hscom.hsage:*train-size-hybrid-strategies-metrics*
			       hscom.hsage:*test-size-hybrid-strategies-metrics*)
			   (+ hscom.hsage:*train-size-hybrid-strategies-signals*
			       hscom.hsage:*test-size-hybrid-strategies-signals*))))
     (dolist (human-strategy human-strategies)
       (dolist (instrument (assoccess human-strategy :instruments))
	 (dolist (timeframe (assoccess human-strategy :timeframes))
	   (unless (is-market-close)
	     (let* ((input-dataset (-get-rates instrument
					       timeframe
					       dataset-size)))
	       ,@body)
	     ;; We don't want our data feed provider to ban us.
	     ;; We also want to go easy on those database reads (`agents-count`).
	     (when hscom.all:*is-production*
	       (sleep 1))))))))

(defun -loop-optimize-human-strategies (&key (testingp nil))
  (-loop-human-strategies
   testingp
   (optimize-human-strategy instrument timeframe '((:single))
			    input-dataset
			    (if testingp
			        hscom.hsage:*train-size-hybrid-strategies-metrics*
				hscom.hsage:*train-size-hybrid-strategies-signals*)
			    human-strategy
			    :maximize *hybrid-maximize-p*
			    :population-size *hybrid-population-size*
			    :max-iterations *hybrid-iterations*
			    :mutation-rate *hybrid-mutation-rate*
			    :test-size dataset-size
			    :fitness-metric :avg-return)))

(defun -loop-test-human-strategies (&key (testingp nil))
  (-loop-human-strategies
   testingp
   ;; Testing hybrid strategy.
   (let ((hybrid (get-hybrid instrument timeframe (get-hybrid-name human-strategy))))
     (when hybrid
       (test-hybrid-strategy instrument timeframe
			     '((:single))
			     (get-hybrid-id hybrid)
			     (subseq input-dataset
				     (if testingp
					 hscom.hsage:*train-size-hybrid-strategies-metrics*
					 hscom.hsage:*train-size-hybrid-strategies-signals*))
			     (lambda (input-dataset)
			       (funcall (assoccess human-strategy :model)
					input-dataset
				        (whole-reals-to-integers (best-individual hybrid))))
			     (assoccess human-strategy :lookbehind-count)
			     :test-size dataset-size
			     :label (get-hybrid-name human-strategy)
			     :testingp testingp)))
   ;; Testing human strategy.
   (test-human-strategy instrument timeframe
			;; (assoccess human-strategy :types)
			'((:single))
			input-dataset
			(lambda (input-dataset)
			  (funcall (assoccess human-strategy :model)
				   input-dataset
				   (assoccess human-strategy :args-default)))
			(assoccess human-strategy :lookbehind-count)
			:test-size dataset-size
			:label (get-human-name human-strategy)
			:testingp testingp)))
;; (-loop-test-human-strategies)
;; (-loop-test-human-strategies :testingp t)

;; (conn (query (:select '* :from 'hybrids) :alists))

(defun create-job-human-strategies-metrics (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  (when hscom.hsage:*run-human-and-hybrid-p*
    ($log $trace :-> :create-job-human-strategies-metrics)
    ;; Let's run it once immediately.
    (eval `(clerk:job "Creating human strategies metrics" every ,(read-from-string (format nil "~a.seconds" seconds))
		      (-loop-test-human-strategies :testingp t)))
    ($log $trace :<- :create-job-human-strategies-metrics)))
;; (create-human-strategies-metrics-job 10)

(defun create-job-optimize-human-strategies (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  (when hscom.hsage:*run-human-and-hybrid-p*
    ($log $trace :-> :create-job-optimize-human-strategies)
    ;; Let's run it once immediately.
    (eval `(clerk:job "Optimizing human strategies to create hybrid strategies" every ,(read-from-string (format nil "~a.seconds" seconds))
		      (-loop-optimize-human-strategies :testingp t)))
    ($log $trace :<- :create-job-optimize-human-strategies)))

(defun create-job-signals (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing*` seconds."
  ($log $trace :-> :create-job-signals)
  (eval `(clerk:job "Creating signals" every ,(read-from-string (format nil "~a.seconds" seconds)) (-loop-test-all)))
  ($log $trace :<- :create-job-signals))

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
    ($log $info "Validating trades older than 24 hours.")
    (validate-trades)))

(defun -loop-test (instrument timeframe type-groups testing-dataset)
  ($log $trace :-> :-loop-test instrument timeframe)
  ($log $info "Creating signal for" instrument timeframe)
  (if hscom.hsage:*use-nested-signals-p*
      (test-most-activated-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size*)
      (progn
	(let ((hscom.hsage:*consensus-threshold* 1))
	  (test-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size* :label (format nil "hermes.consensus-~a" hscom.hsage:*consensus-threshold*)))
	(when (> hscom.hsage:*consensus-threshold* 1)
	  (test-agents instrument timeframe type-groups testing-dataset :test-size hscom.hsage:*test-size* :label (format nil "hermes.consensus-~a" hscom.hsage:*consensus-threshold*)))))
  ($log $trace :<- :-loop-test instrument timeframe))

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
  ($log $trace :-> :-loop-optimize)
  (let* ((training-dataset (-loop-get-dataset instrument timeframe types :training full-training-dataset))
	 (creation-dataset (-loop-get-dataset instrument timeframe types :creation full-creation-dataset)))
    ($log $info (format nil "~a agents retrieved for pattern ~s." agents-count (list instrument timeframe types)))
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
    ($log $info "Optimization process completed.")
    (sync-agents instrument timeframe types)
    ($log $trace :<- :-loop-optimize)))

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
        (cache-agents-from-db instrument timeframe)
	(let ((agents-count (get-agents-count instrument timeframe hscom.hsage:*type-groups*)))
	  (when (> agents-count 0)
	    (let* ((testing-dataset (get-rates-count-big instrument timeframe
							 hscom.hsage:*max-testing-dataset-size*)))
	      (-loop-test instrument timeframe hscom.hsage:*type-groups* testing-dataset))))
	;; We don't want our data feed provider to ban us.
	;; We also want to go easy on those database reads (`agents-count`).
        (uncache-agents-from-db instrument timeframe)
	(sleep 1)))))

(defun -loop-optimize-test-validate ()
  ;; Let's start by gathering the human strategies metrics when in development mode.
  ($log $trace :-> :-loop-optimize-test-validate)
  (when (and (not hscom.all:*is-production*)
	     hscom.hsage:*run-human-and-hybrid-p*)
    (-loop-optimize-human-strategies :testingp t)
    (-loop-test-human-strategies :testingp t))
  ;; Human strategies signals. Let's evaluate human strategies first regardless of development or production mode.
  (when hscom.hsage:*run-human-and-hybrid-p*
    (-loop-test-human-strategies :testingp nil))
  (pmap nil (lambda (instrument)
	      (dolist (timeframe hscom.hsage:*timeframes*)
		(unless (is-market-close)
                  ($log $info "Caching agents for pattern" (list instrument timeframe '(:single)))
                  (cache-agents-from-db instrument timeframe t)
                  ($log $info "Retrieving datasets for agents.")
		  (let* ((rates (-loop-get-rates instrument timeframe))
			 ;; (human-rates (-get-rates instrument
			 ;; 			  timeframe
			 ;; 			  *test-size-human-strategies-signals*))
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
		      ($log $info "Beginning agent optimization process.")
		      ;; Optimization.
		      (loop for types in hscom.hsage:*type-groups*
			    do (-loop-optimize instrument timeframe types full-creation-dataset full-training-dataset agents-count))
		      ;; Signal creation. Development.
		      (when (not hscom.all:*is-production*)
			(-loop-test instrument timeframe hscom.hsage:*type-groups* testing-dataset))
		      ($log $info "Done agent optimization process.")
		      ))
		  (-loop-validate)
                  (uncache-agents-from-db instrument timeframe t))
		(sync-datasets-to-database)
		(when *print-hypothesis-test*
		  (hsage.trading::hypothesis-test))))
	hscom.hsage:*instruments*)
  (unless hscom.all:*is-production*
    (wipe-agents))
  ($log $trace :<- :-loop-optimize-test-validate))

(def (function d) loop-optimize-test ()
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
      (create-job-optimize-human-strategies hscom.hsage:*seconds-interval-optimizing-human-strategies*)
      (clerk:start))
    (if (< *iterations* 0)
	(loop (unless (is-market-close))
	      (-loop-optimize-test-validate))
	(loop repeat *iterations*
	      do (-loop-optimize-test-validate)))))
