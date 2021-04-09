(defpackage overmind-agents.trading
  (:use #:cl #:alexandria #:postmodern #:omage.log)
  (:import-from #:ominp.db
		#:conn)
  (:import-from #:omcom.utils
		#:format-table
		#:assoccess
		#:random-float)
  (:import-from #:ominp.rates
		#:to-pips
		#:get-tp-sl
		#:from-pips
		#:get-input-dataset
		#:get-output-dataset
		#:get-tp-sl)
  (:import-from #:omage.log
		#:push-to-log)
  (:import-from #:omage.utils
		#:format-rr)
  (:import-from #:omint
		#:eval-ifis)
  (:export #:agent
	   #:log-agent
	   #:optimization
	   #:evaluate-trade
	   #:insert-trade
	   #:get-same-direction-outputs-idxs
	   #:make-ifis
	   #:evaluate-agent
	   #:evaluate-agents
	   #:test-agents
	   #:init-patterns
	   #:add-agent
	   #:sync-agents
	   #:remove-agent
	   #:eval-agent
	   #:is-agent-dominated?
	   #:get-agent
	   #:get-agents
	   #:test-most-activated-agents
	   #:gen-agent
	   #:gen-agents
	   #:update-agent-fitnesses
	   #:update-agents-fitnesses
	   #:get-agents-count
	   #:wipe-agents
	   #:get-patterns
	   #:insert-pattern
	   #:get-agent-ids-from-patterns)
  (:nicknames #:omage.trading))
(in-package :overmind-agents.trading)

(defparameter *agents-cache* (make-hash-table :test 'equal :synchronized t))

(defclass agent ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (perception-fns :col-type string :initarg :perception-fns)
   (lookahead-count :col-type integer :initarg :lookahead-count)
   (lookbehind-count :col-type integer :initarg :lookbehind-count)
   (antecedents :col-type string :initarg :antecedents)
   (consequents :col-type string :initarg :consequents)
   (creation-begin-time :col-type (or db-null int8) :initarg :creation-begin-time :initform :null)
   (creation-end-time :col-type (or db-null int8) :initarg :creation-end-time :initform :null)
   (begin-time :col-type (or db-null int8) :initarg :begin-time :initform :null)
   (end-time :col-type (or db-null int8) :initarg :end-time :initform :null)
   (dataset-size :col-type (or db-null integer) :initarg :dataset-size :initform :null)
   (avg-revenue :col-type (or db-null double-float) :initarg :avg-revenue :initform :null)
   (stdev-revenue :col-type (or db-null double-float) :initarg :stdev-revenue :initform :null)
   (total-revenue :col-type (or db-null double-float) :initarg :total-revenue :initform :null)
   (avg-return :col-type (or db-null double-float) :initarg :avg-return :initform :null)
   (total-return :col-type (or db-null double-float) :initarg :total-return :initform :null)
   (avg-max-pos :col-type (or db-null double-float) :initarg :avg-max-pos :initform :null)
   (stdev-max-pos :col-type (or db-null double-float) :initarg :stdev-max-pos :initform :null)
   (avg-max-neg :col-type (or db-null double-float) :initarg :avg-max-neg :initform :null)
   (stdev-max-neg :col-type (or db-null double-float) :initarg :stdev-max-neg :initform :null)
   (avg-tp :col-type (or db-null double-float) :initarg :avg-tp :initform :null)
   (stdev-tp :col-type (or db-null double-float) :initarg :stdev-tp :initform :null)
   (avg-sl :col-type (or db-null double-float) :initarg :avg-sl :initform :null)
   (stdev-sl :col-type (or db-null double-float) :initarg :stdev-sl :initform :null)
   (avg-activation :col-type (or db-null double-float) :initarg :avg-activation :initform :null)
   (stdev-activation :col-type (or db-null double-float) :initarg :stdev-activation :initform :null)
   (max-tp :col-type (or db-null double-float) :initarg :max-tp :initform :null)
   (min-tp :col-type (or db-null double-float) :initarg :min-tp :initform :null)
   (max-sl :col-type (or db-null double-float) :initarg :max-sl :initform :null)
   (min-sl :col-type (or db-null double-float) :initarg :min-sl :initform :null)
   (trades-won :col-type (or db-null integer) :initarg :trades-won :initform :null)
   (trades-lost :col-type (or db-null integer) :initarg :trades-lost :initform :null)
   (revenues :col-type (or db-null float[]) :initarg :revenues :initform :null)
   (entry-times :col-type (or db-null int8[]) :initarg :entry-times :initform :null)
   (exit-times :col-type (or db-null int8[]) :initarg :exit-times :initform :null)
   (entry-prices :col-type (or db-null float[]) :initarg :entry-prices :initform :null)
   (exit-prices :col-type (or db-null float[]) :initarg :exit-prices :initform :null)
   (tps :col-type (or db-null float[]) :initarg :tps :initform :null)
   (sls :col-type (or db-null float[]) :initarg :sls :initform :null)
   (activations :col-type (or db-null float[]) :initarg :activations :initform :null)
   (returns :col-type (or db-null float[]) :initarg :returns :initform :null))
  (:metaclass postmodern:dao-class)
  (:table-name agents)
  (:keys id))

(defclass agent-pattern ()
  ((agent-id :col-type string :initarg :agent-id)
   (pattern-id :col-type string :initarg :pattern-id))
  (:metaclass postmodern:dao-class)
  (:table-name agents-patterns)
  (:keys agent-id pattern-id))

(defclass pattern-trade ()
  ((pattern-id :col-type string :initarg :pattern-id)
   (trade-id :col-type string :initarg :trade-id))
  (:metaclass postmodern:dao-class)
  (:table-name patterns-trades)
  (:keys pattern-id trade-id))

(defclass pattern ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (type :col-type string :initarg :type)
   (instrument :col-type string :initarg :instrument)
   (timeframe :col-type string :initarg :timeframe))
  (:metaclass postmodern:dao-class)
  (:table-name patterns)
  (:keys id))

(defclass trade ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (agent-id :col-type (or db-null string) :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :agent-id)
   (creation-time :col-type integer :initarg :creation-time)
   (decision :col-type string :initarg :decision)
   (result :col-type (or db-null double-float) :initarg :result)
   (tp :col-type double-float :initarg :tp)
   (sl :col-type double-float :initarg :sl)
   (activation :col-type double-float :initarg :activation)
   (entry-price :col-type double-float :initarg :entry-price)
   (entry-time :col-type double-float :initarg :entry-time)
   (train-begin-time :col-type integer :initarg :train-begin-time)
   (train-end-time :col-type integer :initarg :train-end-time)
   (test-begin-time :col-type integer :initarg :test-begin-time)
   (test-end-time :col-type integer :initarg :test-end-time)
   (train-dataset-size :col-type integer :initarg :train-dataset-size)
   (test-dataset-size :col-type integer :initarg :test-dataset-size)
   (train-avg-revenue :col-type double-float :initarg :train-avg-revenue)
   (test-avg-revenue :col-type double-float :initarg :test-avg-revenue)
   (train-stdev-revenue :col-type double-float :initarg :train-stdev-revenue)
   (test-stdev-revenue :col-type double-float :initarg :test-stdev-revenue)
   (train-total-revenue :col-type double-float :initarg :train-total-revenue)
   (test-total-revenue :col-type double-float :initarg :test-total-revenue)
   (train-avg-return :col-type double-float :initarg :train-avg-return)
   (test-avg-return :col-type double-float :initarg :test-avg-return)
   (train-total-return :col-type double-float :initarg :train-total-return)
   (test-total-return :col-type double-float :initarg :test-total-return)
   (train-avg-max-pos :col-type double-float :initarg :train-avg-max-pos)
   (test-avg-max-pos :col-type double-float :initarg :test-avg-max-pos)
   (train-stdev-max-pos :col-type double-float :initarg :train-stdev-max-pos)
   (test-stdev-max-pos :col-type double-float :initarg :test-stdev-max-pos)
   (train-avg-max-neg :col-type double-float :initarg :train-avg-max-neg)
   (test-avg-max-neg :col-type double-float :initarg :test-avg-max-neg)
   (train-stdev-max-neg :col-type double-float :initarg :train-stdev-max-neg)
   (test-stdev-max-neg :col-type double-float :initarg :test-stdev-max-neg)
   (train-avg-tp :col-type double-float :initarg :train-avg-tp)
   (test-avg-tp :col-type double-float :initarg :test-avg-tp)
   (train-stdev-tp :col-type double-float :initarg :train-stdev-tp)
   (test-stdev-tp :col-type double-float :initarg :test-stdev-tp)
   (train-avg-sl :col-type double-float :initarg :train-avg-sl)
   (test-avg-sl :col-type double-float :initarg :test-avg-sl)
   (train-stdev-sl :col-type double-float :initarg :train-stdev-sl)
   (test-stdev-sl :col-type double-float :initarg :test-stdev-sl)
   (train-avg-activation :col-type double-float :initarg :train-avg-activation)
   (test-avg-activation :col-type double-float :initarg :test-avg-activation)
   (train-stdev-activation :col-type double-float :initarg :train-stdev-activation)
   (test-stdev-activation :col-type double-float :initarg :test-stdev-activation)
   (train-max-tp :col-type double-float :initarg :train-max-tp)
   (test-max-tp :col-type double-float :initarg :test-max-tp)
   (train-min-tp :col-type double-float :initarg :train-min-tp)
   (test-min-tp :col-type double-float :initarg :test-min-tp)
   (train-max-sl :col-type double-float :initarg :train-max-sl)
   (test-max-sl :col-type double-float :initarg :test-max-sl)
   (train-min-sl :col-type double-float :initarg :train-min-sl)
   (test-min-sl :col-type double-float :initarg :test-min-sl)
   (train-trades-won :col-type double-float :initarg :train-trades-won)
   (test-trades-won :col-type double-float :initarg :test-trades-won)
   (train-trades-lost :col-type double-float :initarg :train-trades-lost)
   (test-trades-lost :col-type double-float :initarg :test-trades-lost)
   (train-revenues :col-type float[] :initarg :train-revenues)
   (test-revenues :col-type float[] :initarg :test-revenues)
   (train-entry-times :col-type float[] :initarg :train-entry-times)
   (test-entry-times :col-type float[] :initarg :test-entry-times)
   (train-exit-times :col-type float[] :initarg :train-exit-times)
   (test-exit-times :col-type float[] :initarg :test-exit-times)
   (train-entry-prices :col-type float[] :initarg :train-entry-prices)
   (test-entry-prices :col-type float[] :initarg :test-entry-prices)
   (train-exit-prices :col-type float[] :initarg :train-exit-prices)
   (test-exit-prices :col-type float[] :initarg :test-exit-prices)
   (train-tps :col-type float[] :initarg :train-tps)
   (test-tps :col-type float[] :initarg :test-tps)
   (train-sls :col-type float[] :initarg :train-sls)
   (test-sls :col-type float[] :initarg :test-sls)
   (train-activations :col-type float[] :initarg :train-activations)
   (test-activations :col-type float[] :initarg :test-activations)
   (train-returns :col-type float[] :initarg :train-returns)
   (test-returns :col-type float[] :initarg :test-returns))
  (:metaclass postmodern:dao-class)
  (:table-name trades)
  (:keys id))

(defun insert-trade (agent-id instrument timeframe types train-fitnesses test-fitnesses tp sl activation rates creation-time)
  (conn (let ((patterns (get-patterns instrument timeframe types))
	      (trade (make-dao 'trade
			       :agent-id agent-id
			       :creation-time creation-time
			       :decision (if (or (= sl 0) (= tp 0))
					     "HOLD"
					     (if (> tp 0)
						 "BUY"
						 "SELL"))
			       :tp tp
			       :sl sl
			       :activation activation
			       :entry-price (if (> tp 0)
						(ominp.rates:->close-ask (last-elt rates))
						(ominp.rates:->close-bid (last-elt rates)))
			       :entry-time (/ (read-from-string (assoccess (last-elt rates) :time)) 1000000)
			       :train-begin-time (assoccess train-fitnesses :begin-time)
			       :test-begin-time (assoccess test-fitnesses :begin-time)
			       :train-end-time (assoccess train-fitnesses :end-time)
			       :test-end-time (assoccess test-fitnesses :end-time)
			       :train-dataset-size (assoccess train-fitnesses :dataset-size)
			       :test-dataset-size (assoccess test-fitnesses :dataset-size)
			       :train-avg-revenue (assoccess train-fitnesses :avg-revenue)
			       :test-avg-revenue (assoccess test-fitnesses :avg-revenue)
			       :train-stdev-revenue (assoccess train-fitnesses :stdev-revenue)
			       :test-stdev-revenue (assoccess test-fitnesses :stdev-revenue)
			       :train-total-revenue (assoccess train-fitnesses :total-revenue)
			       :test-total-revenue (assoccess test-fitnesses :total-revenue)
			       :train-avg-return (assoccess train-fitnesses :avg-return)
			       :test-avg-return (assoccess test-fitnesses :avg-return)
			       :train-total-return (assoccess train-fitnesses :total-return)
			       :test-total-return (assoccess test-fitnesses :total-return)
			       :train-avg-max-pos (assoccess train-fitnesses :avg-max-pos)
			       :test-avg-max-pos (assoccess test-fitnesses :avg-max-pos)
			       :train-stdev-max-pos (assoccess train-fitnesses :stdev-max-pos)
			       :test-stdev-max-pos (assoccess test-fitnesses :stdev-max-pos)
			       :train-avg-max-neg (assoccess train-fitnesses :avg-max-neg)
			       :test-avg-max-neg (assoccess test-fitnesses :avg-max-neg)
			       :train-stdev-max-neg (assoccess train-fitnesses :stdev-max-neg)
			       :test-stdev-max-neg (assoccess test-fitnesses :stdev-max-neg)
			       :train-avg-tp (assoccess train-fitnesses :avg-tp)
			       :test-avg-tp (assoccess test-fitnesses :avg-tp)
			       :train-stdev-tp (assoccess train-fitnesses :stdev-tp)
			       :test-stdev-tp (assoccess test-fitnesses :stdev-tp)
			       :train-avg-sl (assoccess train-fitnesses :avg-sl)
			       :test-avg-sl (assoccess test-fitnesses :avg-sl)
			       :train-stdev-sl (assoccess train-fitnesses :stdev-sl)
			       :test-stdev-sl (assoccess test-fitnesses :stdev-sl)
			       :train-avg-activation (assoccess train-fitnesses :avg-activation)
			       :test-avg-activation (assoccess test-fitnesses :avg-activation)
			       :train-stdev-activation (assoccess train-fitnesses :stdev-activation)
			       :test-stdev-activation (assoccess test-fitnesses :stdev-activation)
			       :train-max-tp (assoccess train-fitnesses :max-tp)
			       :test-max-tp (assoccess test-fitnesses :max-tp)
			       :train-min-tp (assoccess train-fitnesses :min-tp)
			       :test-min-tp (assoccess test-fitnesses :min-tp)
			       :train-max-sl (assoccess train-fitnesses :max-sl)
			       :test-max-sl (assoccess test-fitnesses :max-sl)
			       :train-min-sl (assoccess train-fitnesses :min-sl)
			       :test-min-sl (assoccess test-fitnesses :min-sl)
			       :train-trades-won (assoccess train-fitnesses :trades-won)
			       :test-trades-won (assoccess test-fitnesses :trades-won)
			       :train-trades-lost (assoccess train-fitnesses :trades-lost)
			       :test-trades-lost (assoccess test-fitnesses :trades-lost)
			       :train-revenues (apply #'vector (assoccess train-fitnesses :revenues))
			       :test-revenues (apply #'vector (assoccess test-fitnesses :revenues))
			       :train-entry-times (apply #'vector (assoccess train-fitnesses :entry-times))
			       :test-entry-times (apply #'vector (assoccess test-fitnesses :entry-times))
			       :train-exit-times (apply #'vector (assoccess train-fitnesses :exit-times))
			       :test-exit-times (apply #'vector (assoccess test-fitnesses :exit-times))
			       :train-entry-prices (apply #'vector (assoccess train-fitnesses :entry-prices))
			       :test-entry-prices (apply #'vector (assoccess test-fitnesses :entry-prices))
			       :train-exit-prices (apply #'vector (assoccess train-fitnesses :exit-prices))
			       :test-exit-prices (apply #'vector (assoccess test-fitnesses :exit-prices))
			       :train-tps (apply #'vector (assoccess train-fitnesses :tps))
			       :test-tps (apply #'vector (assoccess test-fitnesses :tps))
			       :train-sls (apply #'vector (assoccess train-fitnesses :sls))
			       :test-sls (apply #'vector (assoccess test-fitnesses :sls))
			       :train-activations (apply #'vector (assoccess train-fitnesses :activations))
			       :test-activations (apply #'vector (assoccess test-fitnesses :activations))
			       )))
	  (loop for pattern in patterns
		do (make-dao 'pattern-trade
			     :pattern-id (assoccess pattern :id)
			     :trade-id (slot-value trade 'id))))))

(defun buy-and-hold (rates)
  (- (ominp.rates:->close (last-elt rates))
     (ominp.rates:->close (first rates))))

(defun evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (if (plusp tp)
			   ;; We need to use `open` because it's when we start.
			   (ominp.rates:->open-ask (first rates))
			   (ominp.rates:->open-bid (first rates))))
	(revenue 0)
	(max-pos 0)
	(max-neg 0)
	(exit-time)
	;; Needs to be (length rates) in case the trade never finishes.
	(finish-idx (length rates)))
    (when (or (= tp 0) (= sl 0))
      ;; We move to the next price.
      (setf finish-idx 1))
    ;; We use the full `rates` dataset because we're starting at open.
    ;; We need to check the starting candle's low and high.
    (unless (or (= tp 0) (= sl 0))
      (loop for rate in rates
	    for idx from 0 below finish-idx ;; (length (rest rates))
	    do (let ((low (if (plusp tp) ;; Used to exit a trade, so buy -> bid, sell -> ask.
			      (ominp.rates:->low-bid rate)
			      (ominp.rates:->low-ask rate)))
		     (high (if (plusp tp)
			       (ominp.rates:->high-bid rate)
			       (ominp.rates:->high-ask rate)))
		     (time (assoccess rate :time)))
		 (if (> tp 0)
		     ;; Then it's bullish.
		     (if (< (- low starting-rate) sl)
			 ;; Then we lost.
			 (progn
			   (setf revenue sl)
			   (setf finish-idx idx)
			   (setf exit-time time)
			   (return))
			 (when (>= (- high starting-rate) tp)
			   ;; Then we won.
			   (setf revenue tp)
			   (setf finish-idx idx)
			   (setf exit-time time)
			   (return)))
		     ;; Then it's bearish.
		     (if (> (- high starting-rate) sl)
			 ;; Then we lost.
			 (progn
			   (setf revenue (- sl))
			   (setf finish-idx idx)
			   (setf exit-time time)
			   (return))
			 (when (<= (- low starting-rate) tp)
			   ;; Then we won.
			   (setf revenue (- tp))
			   (setf finish-idx idx)
			   (setf exit-time time)
			   (return))))
		 ;; These need to be done after determining if we won or lost.
		 ;; Otherwise, we'd be reporting max-neg or max-pos that are greater
		 ;; than the TP or SL.
		 ;; Updating max-pos.
		 (when (> (- high starting-rate) max-pos)
		   (setf max-pos (- high starting-rate)))
		 ;; Updating max-neg.
		 (when (< (- low starting-rate) max-neg)
		   (setf max-neg (- low starting-rate))))))
    `((:revenue . ,revenue)
      (:max-pos . ,max-pos)
      (:max-neg . ,max-neg)
      (:exit-time . ,exit-time)
      (:finish-idx . ,finish-idx))))
;; (evaluate-trade 0.0015 -0.0020 (get-output-dataset *rates* 3))
;; (get-tp-sl (get-output-dataset *rates* 188))

(defun test-agents (instrument timeframe types testing-dataset &key (test-size 50))
  (multiple-value-bind (tp sl activation agent-ids)
      ;; This one gets the final TP and SL.
      (eval-agents instrument timeframe types testing-dataset)
    (let* ( ;; (train-fitnesses (evaluate-agents instrument timeframe types training-dataset))
	   (test-fitnesses (evaluate-agents instrument timeframe types testing-dataset :test-size test-size)))
      ;; (when train-fitnesses
      ;; 	(push-to-log "Training process successful."))
      (when test-fitnesses
	(push-to-log "Testing process successful."))
      (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
      (when (and (/= tp 0)
		 ;; (if (not (eq instrument :USD_CNH)) (< (assoccess prediction :tp) 100) t)
		 (> (abs tp) (abs sl))
		 (/= sl 0)
		 (< (* tp sl) 0)
		 (> (abs (/ tp sl))
		    omcom.omage:*agents-min-rr-signal*)
		 (> (abs (to-pips instrument sl)) 3)
		 ;; (< (to-pips instrument (abs sl)) 20)
		 (/= (assoccess test-fitnesses :trades-won) 0)
		 (/= (+ (assoccess test-fitnesses :trades-won)
			(assoccess test-fitnesses :trades-lost))
		     0))
	(push-to-log (format nil "Trying to create trade. Agents IDs: ~a" agent-ids))
	(insert-trade (first agent-ids) instrument timeframe (first (get-agent-ids-patterns agent-ids)) test-fitnesses test-fitnesses tp sl activation testing-dataset (local-time:timestamp-to-unix (local-time:now)))
	(push-to-log "Trade created successfully.")))))

(defun get-max-lookbehind (instrument timeframe types)
  (let* ((types (flatten types)))
    (loop for agent in (get-agents instrument timeframe types) maximize (slot-value agent 'lookbehind-count))))

(defun -evaluate-agents (&key instrument timeframe types rates agent idx test-size)
  "Used for EVALUATE-AGENT and EVALUATE-AGENTS."
  (push-to-log (if agent "Trying to evaluate agent." "Trying to evaluate agents."))
  (let* ((max-lookbehind (get-max-lookbehind instrument timeframe types))
	 (idx (if idx idx max-lookbehind))
	 (rates (last rates (+ (if test-size test-size (length rates)) max-lookbehind 1)))
	 (revenues)
	 (max-poses)
	 (max-negses)
	 (trades-won 0)
	 (trades-lost 0)
	 (entry-prices)
	 (exit-prices)
	 (tps)
	 (sls)
	 (activations)
	 (entry-times)
	 (exit-times)
	 (num-datapoints 0)
	 (num-datapoints-traded 0))
    (push-to-log (if agent "Evaluating agent:" "Evaluating agents:"))
    (loop while (< idx (length rates))
	  do (let* ((input-dataset (ominp.rates:get-input-dataset rates idx))
		    (output-dataset (ominp.rates:get-output-dataset rates idx)))
	       (multiple-value-bind (tp sl activation)
		   (if agent
		       (eval-agent agent input-dataset)
		       (eval-agents instrument timeframe types input-dataset))
		 (if (< activation omcom.omage:*evaluate-agents-activation-threshold*)
		     (progn
		       (incf num-datapoints)
		       (incf idx))
		     (let* ((trade (evaluate-trade tp sl output-dataset))
			    (revenue (assoccess trade :revenue))
			    (max-pos (assoccess trade :max-pos))
			    (max-neg (assoccess trade :max-neg))
			    (exit-time (assoccess trade :exit-time))
			    (finish-idx (assoccess trade :finish-idx)))
		       (if (or (= revenue 0)
			       (> (abs sl) (abs tp))
			       (> (* tp sl) 0)
			       (< (abs (/ tp sl))
			       	  omcom.omage:*agents-min-rr-trading*)
			       (= tp 0)
			       (= sl 0)
			       (< (abs (to-pips instrument sl)) omcom.omage:*min-pips-sl*)
			       (> (abs (to-pips instrument sl)) omcom.omage:*max-pips-sl*)
			       )
			   ;; (push-to-log "." :add-newline? nil)
			   (incf num-datapoints)
			   (progn
			     (incf num-datapoints-traded)
			     (incf num-datapoints)
			     (if (> revenue 0)
				 (incf trades-won)
				 (incf trades-lost))
			     (push tp tps)
			     (push sl sls)
			     (push activation activations)
			     (push (read-from-string (assoccess (nth idx rates) :time)) entry-times)
			     (push (read-from-string exit-time) exit-times)
			     (push (if (plusp tp)
				       (ominp.rates:->close-ask (nth idx rates))
				       (ominp.rates:->close-bid (nth idx rates)))
				   entry-prices)
			     (push (if (plusp tp)
				       (ominp.rates:->close-bid (nth finish-idx output-dataset))
				       (ominp.rates:->close-ask (nth finish-idx output-dataset)))
				   exit-prices)
			     (push max-pos max-poses)
			     (push max-neg max-negses)
			     (push revenue revenues)))
		       (if omcom.omage:*trade-every-dp-p*
			   (incf idx)
			   (incf idx finish-idx))
		       ))
		 )))
    ;; ACTPROOF
    ;; (unless agent
    ;;   (setf *activations* activations)
    ;;   (setf *revenues* revenues)
    ;;   (coco))
    (push-to-log (format nil "Traded ~a out of ~a datapoints." num-datapoints-traded num-datapoints))
    (push-to-log "<br/>Agents evaluated successfully.")
    (let* ((returns (loop for revenue in revenues
    			  for tp in tps
    			  for sl in sls
    			  collect (if (or (= tp 0)
    					  (= sl 0))
    				      0
    				      (if (> revenue 0)
    					  (* (/ tp sl) -1) ;; To always get positive number.
    					  -1))))
    	   (total-return (reduce #'+ returns)))
      `((:begin-time . ,(read-from-string (assoccess (first rates) :time)))
    	(:end-time . ,(read-from-string (assoccess (last-elt rates) :time)))
    	(:dataset-size . ,(length rates))
    	(:avg-revenue . ,(if (> (length revenues) 0) (mean revenues) 0))
    	(:stdev-revenue . ,(if (> (length revenues) 0) (standard-deviation revenues) 0))
    	(:total-revenue . ,(if (> (length revenues) 0) (reduce #'+ revenues) 0))
    	(:avg-max-pos . ,(if (> (length max-poses) 0) (mean max-poses) 0))
    	(:stdev-max-pos . ,(if (> (length max-poses) 0) (standard-deviation max-poses) 0))
    	(:avg-max-neg . ,(if (> (length max-negses) 0) (mean max-negses) 0))
    	(:stdev-max-neg . ,(if (> (length max-negses) 0) (standard-deviation max-negses) 0))
    	;; (:max-max-pos . ,(if (> (length max-poses) 0) (apply #'max max-poses) 0))
    	;; (:max-max-neg . ,(if (> (length max-negses) 0) (apply #'max max-negses) 0))
    	(:avg-tp . ,(if (> (length tps) 0) (mean tps) 0))
    	(:stdev-tp . ,(if (> (length tps) 0) (standard-deviation tps) 0))
    	(:avg-sl . ,(if (> (length sls) 0) (mean sls) 0))
    	(:stdev-sl . ,(if (> (length sls) 0) (standard-deviation sls) 0))
    	(:avg-activation . ,(if (> (length activations) 0) (mean activations) 0))
    	(:stdev-activation . ,(if (> (length activations) 0) (standard-deviation activations) 0))
    	(:avg-return . ,(if (> (length tps) 0) (/ total-return (length tps)) 0))
    	(:total-return . ,total-return)
    	(:max-tp . ,(if (> (length max-negses) 0) (if (> (first tps) 0) (apply #'max tps) (apply #'min tps)) 0))
    	(:min-tp . ,(if (> (length max-negses) 0) (if (> (first tps) 0) (apply #'min tps) (apply #'max tps)) 0))
    	(:max-sl . ,(if (> (length max-negses) 0) (if (> (first sls) 0) (apply #'max sls) (apply #'min sls)) 0))
    	(:min-sl . ,(if (> (length max-negses) 0) (if (> (first sls) 0) (apply #'min sls) (apply #'max sls)) 0))
    	(:trades-won . ,trades-won)
    	(:trades-lost . ,trades-lost)
    	(:revenues . ,(reverse revenues))
    	(:entry-times . ,(reverse entry-times))
    	(:exit-times . ,(reverse exit-times))
    	(:entry-prices . ,(reverse entry-prices))
    	(:exit-prices . ,(reverse exit-prices))
    	(:tps . ,(reverse tps))
    	(:sls . ,(reverse sls))
    	(:activations . ,(reverse activations))
    	(:returns . ,(reverse returns))))))
;; (evaluate-agents :EUR_USD omcom.omage:*train-tf* '(:BULLISH) *rates*)

(defun evaluate-agent (instrument timeframe agent rates &key test-size (return-fitnesses-p nil))
  (let ((fitnesses (-evaluate-agents :instrument instrument :timeframe timeframe :agent agent :rates rates :idx (slot-value agent 'lookbehind-count) :test-size test-size)))
    (loop for fitness in fitnesses
	  ;; TODO: We should be returning symbols (not kws) from -evaluate-agents.
;;; and then remove this format + read-from-string.
	  do (setf (slot-value agent (read-from-string (format nil "overmind-agents.trading::~a" (car fitness))))
		   (if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    (if return-fitnesses-p
	fitnesses
	agent)))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(defun evaluate-agents (instrument timeframe types rates &key (test-size 50))
  (-evaluate-agents :instrument instrument :timeframe timeframe :types types :rates rates :test-size test-size))
;; (time (evaluate-agents :EUR_USD omcom.omage:*train-tf* '(:BULLISH) (subseq *rates* 0 200)))

(defun insert-pattern (instrument timeframe type)
  (conn (make-dao 'pattern :type (format nil "~a" type)
			   :instrument (format nil "~a" instrument)
			   :timeframe (format nil "~a" timeframe))))

(defun wipe-agents ()
  (conn (query (:delete-from 'agents :where (:= 1 1)))
	(query (:delete-from 'agents-patterns :where (:= 1 1)))))
;; (wipe-agents)

(defun get-patterns (instrument timeframe types)
  (let* ((types (flatten types)))
    (conn (query (:select '* :from 'patterns :where (:and (:= 'instrument (format nil "~a" instrument))
							  (:= 'timeframe (format nil "~a" timeframe))
							  (:in 'type (:set (loop for type in types collect (format nil "~a" type))))))
		 :alists))))
;; (get-patterns :EUR_JPY omcom.omage:*train-tf* '(:BULLISH (:BEARISH) :STAGNATED))

(defun get-agent-ids-from-patterns (instrument timeframe types)
  (let* ((types (flatten types))
	 (patterns (get-patterns instrument timeframe types))
	 )
    ;; (loop for agent in (get-agents instrument timeframe types)
    ;; 	  collect (slot-value agent 'id))
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (assoccess pattern :id))))) :column))))
;; (get-agent-ids-from-patterns :AUD_USD omcom.omage:*train-tf* '(:bullish))
;; (get-agent-ids-from-patterns :AUD_USD omcom.omage:*train-tf* '((:bullish) (:stagnated)))

(defun get-agents-from-cache (instrument timeframe types)
  (gethash (list instrument timeframe types) *agents-cache*))

;; (get-agents :EUR_JPY omcom.omage:*train-tf* '(:BULLISH))
;; (get-agents-from-cache :EUR_JPY omcom.omage:*train-tf* '(:BULLISH))

(defun sync-agents (instrument timeframe types)
  ;; Get agents from database (A1)
  ;; Get agents from cache (A2)
  ;; Update or add agents from A1 using A2
  ;; Delete agents found in A1 but not in A2
  (let ((A1 (get-agents instrument timeframe types))
	(A2 (get-agents-from-cache instrument timeframe types)))
    (conn
     ;; First we update existing agents on database and insert the new ones.
     ;; If the algorithm crashes, we at least keep some of the agents updated and new ones.
     (loop for agent in A2
	   do (if (get-dao 'agent (slot-value agent 'id))
		  (update-dao agent)
		  (insert-agent agent instrument timeframe types)))
     ;; Now we delete the agents that are in A1 (database) but not in A2 (cache).
     (let ((ids (mapcar (lambda (agent) (slot-value agent 'id)) A2)))
       (loop for agent in A1
	     do (let* ((id (slot-value agent 'id))
		       (foundp (find id ids :test #'string=)))
		  (unless foundp
		    (delete-dao agent))))))))
;; (time (sync-agents :AUD_USD omcom.omage:*train-tf* '(:BULLISH)))

(defun get-agents (instrument timeframe types)
  (let (result)
    (loop for type in (flatten types)
	  do (let ((type (list type)))
	       (if-let ((agents (gethash (list instrument timeframe type) *agents-cache*)))
		 (loop for agent in agents do (push agent result))
		 (let* ((agent-ids (get-agent-ids-from-patterns instrument timeframe type))
			(agents (conn (query (:select '* :from 'agents :where (:in 'id (:set agent-ids)))
					     (:dao agent)))))
		   (setf (gethash (list instrument timeframe type) *agents-cache*) agents)
		   (setf result agents)))))
    result))
;; (time (get-agents :EUR_USD omcom.omage:*train-tf* '(:bullish :stagnated)))

(defun add-agent (agent instrument timeframe types)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe types) *agents-cache*)
	(append (gethash (list instrument timeframe types) *agents-cache*)
		(list agent))))

(defun remove-agent (agent instrument timeframe types)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe types) *agents-cache*)
	(remove agent
		(gethash (list instrument timeframe types) *agents-cache*)
		:test (lambda (elt1 elt2)
			(string= (slot-value elt1 'id)
				 (slot-value elt2 'id))))))

(defun insert-agent (agent instrument timeframe types)
  "Works with database."
  (let ((agent-id (slot-value agent 'id))
	(patterns (get-patterns instrument timeframe types)))
    (conn
     (unless (get-dao 'agent agent-id)
       (insert-dao agent)
       (loop for pattern in patterns
	     do (make-dao 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id)))))
    agent))

(defun delete-agent (agent instrument timeframe types)
  "Works with database."
  (let ((agent-id (slot-value agent 'id))
	(patterns (get-patterns instrument timeframe types)))
    (conn
     ;; First we remove agent-pattern.
     (loop for pattern in patterns
	   do (delete-dao (make-instance 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id))))
     ;; Then we delete the agent only if there are no agent-patterns related to this agent.
     (unless (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id agent-id)))
       (delete-dao agent)))))

(defun get-agent-ids-patterns (agent-ids)
  "AGENT-IDS are the IDs of agents that participated in the creation of a signal. We retrieve a list of patterns associated to these AGENT-IDS."
  (loop for key being each hash-key of *agents-cache*
	for value being each hash-value of *agents-cache*
	when (find agent-ids value
		   :key (lambda (agent) (slot-value agent 'id))
		   :test (lambda (ids id) (find id ids :test #'string=)))
	  collect (car (third key))))
;; (get-agent-ids-patterns (list (slot-value (first (get-agents :AUD_USD omcom.omage:*train-tf* '(:bearish))) 'id) (slot-value (first (get-agents :AUD_USD omcom.omage:*train-tf* '(:stagnated))) 'id)))

(defun get-agents-count (instrument timeframe types)
  (length (get-agents instrument timeframe types)))

(defun eval-agent (agent rates)
  (let ((perception-fn (get-perception-fn agent)))
    (eval-ifis (funcall perception-fn rates)
	       (omage.utils:read-str (slot-value agent 'antecedents))
	       (omage.utils:read-str (slot-value agent 'consequents)))))

(defun get-perception-fn (agent)
  (omper:gen-perception-fn (omage.utils:read-str (slot-value agent 'perception-fns))))

(defun update-agent-fitnesses (instrument timeframe types agent rates)
  (let* ((agents (gethash (list instrument timeframe types) *agents-cache*))
	 (agent-idx (position agent agents :test (lambda (agent1 agent2) (string= (slot-value agent1 'id)
										  (slot-value agent2 'id))))))
    (setf (nth agent-idx agents) (evaluate-agent instrument timeframe agent rates))))

(defun update-agents-fitnesses (instrument timeframe types agents rates)
  (loop for agent in agents
	do (update-agent-fitnesses instrument timeframe types agent rates))
  agents)

(defun is-agent-dominated? (agent agents &optional (logp nil))
  (if (or (= (length (slot-value agent 'tps)) 0)
  	  (<= (slot-value agent 'total-return) 0)
  	  (< (length (slot-value agent 'tps))
  	     omcom.omage:*min-num-trades-training*))
      ;; AGENT is dominated.
      (progn
  	(when logp
  	  (log-agent :crap agent))
  	t)
      (let* ((total-return-0 (slot-value agent 'total-return))
  	     (avg-return-0 (slot-value agent 'avg-return))
  	     (activations-0 (slot-value agent 'activations))
  	     (returns-0 (slot-value agent 'returns))
  	     (entry-times-0 (slot-value agent 'entry-times)))
  	;; `data`'s going to hold the max activations, returns and entry times per DP.
  	(let ((data (make-hash-table)))
  	  ;; Determining max activations and returns.
  	  (loop for agent in agents
  		for agent-idx from 0
  		do (loop for act across (slot-value agent 'activations)
  			 for ret across (slot-value agent 'returns)
  			 for time across (slot-value agent 'entry-times)
  			 do (progn
  			      ;; Checking if internal hash-table doesn't exist.
  			      (unless (gethash time data)
  				(setf (gethash time data) (make-hash-table :size 3)))
  			      (let ((datum (gethash time data)))
  				;; Updating internal hash-table.
  				(when (or (not (gethash :activation datum))
  					  (> act (gethash :activation datum)))
  				  (setf (gethash :agent-idx datum) agent-idx)
  				  (setf (gethash :activation datum) act)
  				  ;; (setf (gethash :return datum) ret) ;; Keeping in case we want to compare against DPret.
  				  (setf (gethash :total-return datum)
  					(slot-value agent 'total-return)))))))
  	  ;; Comparing agent activations, returns, etc. to see if it doesn't get dominated.
  	  (let ((dominatedp t)
  		(dominated-idx -1))
  	    ;; First checking if our candidate AGENT wins by default because no other agent is trading at a particular DP.
  	    ;; Also checking if candidate has a positive total return and return at that DP.
  	    ;; Checking agent data against max values.
  	    (let ((foundp t))
  	      (loop for time across entry-times-0
  		    for ret across returns-0
  		    do (when (and (not (gethash time data))
  				  (> ret 0)
  				  (> avg-return-0 omcom.omage:*min-agent-avg-return*)
  				  (> total-return-0 0)
  				  )
  			 (setf foundp nil)
  			 (return)))
  	      (unless foundp
  		;; Not dominated.
  		(setf dominatedp nil)))

  	    ;; Checking if we already know that it wasn't dominated.
  	    (when dominatedp
  	      ;; Checking each DP if agent dominates at that DP.
  	      (loop for time across entry-times-0
  		    for act across activations-0
  		    for ret across returns-0
  		    do (let ((datum (gethash time data)))
  			 ;; We're going to add an agent to the agent pool that has a positive DPret (`ret`),
  			 ;; a greater total-return on that DP and a greater than or equal activation than that DP's (and threshold).
  			 (when (and datum
  				    (> ret 0)
  				    (> total-return-0 (gethash :total-return datum))
  				    (>= act
  					(gethash :activation datum)
  					omcom.omage:*evaluate-agents-activation-threshold*))
  			   ;; Storing what agent got dominated, mainly for logging purposes.
  			   (setf dominated-idx (gethash :agent-idx datum))
  			   ;; Not dominated. Returning from loop.
  			   (setf dominatedp nil)
  			   (return)))))
  	    ;; TODO: Refactor to its own function.
  	    (when logp
  	      (if (>= dominated-idx 0)
  		  ;; Logging what BETA agent our ALPHA dominated (if any).
  		  (let ((dominated-agent (nth dominated-idx agents)))
  		    (log-agent :beta dominated-agent)
  		    (log-agent :alpha agent))
  		  ;; Logging crappy agent who couldn't beat anyone.
  		  (if dominatedp
  		      (log-agent :crap agent)
  		      (log-agent :default agent))))
  	    ;; Returning result.
  	    dominatedp)))))

(defun get-agent (instrument timeframe types agent-id)
  (find agent-id (gethash (list instrument timeframe types) *agents-cache*)
	:key (lambda (agent) (slot-value agent 'id))
	:test #'string=))
;; (get-agent :EUR_USD omcom.omage:*train-tf* '(:BULLISH) "48F3970F-36C1-4A49-9E54-95746CFEA9FE")
;; (slot-value (first (get-agents :EUR_USD omcom.omage:*train-tf* '(:BULLISH))) 'id)

(defun eval-agents (instrument timeframe types rates)
  (let (tps sls activations ids)
    (let ((agents (get-agents instrument timeframe types)))
      (loop for agent in agents
	    do (multiple-value-bind (tp sl activation)
		   (eval-agent agent rates)
		 (let* ((last-rate (last-elt rates))
			;; Checking if calculated SL is greater than Nx the current spread.
			;; If not, we set Nx the current spread as the SL.
			(corrected-sl (let ((nx-spread (* omcom.omage:*min-n-times-spread-sl*
							  (abs (- (ominp.rates:->close-bid last-rate)
								  (ominp.rates:->close-ask last-rate))))))
					(if (>= (abs sl) nx-spread)
					    sl
					    (if (plusp sl)
						nx-spread
						(* -1 nx-spread))))))
		   (push tp tps)
		   (push corrected-sl sls)
		   (push activation activations)
		   (push (slot-value agent 'id) ids))
		 )))
    ;; (format t "~a, ~a~%" (apply #'min activations) (apply #'max activations))
    (let ((idxs (omage.utils:sorted-indexes activations #'>))
	  (tp 0)
	  (sl 0)
	  (dir 0)
	  (activation 0)
	  ;; (consensus t)
	  (len (min omcom.omage:*consensus-threshold* (length activations)))
	  )
      (setf tp (nth (position 0 idxs) tps))
      (setf sl (nth (position 0 idxs) sls))
      (setf activation (nth (position 0 idxs) activations))

      ;; Majority of agents must agree on direction.
      ;; (when (< (length (loop for idx from 1 below len
      ;; 			     collect (let* ((pos (position idx idxs))
      ;; 					    (nth-tp (nth pos tps)))
      ;; 				       (> (* nth-tp tp) 0))))
      ;; 	       (/ (1+ len) 2))
      ;; 	(setf tp 0)
      ;; 	(setf sl 0)
      ;; 	(setf activation 0))

      

      ;; Using activation as weight to determine direction.
      ;; (loop for idx from 0 below (length activations)
      ;; 	    do (let* ((nth-tp (nth idx tps))
      ;; 		      (nth-act (nth idx activations)))
      ;; 		 (incf dir (if (plusp nth-tp)
      ;; 			       nth-act
      ;; 			       (- nth-act)))))

      (loop for idx from 1 below len
      	    do (let* ((pos (position idx idxs))
      		      (nth-tp (nth pos tps))
      		      (nth-sl (nth pos sls))
		      (nth-act (nth idx activations)))

		 (incf dir (if (plusp nth-tp)
      			       nth-act
      			       (- nth-act)))

      		 ;; ;; consensus
      		 ;; (when (< (* nth-tp tp) 0)
      		 ;;   (setf consensus nil)
      		 ;;   (return))

      		 ;; (when (< (* nth-tp tp) 0)
      		 ;;   (setf tp 0)
      		 ;;   (setf sl 0)
      		 ;;   (return))
		 
      		 ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      		 ;; 	(setf tp nth-tp))
      		 ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      		 ;; 	(setf sl nth-sl))
      		 ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      		 ;; 	(setf tp nth-tp))
      		 ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      		 ;; 	(setf sl nth-sl))
      		 (incf tp nth-tp)
      		 (incf sl nth-sl)
      		 ))
      ;; (push consensus *consensus*)
      (values (/ tp 1)
      	      (/ sl 1)
      	      activation
      	      (list (nth (position 0 idxs) ids)))

      ;; (values
      ;;  (/ (* (abs tp) (if (/= dir 0) (/ dir (abs dir)) 0)) len)
      ;;  (/ (* (abs sl) (if (/= dir 0) (- (/ dir (abs dir))) 0)) len)
      ;;  activation
      ;;  (list (nth (position 0 idxs) ids))
      ;;  )
      )))

(defun get-most-activated-agents (instrument timeframe types &optional (n 10))
  ;; We're ordering the agents in ascending order to avoid
  ;; reversing them after `push`ing them to `bullish-agents` and `bearish-agents`.
  (let ((agents (sort (get-agents instrument timeframe types) #'< :key (lambda (agent) (slot-value agent 'avg-activation))))
	(bullish-agents)
	(bearish-agents))
    (loop for agent in agents
	  ;; Checking if bullish or bearish agent.
	  do (if (plusp (slot-value agent 'avg-tp))
		 (push agent bullish-agents)
		 (push agent bearish-agents)))
    (values
     ;; n bullish.
     (loop for agent in bullish-agents
	   for i from 0 below n
	   collect agent)
     (loop for agent in bearish-agents
	   for i from 0 below n
	   collect agent))))
;; (get-most-activated-agents :AUD_USD :H1 '(:BULLISH :BEARISH) 10)

(defun trade-most-activated-agents (instrument timeframe types agents testing-dataset creation-time &key (test-size 50))
  "Used in `test-most-activated-agents`."
  (loop for agent in agents
	do (let ((test-fitnesses (evaluate-agent instrument timeframe agent testing-dataset :test-size test-size :return-fitnesses-p t))
		 (agent-id (slot-value agent 'id)))
	     (when test-fitnesses
	       (push-to-log "Testing process successful."))
	     (multiple-value-bind (tp sl activation)
		 (eval-agent agent testing-dataset)
	       (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
	       (when (and (/= tp 0)
			  ;; (if (not (eq instrument :USD_CNH)) (< (assoccess prediction :tp) 100) t)
			  (> (abs tp) (abs sl))
			  (/= sl 0)
			  (< (* tp sl) 0)
			  (> (abs (/ tp sl))
			     omcom.omage:*agents-min-rr-signal*)
			  (> (abs (to-pips instrument sl)) 3)
			  ;; (< (to-pips instrument (abs sl)) 20)
			  (/= (assoccess test-fitnesses :trades-won) 0)
			  (/= (+ (assoccess test-fitnesses :trades-won)
				 (assoccess test-fitnesses :trades-lost))
			      0))
		 (push-to-log (format nil "Trying to create trade. Agents ID: ~a" agent-id))
		 (insert-trade agent-id instrument timeframe types test-fitnesses test-fitnesses tp sl activation testing-dataset creation-time)
		 (push-to-log "Trade created successfully."))))))

(defun test-most-activated-agents (instrument timeframe types testing-dataset &key (test-size 50))
  (let ((creation-time (local-time:timestamp-to-unix (local-time:now))))
    (multiple-value-bind (bullish-agents bearish-agents)
	(get-most-activated-agents instrument timeframe types)
      (trade-most-activated-agents instrument timeframe '(:BULLISH) bullish-agents testing-dataset creation-time :test-size test-size)
      (trade-most-activated-agents instrument timeframe '(:BEARISH) bearish-agents testing-dataset creation-time :test-size test-size))))

(defun -init-patterns (instrument timeframe)
  (unless (get-patterns instrument timeframe '(:BULLISH))
    (insert-pattern instrument timeframe :BULLISH))
  (unless (get-patterns instrument timeframe '(:BEARISH))
    (insert-pattern instrument timeframe :BEARISH))
  (unless (get-patterns instrument timeframe '(:STAGNATED))
    (insert-pattern instrument timeframe :STAGNATED)))

(defun init-patterns ()
  (loop for instrument in omcom.omage:*instruments*
	do (loop for timeframe in omcom.omage:*all-timeframes*
		 do (-init-patterns instrument timeframe))))
;; (init-patterns)

(defun gen-agent (num-rules instrument rates perception-fns lookahead-count lookbehind-count)
  (let ((agent (make-instance 'agent)))
    (setf (slot-value agent 'creation-begin-time) (read-from-string (assoccess (first rates) :time)))
    (setf (slot-value agent 'creation-end-time) (read-from-string (assoccess (last-elt rates) :time)))
    (setf (slot-value agent 'perception-fns) (format nil "~s" perception-fns))
    (setf (slot-value agent 'lookahead-count) lookahead-count)
    (setf (slot-value agent 'lookbehind-count) lookbehind-count)
    (multiple-value-bind (antecedents consequents)
    	(make-ifis agent num-rules instrument rates)
      (setf (slot-value agent 'antecedents) (format nil "~s" antecedents))
      (setf (slot-value agent 'consequents) (format nil "~s" consequents)))
    agent))
;; (gen-agent 3 *rates* (gen-random-perceptions 2) 10 55)

(defun gen-agents (num-agents num-rules instrument rates perception-fns lookahead-count lookbehind-count)
  (loop repeat num-agents collect (gen-agent instrument num-rules rates perception-fns lookahead-count lookbehind-count)))
;; (gen-agents 2 3 *rates* (assoccess *beliefs* :perception-fns) 10 55)

(defun get-same-direction-outputs-idxs (instrument rates count &key (lookahead-count 10) (lookbehind-count 10) direction-fn)
  (let* ((r (random-float 0 1))
	 (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
	 (opposite-pred (if (> r 0.5) #'minusp #'plusp))
	 (idxs (shuffle (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count)))
	 (result))
    (loop for idx in idxs
	  do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
	       (when (and (< (length result) count)
			  (funcall pred (assoccess tp-sl :tp))
			  (/= (assoccess tp-sl :sl) 0)
			  (> (abs (assoccess tp-sl :sl)) (from-pips instrument omcom.omage:*min-sl*))
			  (> (abs (/ (assoccess tp-sl :tp)
				     (assoccess tp-sl :sl)))
			     omcom.omage:*agents-min-rr-creation*)
			  (or (eq instrument :USD_CNH)
			      (< (abs (assoccess tp-sl :tp)) (from-pips instrument omcom.omage:*max-tp*))))
		 (push idx result))))
    (if (> (length result) 1)
	result
	(get-same-direction-outputs-idxs instrument rates count
					 :lookahead-count lookahead-count
					 :lookbehind-count lookbehind-count
					 :direction-fn opposite-pred))))
;; (get-same-direction-outputs-idxs *rates* :lookahead-count 5)

(defun make-ifis (agent num-rules instrument rates)
  "Analytical version."
  (let* ((perception-fn (get-perception-fn agent))
	 (lookahead-count (slot-value agent 'lookahead-count))
	 (lookbehind-count (slot-value agent 'lookbehind-count))
	 (idxs (sort (remove-duplicates (get-same-direction-outputs-idxs instrument rates num-rules :lookahead-count lookahead-count :lookbehind-count lookbehind-count)) #'<))
	 (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
	 (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count))))
    
    (values
     (let* ((v (loop
		 for inputs in (apply #'mapcar #'list chosen-inputs)
		 for idx from 0
		 collect (let* ((min-input (apply #'min inputs))
				(max-input (apply #'max inputs))
				(v (flatten (loop
					      for input in inputs
					      collect (list
						       (vector min-input input)
						       (vector max-input input)
						       )))))
			   (make-array (length v) :initial-contents v)))))
       (make-array (length v) :initial-contents v))
     (let* ((one-set-outputs
	      (flatten (loop for output in chosen-outputs
			     collect (let* ((tp (assoccess output :tp))
					    (sl (assoccess output :sl))
					    ;; (mn-tp (- tp tp-sd))
					    ;; (mx-tp (+ tp tp-sd))
					    ;; (mn-sl (- sl sl-sd))
					    ;; (mx-sl (+ sl sl-sd))
					    )
				       ;; Consequent creation.
				       (list (vector (vector 0 tp)
						     (vector (* omcom.omage:*n-times-sl-for-max-sl* sl) sl))
					     ;; We need to repeat the consequent for the "other side" of the triangle.
					     (vector (vector 0 tp)
						     (vector (* omcom.omage:*n-times-sl-for-max-sl* sl) sl))
					     )))))
	    (one-set-outputs-v (make-array (length one-set-outputs) :initial-contents one-set-outputs))
	    (v (loop repeat (length (first chosen-inputs))
		     collect one-set-outputs-v)))
       (make-array (length v) :initial-contents v)))))

(defun log-agent (type agent)
  (let ((agent-id (slot-value agent 'id))
	(avg-revenue (slot-value agent 'avg-revenue))
	(trades-won (slot-value agent 'trades-won))
	(trades-lost (slot-value agent 'trades-lost))
	(avg-return (slot-value agent 'avg-return))
	(total-return (slot-value agent 'total-return))
	(avg-sl (slot-value agent 'avg-sl))
	(avg-tp (slot-value agent 'avg-tp))
	(metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST" "AVG-RETURN" "TOTAL-RETURN" "AVG-RR" "DIRECTION")))
    (with-open-stream (s (make-string-output-stream))
      (format s "<pre><b>(~a) </b>Agent ID ~a~%" type agent-id)
      (format-table s `((,(format nil "~6$" avg-revenue)
			 ,trades-won
			 ,trades-lost
			 ,(format nil "~2$" avg-return)
			 ,(format nil "~2$" total-return)
			 ,(format-rr avg-sl avg-tp)
			 ,(if (plusp avg-tp) "BULL" "BEAR")))
		    :column-label metric-labels)
      (format s "</pre><hr/>")
      (push-to-agents-log (get-output-stream-string s))))
  ;; Don't return anything.
  (values))

(defun optimization (instrument timeframe types gen-agent-fn rates seconds)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe types)))
		  (update-agents-fitnesses instrument timeframe types agents rates)
		  (loop repeat omcom.omage:*initial-agent-count*
			collect (evaluate-agent instrument timeframe (funcall gen-agent-fn) rates))))
	(purged-agents))
    (push-to-log (format nil "~a agents retrieved to start optimization." (length agents)))
    (push-to-log (format nil "Performing optimization for ~a seconds." seconds))
    (push-to-agents-log (format nil "<h4>~a (~a, ~a)</h4>~%" instrument (car types) (length agents)))
    (loop with first-iteration-p = t
	  with until-timestamp = (local-time:timestamp+ (local-time:now) seconds :sec)
	  do (if (and (not first-iteration-p)
		      (local-time:timestamp> (local-time:now) until-timestamp))
		 (progn
		   ;; Inserting new agents in Pareto Frontier.
		   (push-to-log (format nil "Updating Pareto frontier with ~a agents." (length agents)))
		   (conn (loop for agent in agents
			       do (unless (get-agent instrument timeframe types (slot-value agent 'id))
				    (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
				    (log-agent :omega agent)
				    (add-agent agent instrument timeframe types))))
		   (push-to-log "Pareto frontier updated successfully.")
		   (return))
		 (let* ((challenger (list (evaluate-agent instrument timeframe (funcall gen-agent-fn) rates)))
			(is-dominated? (when omcom.omage:*optimize-p*
					 (is-agent-dominated? (car challenger) agents t))))
		   ;; No longer the first iteration after this.
		   (setf first-iteration-p nil)
		   ;; Logging agent direction.
		   (push-to-agent-directions-log instrument timeframe types (slot-value (first challenger) 'avg-tp))
		   (unless is-dominated?
		     ;; Purging agents.
		     (loop for in-trial in agents
			   do (if (and omcom.omage:*optimize-p*
				       (is-agent-dominated? in-trial challenger))
				  (progn
				    (push-to-log (format nil "Removing agent with ID ~a" (slot-value in-trial 'id)))
				    (push-to-agents-log (format nil "Removing agent with ID ~a" (slot-value in-trial 'id)))
				    (remove-agent in-trial instrument timeframe types))
				  (push in-trial purged-agents)))
		     (push (first challenger) purged-agents)
		     (setf agents purged-agents)
		     (setf purged-agents nil)))))))
