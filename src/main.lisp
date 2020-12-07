;; (ql:quickload :overmind-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (loop-optimize-test)

(defpackage overmind-agents
  (:use #:cl
	#:local-time
	#:access
	#:lparallel
	#:postmodern
	#:alexandria
	#:computable-reals
	#:random-state
	#:defenum
	#:fare-mop
	#:overmind-code
	#:overmind-input
	#:overmind-perception
	#:overmind-intuition
	#:overmind-agents.db
	#:overmind-agents.km
	#:overmind-agents.utilities)
  (:export #:get-trades
	   #:loop-optimize-test
	   #:read-log
	   #:read-agents-log
	   #:unix-to-nano
	   #:unix-from-nano
	   #:get-rates-range-big
	   #:get-rates-count-big
	   #:get-all-agents)
  (:nicknames #:omage))
(in-package :overmind-agents)

(defparameter *rand-gen* (make-generator :mersenne-twister-32))
(defparameter *agents-cache* (make-hash-table :test 'equal))
(defparameter +CELL-FORMATS+ '(:left   "~vA"
			       :center "~v:@<~A~>"
			       :right  "~v@A"))

(setf lparallel:*kernel* (lparallel:make-kernel
			  (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
			    (if (/= ideal-cores-count 0)
				ideal-cores-count 1))))

(defun log-stack (c)
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

(defmacro comment (&rest body))

(defun assoccess (o k)
  (cdr (assoc k o)))

(defun get-input-dataset (rates idx)
  (subseq rates 0 idx))

(defun get-output-dataset (rates idx)
  (subseq rates idx))

(defun unix-from-nano (unix-nano &optional (is-string? nil))
  (if is-string?
      (/ (read-from-string unix-nano) 1000000)
      (/ unix-nano 1000000)))

(defun unix-to-nano (unix &optional (is-string? nil))
  (if is-string?
      (* (read-from-string unix) 1000000)
      (* unix 1000000)))

(defun rate-close (rate)
  (/ (+ (assoccess rate :close-bid)
	(assoccess rate :close-ask))
     2))

(defun rate-high (rate)
  (/ (+ (assoccess rate :high-bid)
	(assoccess rate :high-ask))
     2))

(defun rate-low (rate)
  (/ (+ (assoccess rate :low-bid)
	(assoccess rate :low-ask))
     2))

(defun rate-open (rate)
  (/ (+ (assoccess rate :open-bid)
	(assoccess rate :open-ask))
     2))

(defun rate-close-bid (rate)
  (assoccess rate :close-bid))

(defun rate-close-ask (rate)
  (assoccess rate :close-bid))

(defun rate-low-bid (rate)
  (assoccess rate :low-bid))

(defun rate-low-ask (rate)
  (assoccess rate :low-ask))

(defun rate-high-bid (rate)
  (assoccess rate :high-bid))

(defun rate-high-ask (rate)
  (assoccess rate :high-ask))

(defun rate-open-bid (rate)
  (assoccess rate :open-bid))

(defun rate-open-ask (rate)
  (assoccess rate :open-ask))

(defmacro conn (&rest body)
  `(with-connection (list ,*db-name* ,*db-user* ,*db-pass* ,*db-hostname*)
     ,@body))
;; (conn (get-dao 'pattern :instrument "EUR_USD" :timeframe "H1" :type "BULLISH"))

(defun drop-database ()
  (conn
   (drop-table 'agents)
   (drop-table 'agents-patterns)
   (drop-table 'patterns)
   (drop-table 'patterns-trades)
   (drop-table 'trades)))
;; (drop-database)
;; (progn (drop-database) (init-database) (init-patterns) (clear-log))

(defun init-patterns ()
  (unless (get-patterns :AUD_USD :H1 '(:BULLISH))
    (insert-pattern :AUD_USD :H1 :BULLISH))
  (unless (get-patterns :AUD_USD :H1 '(:BEARISH))
    (insert-pattern :AUD_USD :H1 :BEARISH))
  (unless (get-patterns :AUD_USD :H1 '(:STAGNATED))
    (insert-pattern :AUD_USD :H1 :STAGNATED))
  
  (unless (get-patterns :EUR_GBP :H1 '(:BULLISH))
    (insert-pattern :EUR_GBP :H1 :BULLISH))
  (unless (get-patterns :EUR_GBP :H1 '(:BEARISH))
    (insert-pattern :EUR_GBP :H1 :BEARISH))
  (unless (get-patterns :EUR_GBP :H1 '(:STAGNATED))
    (insert-pattern :EUR_GBP :H1 :STAGNATED))

  (unless (get-patterns :EUR_JPY :H1 '(:BULLISH))
    (insert-pattern :EUR_JPY :H1 :BULLISH))
  (unless (get-patterns :EUR_JPY :H1 '(:BEARISH))
    (insert-pattern :EUR_JPY :H1 :BEARISH))
  (unless (get-patterns :EUR_JPY :H1 '(:STAGNATED))
    (insert-pattern :EUR_JPY :H1 :STAGNATED))
  
  (unless (get-patterns :EUR_USD :H1 '(:BULLISH))
    (insert-pattern :EUR_USD :H1 :BULLISH))
  (unless (get-patterns :EUR_USD :H1 '(:BEARISH))
    (insert-pattern :EUR_USD :H1 :BEARISH))
  (unless (get-patterns :EUR_USD :H1 '(:STAGNATED))
    (insert-pattern :EUR_USD :H1 :STAGNATED))

  (unless (get-patterns :GBP_USD :H1 '(:BULLISH))
    (insert-pattern :GBP_USD :H1 :BULLISH))
  (unless (get-patterns :GBP_USD :H1 '(:BEARISH))
    (insert-pattern :GBP_USD :H1 :BEARISH))
  (unless (get-patterns :GBP_USD :H1 '(:STAGNATED))
    (insert-pattern :GBP_USD :H1 :STAGNATED))

  (unless (get-patterns :USD_CAD :H1 '(:BULLISH))
    (insert-pattern :USD_CAD :H1 :BULLISH))
  (unless (get-patterns :USD_CAD :H1 '(:BEARISH))
    (insert-pattern :USD_CAD :H1 :BEARISH))
  (unless (get-patterns :USD_CAD :H1 '(:STAGNATED))
    (insert-pattern :USD_CAD :H1 :STAGNATED))

  (unless (get-patterns :USD_CHF :H1 '(:BULLISH))
    (insert-pattern :USD_CHF :H1 :BULLISH))
  (unless (get-patterns :USD_CHF :H1 '(:BEARISH))
    (insert-pattern :USD_CHF :H1 :BEARISH))
  (unless (get-patterns :USD_CHF :H1 '(:STAGNATED))
    (insert-pattern :USD_CHF :H1 :STAGNATED))

  (unless (get-patterns :USD_CNH :H1 '(:BULLISH))
    (insert-pattern :USD_CNH :H1 :BULLISH))
  (unless (get-patterns :USD_CNH :H1 '(:BEARISH))
    (insert-pattern :USD_CNH :H1 :BEARISH))
  (unless (get-patterns :USD_CNH :H1 '(:STAGNATED))
    (insert-pattern :USD_CNH :H1 :STAGNATED))

  (unless (get-patterns :USD_JPY :H1 '(:BULLISH))
    (insert-pattern :USD_JPY :H1 :BULLISH))
  (unless (get-patterns :USD_JPY :H1 '(:BEARISH))
    (insert-pattern :USD_JPY :H1 :BEARISH))
  (unless (get-patterns :USD_JPY :H1 '(:STAGNATED))
    (insert-pattern :USD_JPY :H1 :STAGNATED)))
;; (init-patterns)

(defun insert-rates (instrument timeframe rates)
  (conn
   (loop for rate in rates
	 ;; Inserting only if complete.
	 do (when (assoccess rate :complete)
	      (let ((time (assoccess rate :time))
		    (instrument (format nil "~a" instrument))
		    (timeframe (format nil "~a" timeframe)))
		(unless (get-dao 'rate time instrument timeframe)
		  (make-dao 'rate
			    :time time
			    :instrument instrument
			    :timeframe timeframe
			    :open-bid (assoccess rate :open-bid)
			    :open-ask (assoccess rate :open-ask)
			    :high-bid (assoccess rate :high-bid)
			    :high-ask (assoccess rate :high-ask)
			    :low-bid (assoccess rate :low-bid)
			    :low-ask (assoccess rate :low-ask)
			    :close-bid (assoccess rate :close-bid)
			    :close-ask (assoccess rate :close-ask)
			    :volume (assoccess rate :volume)
			    )))))))

(defun init-rates (howmany-batches)
  (let ((instruments ominp:*forex*)
	(timeframes '(:H1 :M1)))
    (loop for instrument in instruments
	  do (loop for timeframe in timeframes
		   do (let ((rates (get-rates-batches instrument timeframe howmany-batches)))
			(insert-rates instrument timeframe rates))))))
;; (init-rates 1)

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (conn
   (unless (table-exists-p 'rates)
     (query (:create-table 'rates
		((time :type string)
		 (instrument :type string)
		 (timeframe :type string)
		 (open-bid :type double-float)
		 (open-ask :type double-float)
		 (high-bid :type double-float)
		 (high-ask :type double-float)
		 (low-bid :type double-float)
		 (low-ask :type double-float)
		 (close-bid :type double-float)
		 (close-ask :type double-float)
		 (volume :type int))
	      (:primary-key time instrument timeframe))))
   (unless (table-exists-p 'agents)
     (query (:create-table 'agents
		((id :type string)
		 (perception-fns :type string)
		 (lookahead-count :type integer)
		 (lookbehind-count :type integer)
		 (antecedents :type string)
		 (consequents :type string)
		 (creation-begin-time :type (or db-null int8))
		 (creation-end-time :type (or db-null int8))
		 (begin-time :type (or db-null int8))
		 (end-time :type (or db-null int8))
		 (dataset-size :type (or db-null integer))
		 (avg-revenue :type (or db-null double-float))
		 (stdev-revenue :type (or db-null double-float))
		 (total-revenue :type (or db-null double-float))
		 (avg-return :type (or db-null double-float))
		 (total-return :type (or db-null double-float))
		 (avg-max-pos :type (or db-null double-float))
		 (stdev-max-pos :type (or db-null double-float))
		 (avg-max-neg :type (or db-null double-float))
		 (stdev-max-neg :type (or db-null double-float))
		 (avg-tp :type (or db-null double-float))
		 (stdev-tp :type (or db-null double-float))
		 (avg-sl :type (or db-null double-float))
		 (stdev-sl :type (or db-null double-float))
		 (avg-activation :type (or db-null double-float))
		 (stdev-activation :type (or db-null double-float))
		 (max-tp :type (or db-null double-float))
		 (min-tp :type (or db-null double-float))
		 (max-sl :type (or db-null double-float))
		 (min-sl :type (or db-null double-float))
		 (trades-won :type (or db-null integer))
		 (trades-lost :type (or db-null integer))
		 (revenues :type (or db-null float[]))
		 (entry-times :type (or db-null int8[]))
		 (exit-times :type (or db-null int8[]))
		 (entry-prices :type (or db-null float[]))
		 (exit-prices :type (or db-null float[]))
		 (tps :type (or db-null float[]))
		 (sls :type (or db-null float[]))
		 (activations :type (or db-null float[]))
		 (returns :type (or db-null float[])))
	      (:primary-key id))))
   (unless (table-exists-p 'agents-patterns)
     (query (:create-table 'agents-patterns
		((agent-id :type string)
		 (pattern-id :type string))
	      (:primary-key agent-id pattern-id))))
   (unless (table-exists-p 'patterns)
     (query (:create-table 'patterns
		((id :type string)
		 (type :type string)
		 (instrument :type string)
		 (timeframe :type string))
	      (:primary-key id)
	      (:unique type instrument timeframe))))
   (unless (table-exists-p 'patterns-trades)
     (query (:create-table 'patterns-trades
		((pattern-id :type string)
		 (trade-id :type string))
	      (:primary-key pattern-id trade-id))))
   (unless (table-exists-p 'trades)
     (query (:create-table 'trades
		((id :type string)
		 (agent-id :type (or db-null string))
		 (creation-time :type integer)
		 (decision :type string)
		 (result :type (or db-null double-float))
		 (tp :type double-float)
		 (sl :type double-float)
		 (entry-price :type double-float)
		 (entry-time :type (or db-null integer))
		 (train-begin-time :type int8)
		 (train-end-time :type int8)
		 (test-begin-time :type int8)
		 (test-end-time :type int8)
		 (train-dataset-size :type integer)
		 (test-dataset-size :type integer)
		 (train-avg-revenue :type double-float)
		 (test-avg-revenue :type double-float)
		 (train-avg-return :type (or db-null double-float))
		 (test-avg-return :type (or db-null double-float))
		 (train-total-return :type (or db-null double-float))
		 (test-total-return :type (or db-null double-float))
		 (train-stdev-revenue :type double-float)
		 (test-stdev-revenue :type double-float)
		 (train-total-revenue :type double-float)
		 (test-total-revenue :type double-float)
		 (train-avg-max-pos :type double-float)
		 (test-avg-max-pos :type double-float)
		 (train-stdev-max-pos :type double-float)
		 (test-stdev-max-pos :type double-float)
		 (train-avg-max-neg :type double-float)
		 (test-avg-max-neg :type double-float)
		 (train-stdev-max-neg :type double-float)
		 (test-stdev-max-neg :type double-float)
		 (train-avg-tp :type double-float)
		 (test-avg-tp :type double-float)
		 (train-stdev-tp :type double-float)
		 (test-stdev-tp :type double-float)
		 (train-avg-sl :type double-float)
		 (test-avg-sl :type double-float)
		 (train-avg-activation :type double-float)
		 (test-avg-activation :type double-float)
		 (train-stdev-activation :type double-float)
		 (test-stdev-activation :type double-float)
		 (train-stdev-sl :type double-float)
		 (test-stdev-sl :type double-float)
		 (train-max-tp :type double-float)
		 (test-max-tp :type double-float)
		 (train-min-tp :type double-float)
		 (test-min-tp :type double-float)
		 (train-max-sl :type double-float)
		 (test-max-sl :type double-float)
		 (train-min-sl :type double-float)
		 (test-min-sl :type double-float)
		 (train-trades-won :type integer)
		 (test-trades-won :type integer)
		 (train-trades-lost :type integer)
		 (test-trades-lost :type integer)
		 (train-revenues :type float[])
		 (test-revenues :type float[])
		 (train-entry-times :type int8[])
		 (test-entry-times :type int8[])
		 (train-exit-times :type int8[])
		 (test-exit-times :type int8[])
		 (train-entry-prices :type float[])
		 (test-entry-prices :type float[])
		 (train-exit-prices :type float[])
		 (test-exit-prices :type float[])
		 (train-tps :type float[])
		 (test-tps :type float[])
		 (train-sls :type float[])
		 (test-sls :type float[])
		 (train-activations :type (or db-null float[]))
		 (test-activations :type (or db-null float[]))
		 (train-returns :type (or db-null float[]))
		 (test-returns :type (or db-null float[])))
	      (:primary-key id))))))
;; (init-database)

(defun get-trade-result (entry-price tp sl rates)
  (let ((low-type (if (plusp tp) :low-bid :low-ask))
	(high-type (if (plusp tp) :high-bid :high-ask)))
    (loop for rate in rates do (progn
				 ;; Then it's a buy. Lose.
				 (when (and (> tp 0) (< (- (assoccess rate low-type) entry-price) sl))
				   (return sl))
				 ;; Then it's a sell. Lose.
				 (when (and (< tp 0) (> (- (assoccess rate high-type) entry-price) sl))
				   (return (- sl)))
				 ;; Then it's a buy. Win.
				 (when (and (> tp 0) (> (- (assoccess rate high-type) entry-price) tp))
				   (return tp))
				 ;; Then it's a sell. Win.
				 (when (and (< tp 0) (< (- (assoccess rate low-type) entry-price) tp))
				   (return (abs tp)))
				 ))))

;; (get-trade-result 0.72274 -0.0018100000000000893 0.0013900000000000026 )

(defun to-pips (instrument quantity)
  (let ((str-instrument (format nil "~a" instrument)))
    (cond ((or (cl-ppcre:scan "JPY" str-instrument)
	       (cl-ppcre:scan "HUF" str-instrument)
	       (cl-ppcre:scan "KRW" str-instrument)
	       (cl-ppcre:scan "THB" str-instrument))
	   (* quantity 100))
	  ((or (cl-ppcre:scan "CZK" str-instrument)
	       (cl-ppcre:scan "CNY" str-instrument)
	       (cl-ppcre:scan "INR" str-instrument))
	   (* quantity 1000))
	  (t (* quantity 10000)))))
;; (to-pips :USD_CZK 0.010)

(defun get-global-revenue (&key from to)
  (let ((trades (conn (if (and from to)
			  (query (:order-by (:select '*
					      :from (:as (:order-by (:select
									'trades.result
								      'trades.decision
								      'trades.creation-time
								      'patterns.*
								      :distinct-on 'trades.id
								      :from 'trades
								      :inner-join 'patterns-trades
								      :on (:= 'trades.id 'patterns-trades.trade-id)
								      :inner-join 'patterns
								      :on (:= 'patterns.id 'patterns-trades.pattern-id)
								      :where (:and (:not (:is-null 'trades.result))
										   (:>= 'creation-time from)
										   ;; (:not (:= 'patterns.instrument "USD_CNH"))
										   ;; (:not (:= 'patterns.type "STAGNATED"))
										   ;; (:= 'patterns.type "STAGNATED")
										   ;; (:= 'patterns.instrument "USD_CNH")
										   (:<= 'creation-time to)
										   ))
								    'trades.id)
							 'results))
					    (:desc 'creation-time))
				 :alists)
			  (query (:order-by (:select '*
					      :from (:as (:order-by (:select
									'trades.result
								      'trades.decision
								      'trades.creation-time
								      'patterns.*
								      :distinct-on 'trades.id
								      :from 'trades
								      :inner-join 'patterns-trades
								      :on (:= 'trades.id 'patterns-trades.trade-id)
								      :inner-join 'patterns
								      :on (:= 'patterns.id 'patterns-trades.pattern-id)
								      :where (:and (:not (:is-null 'trades.result))
										   ;; (:>= 'creation-time from)
										   ;; (:not (:= 'patterns.instrument "USD_CNH"))
										   ;; (:not (:= 'patterns.type "STAGNATED"))
										   ;; (:= 'patterns.type "STAGNATED")
										   ;; (:= 'patterns.instrument "USD_CNH")
										   ;; (:<= 'creation-time to)
										   ))
								    'trades.id)
							 'results))
					    (:desc 'creation-time))
				 :alists)
			  ))))
    (loop for trade in trades
	  summing (let ((instrument (make-keyword (assoccess trade :instrument))))
		    ;; (if (or (and (string= (assoccess trade :decision) "SELL")
		    ;; 		 (string= (assoccess trade :type) "BEARISH"))
		    ;; 	    (and (string= (assoccess trade :decision) "BUY")
		    ;; 		 (string= (assoccess trade :type) "BULLISH")))
		    ;; 	(to-pips instrument (assoccess trade :result))
		    ;; 	0)
		    (to-pips instrument (assoccess trade :result))))))
;; (get-global-revenue)
;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 3 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 2000 :day)))
;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 4 :day)))

;; Running hours.
;; (loop for i from 0 below 72 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :hour)))))

;; Per day.
;; (loop for i from 0 below 1000 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) (1+ i) :day)))))

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
	   (rates (get-rates-range instrument timeframe from to :provider :oanda :type :fx))
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

(defun validate-trades ()
  (let ((trades (conn (query (:order-by (:select '*
					  :from (:as (:order-by (:select 'trades.* 'patterns.*
								  :distinct-on 'trades.id
								  :from 'trades
								  :inner-join 'patterns-trades
								  :on (:= 'trades.id 'patterns-trades.trade-id)
								  :inner-join 'patterns
								  :on (:= 'patterns.id 'patterns-trades.pattern-id)
								  ;; :where (:not (:is-null 'trades.result))
								  :where (:is-null 'trades.result))
								'trades.id)
						     'results))
					(:desc 'creation-time))
			     :alists))))
    (push-to-log (format nil "Trying to validate ~a trades." (length trades)))
    (loop for trade in trades
	  do (let* ((from (let ((entry-time (assoccess trade :entry-time))
				(creation-time (assoccess trade :creation-time)))
			    (ceiling (* (if (not (equal entry-time :null))
					    entry-time
					    creation-time)
					1000000))))
		    (from-timestamp (local-time:unix-to-timestamp (ceiling (/ from 1000000)))))
	       (when (or (not omage.config:*is-production*)
			 (local-time:timestamp< from-timestamp
						(local-time:timestamp- (local-time:now) 1 :day)))
		 (push-to-log (format nil "Using minute rates from ~a to ~a to validate trade."
				      from-timestamp
				      (local-time:timestamp+ from-timestamp 3 :day)))
		 (let* ((instrument (make-keyword (assoccess trade :instrument)))
			(timeframe :M1)
			;; (to (* (local-time:timestamp-to-unix (local-time:timestamp+ from-timestamp 3 :day)) 1000000))
			;; (rates (get-rates-range instrument timeframe from to :provider :oanda :type :fx))
			(rates (get-rates-count-from-big instrument timeframe 50000 from))
			(result (get-trade-result (assoccess trade :entry-price)
						  (assoccess trade :tp)
						  (assoccess trade :sl)
						  rates)))
		   (push-to-log (format nil "Result obtained for trade: ~a." result))
		   (when result
		     (conn
		      (let ((dao (get-dao 'trade (assoccess trade :id))))
			(setf (slot-value dao 'result) result)
			(update-dao dao))))
		   (sleep 1)))))))
;; (validate-trades)

(defun ->delta-close (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates))
	 (penultimate-candle (nth (- lrates (1+ offset) 1) rates)))
    (- (rate-close last-candle)
       (rate-close penultimate-candle))))
;; (->delta-close *rates*)

(defun ->sma-close (rates offset n)
  (/ (loop for i below n
	   summing (->delta-close rates (+ i offset)))
     n))

(defun ->wma-close (rates offset n)
  (/ (loop
       for i below n
       for j from n above 0
       summing (* j (->delta-close rates (+ i offset))))
     (* n (1+ n) 1/2)))

(defun ->close (rates offset)
  (let* ((lrates (length rates)))
    (rate-close (nth (- lrates offset 1) rates))))
;; (->close *rates* 0)

(defun ->ema-close (rates offset n-sma n-ema)
  (let ((smoothing (/ 2 (1+ n-ema)))
	(ema (->sma-close rates (+ offset n-ema) n-sma)))
    (loop for i from (1- n-ema) downto 1
	  do (setf ema (+ ema (* smoothing (- (->delta-close rates (+ i offset)) ema)))))
    ema))

;; (describe-trades 1000)

(defun ->rsi-close (rates offset n)
  (let ((gain 0)
	(loss 0))
    (loop for i from 0 below n
	  do (let ((delta (->delta-close rates (+ offset i))))
	       (if (plusp delta)
		   (incf gain delta)
		   (incf loss (abs delta)))))
    (if (= loss 0)
	100
	(let ((rs (/ (/ gain n) (/ loss n))))
	  (- 100 (/ 100 (1+ rs)))))))

(defun ->high-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (rate-high last-candle)
       (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-open last-candle)
	   (rate-close last-candle)))))

(defun ->low-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-close last-candle)
	   (rate-open last-candle))
       (rate-low last-candle))))

(defun ->candle-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (abs (- (rate-close last-candle)
	    (rate-open last-candle)))))

;; (->delta-close-bid *rates* :offset 1)

(defenum perceptions
    (->sma-close
     ->wma-close
     ->ema-close
     ->rsi-close
     ;; ->delta-close
     ;; ->high-height
     ;; ->low-height
     ;; ->candle-height
     ))

(defun random->sma-close ()
  (let ((offset (random-int *rand-gen* 0 50))
	(n (random-int *rand-gen* 3 50)))
    (values `#(,->sma-close ,offset ,n)
	    (+ offset n))))
(defun random->wma-close ()
  (let ((offset (random-int *rand-gen* 0 50))
	(n (random-int *rand-gen* 3 50)))
    (values `#(,->wma-close ,offset ,n)
	    (+ offset n))))
(defun random->ema-close ()
  (let ((offset (random-int *rand-gen* 0 50))
	(n-sma (random-int *rand-gen* 3 25))
	(n-ema (random-int *rand-gen* 3 25)))
    (values `#(,->ema-close ,offset ,n-sma ,n-ema)
	    (+ offset n-sma n-ema))))
(defun random->rsi-close ()
  (let ((offset (random-int *rand-gen* 0 50))
	(n (random-int *rand-gen* 10 20)))
    (values `#(,->rsi-close ,offset ,n)
	    (+ offset n))))
(defun random->high-height ()
  (let ((offset (random-int *rand-gen* 0 50)))
    (values `#(,->high-height ,offset)
	    offset)))
(defun random->low-height ()
  (let ((offset (random-int *rand-gen* 0 50)))
    (values `#(,->low-height ,offset)
	    offset)))
(defun random->candle-height ()
  (let ((offset (random-int *rand-gen* 0 50)))
    (values `#(,->candle-height ,offset)
	    offset)))
(defun random->delta-close ()
  (let ((offset (random-int *rand-gen* 0 50)))
    (values `#(,->delta-close ,offset)
	    offset)))

(defun gen-random-beliefs (fns-count)
  (let ((fns-bag `(,#'random->sma-close
		   ,#'random->wma-close
		   ,#'random->ema-close
		   ,#'random->rsi-close
		   ;; ,#'random->high-height
		   ;; ,#'random->low-height
		   ;; ,#'random->candle-height
		   ;; ,#'random->delta-close
		   ))
	(max-lookbehind 0)
	(perceptions))
    (loop repeat fns-count
	  collect (multiple-value-bind (perc lookbehind)
		      (funcall (random-elt fns-bag))
		    (when (> lookbehind max-lookbehind)
		      (setf max-lookbehind lookbehind))
		    (push perc perceptions)))
    `((:perception-fns . ,(make-array (length perceptions) :initial-contents perceptions))
      (:lookahead-count . 10)
      (:lookbehind-count . ,(+ 10 max-lookbehind)))))
;; (gen-random-beliefs 30)

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn across perception-fns
	  collect (apply #'funcall
			 (nth-enum-tag (aref fn 0) 'perceptions)
			 rates (coerce (subseq fn 1) 'list)))))
;; (time (loop repeat 1000 do (funcall (gen-perception-fn #(#(0 0 10) #(0 1 10) #(1 0))) *rates*)))

(defclass rate ()
  ((time :col-type string :initarg :time)
   (instrument :col-type string :initarg :instrument)
   (timeframe :col-type string :initarg :timeframe)
   (open-bid :col-type double-float :initarg :open-bid)
   (open-ask :col-type double-float :initarg :open-ask)
   (high-bid :col-type double-float :initarg :high-bid)
   (high-ask :col-type double-float :initarg :high-ask)
   (low-bid :col-type double-float :initarg :low-bid)
   (low-ask :col-type double-float :initarg :low-ask)
   (close-bid :col-type double-float :initarg :close-bid)
   (close-ask :col-type double-float :initarg :close-ask)
   (volume :col-type int :initarg :volume))
  (:metaclass dao-class)
  (:table-name rates)
  (:keys time instrument timeframe))

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
  (:metaclass dao-class)
  (:table-name agents)
  (:keys id))

(defclass agent-pattern ()
  ((agent-id :col-type string :initarg :agent-id)
   (pattern-id :col-type string :initarg :pattern-id))
  (:metaclass dao-class)
  (:table-name agents-patterns)
  (:keys agent-id pattern-id))

(defclass pattern ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (type :col-type string :initarg :type)
   (instrument :col-type string :initarg :instrument)
   (timeframe :col-type string :initarg :timeframe))
  (:metaclass dao-class)
  (:table-name patterns)
  (:keys id))

(defclass pattern-trade ()
  ((pattern-id :col-type string :initarg :pattern-id)
   (trade-id :col-type string :initarg :trade-id))
  (:metaclass dao-class)
  (:table-name patterns-trades)
  (:keys pattern-id trade-id))

(defclass trade ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (agent-id :col-type (or db-null string) :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (creation-time :col-type integer :initarg :creation-time)
   (decision :col-type string :initarg :decision)
   (result :col-type (or db-null double-float) :initarg :result)
   (tp :col-type double-float :initarg :tp)
   (sl :col-type double-float :initarg :sl)
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
  (:metaclass dao-class)
  (:table-name trades)
  (:keys id))

(defun get-tp-sl (rates &optional (lookahead-count 10))
  (let* ((init-rate (rate-close (first rates)))
	 (max-pos 0)
	 (max-neg 0))
    (loop for rate in (subseq rates 0 lookahead-count)
	  do (let ((delta-high (- (rate-high rate) init-rate))
		   (delta-low (- (rate-low rate) init-rate)))
	       ;; Checking for possible price stagnation. If true, ignore.
	       (unless (and (< init-rate delta-high)
			    (> init-rate delta-low))
		 (when (> delta-high max-pos)
		   (setf max-pos delta-high))
		 (when (< delta-low max-neg)
		   (setf max-neg delta-low)))))
    `((:tp . ,(if (>= max-pos (abs max-neg)) max-pos max-neg))
      ;; (:sl . ,(if (>= max-pos (abs max-neg)) (* max-pos -1.0) (* max-neg -1.0)))
      (:sl . ,(if (>= (abs max-neg) max-pos) max-pos max-neg))
      )))
;; (time (get-tp-sl (get-input-dataset *rates* 400)))

(defun get-same-direction-outputs-idxs (rates count &key (lookahead-count 10) (lookbehind-count 10) direction-fn)
  (let* ((r (random-float *rand-gen* 0 1))
	 (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
	 (opposite-pred (if (> r 0.5) #'minusp #'plusp))
	 (idxs (shuffle (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count)))
	 (result))
    (loop for idx in idxs
	  do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
	       (when (and (< (length result) count)
			  (funcall pred (assoccess tp-sl :tp))
			  (/= (assoccess tp-sl :sl) 0))
		 (push idx result))))
    (if (> (length result) 1)
	result
	(get-same-direction-outputs-idxs rates count
					 :lookahead-count lookahead-count
					 :lookbehind-count lookbehind-count
					 :direction-fn opposite-pred))))
;; (get-same-direction-outputs-idxs *rates* :lookahead-count 5)

(defun gen-agent (num-rules rates perception-fns lookahead-count lookbehind-count)
  (let ((agent (make-instance 'agent)))
    (setf (slot-value agent 'creation-begin-time) (read-from-string (assoccess (first rates) :time)))
    (setf (slot-value agent 'creation-end-time) (read-from-string (assoccess (last-elt rates) :time)))
    (setf (slot-value agent 'perception-fns) (format nil "~s" perception-fns))
    (setf (slot-value agent 'lookahead-count) lookahead-count)
    (setf (slot-value agent 'lookbehind-count) lookbehind-count)
    (multiple-value-bind (antecedents consequents)
    	(make-ifis agent num-rules rates)
      (setf (slot-value agent 'antecedents) (format nil "~s" antecedents))
      (setf (slot-value agent 'consequents) (format nil "~s" consequents)))
    agent))
;; (gen-agent 3 *rates* (gen-random-beliefs 2) 10 55)

(defun gen-agents (num-agents num-rules rates perception-fns lookahead-count lookbehind-count)
  (loop repeat num-agents collect (gen-agent num-rules rates perception-fns lookahead-count lookbehind-count)))
;; (gen-agents 2 3 *rates* (assoccess *beliefs* :perception-fns) 10 55)

(defun evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (if (plusp tp)
			   (rate-close-ask (first rates))
			   (rate-close-bid (first rates))))
	(revenue 0)
	(max-pos 0)
	(max-neg 0)
	(exit-time)
	;; Needs to be (length rates) in case the trade never finishes.
	(finish-idx (length rates)))
    (loop for rate in (rest rates)
	  for idx from 1 below (length (rest rates))
	  do (let ((low (if (plusp tp)
			    (rate-low-bid rate)
			    (rate-low-ask rate)))
		   (high (if (plusp tp)
			     (rate-high-bid rate)
			     (rate-high-ask rate)))
		   (time (assoccess rate :time)))
	       (when (or (= tp 0) (= sl 0))
		 ;; We move to the next price.
		 (setf finish-idx 1)
		 (return))
	       (if (> tp sl)
		   ;; Then it's bullish.
		   (if (<= (- low starting-rate) sl)
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
		   (if (>= (- high starting-rate) sl)
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
		 (setf max-neg (- low starting-rate)))))
    `((:revenue . ,revenue)
      (:max-pos . ,max-pos)
      (:max-neg . ,max-neg)
      (:exit-time . ,exit-time)
      (:finish-idx . ,finish-idx))))
;; (evaluate-trade 0.0010 -0.0010 *rates*)

(defun get-agents-count (instrument timeframe types)
  (length (get-agents instrument timeframe types)))

(defun alist-keys (alist)
  (loop for item in alist collect (car item)))
;; (alist-keys (car (prepare-agents-properties (get-agents :AUD_USD :H1 '(:bullish)))))

(defun alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-values (car (prepare-agents-properties (get-agents :EUR_USD :H1 '(:bullish)))))

(defun describe-agents ()
  (with-open-stream (s (make-string-output-stream))
    (format s "<h3>AGENTS POOL.</h3><hr/>")
    (loop for instrument in ominp:*forex*
	  do (loop for types in '((:bullish) (:bearish) (:stagnated))
		   do (let* ((agents-props (prepare-agents-properties (get-agents instrument :H1 types)))
			     (agents-count (get-agents-count instrument :H1 types))
			     (vals (loop for agent-props in agents-props
					 collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
						       (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
						   (append (alist-values agent-props)
							   (list (format-rr avg-tp avg-sl)))))))
			(when (> (length agents-props) 0)
			  (format s "<h4>~a (~a, ~a)</h4>" instrument (car types) agents-count)
			  (format s "<pre>")
			  (format-table s vals :column-label (append (mapcar #'symbol-name (alist-keys (car agents-props)))
								     '("R/R")))
			  (format s "</pre><hr/>")))))
    (get-output-stream-string s)))
;; (describe-agents)

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
;; (prepare-agents-properties (get-agents :AUD_USD :H1 '(:stagnated)))

(defun get-all-agents ()
  (let ((markets (make-hash-table)))
    (loop for instrument in ominp:*forex*
	  do (let ((agents (make-hash-table)))
	       (loop for types in '(:bullish :bearish :stagnated)
		     do (let ((values (let* ((agents-props (prepare-agents-properties (get-agents instrument :H1 (list types))))
					     (vals (loop for agent-props in agents-props
							 collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
								       (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
								   (append agent-props
									   `((:r/r . ,(format-rr avg-tp avg-sl))))))))
					(when (> (length agents-props) 0)
					  vals))))
			  (when values
			    (setf (gethash types agents) values))
			  ))
	       (setf (gethash instrument markets) agents)
	       ))
    markets))
;; (time (get-all-agents))

(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
							  collect (format nil "COL~D" i)))
				   (column-align (loop for i from 1 to (length (car data))
						       collect :left)))
  (let* ((col-count (length column-label))
	 (strtable  (cons column-label	; table header
                          (loop for row in data ; table body with all cells as strings
				collect (loop for cell in row
					      collect (if (stringp cell)
							  cell
					;else
							  (format nil "~A" cell))))))
	 (col-widths (loop with widths = (make-array col-count :initial-element 0)
			   for row in strtable
			   do (loop for cell in row
				    for i from 0
				    do (setf (aref widths i)
					     (max (aref widths i) (length cell))))
			   finally (return widths))))
					;------------------------------------------------------------------------------------
					; splice in the header separator
    (setq strtable
	  (nconc (list (car strtable)		       ; table header
		       (loop for align in column-align ; generate separator
			     for width across col-widths
			     collect (case align
				       (:left   (format nil ":~v@{~A~:*~}"
							(1- width)  "-"))
				       (:right  (format nil "~v@{~A~:*~}:"
							(1- width)  "-"))
				       (:center (format nil ":~v@{~A~:*~}:"
							(- width 2) "-")))))
		 (cdr strtable)))	; table body
					;------------------------------------------------------------------------------------
					; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
				 collect (getf +CELL-FORMATS+ align))))
	  (widths  (loop for w across col-widths collect w)))
					; write each line to the given stream
      (dolist (row strtable)
	(apply #'format stream row-fmt (mapcan #'list widths row))))))

(defun get-trades (&optional limit)
  (if limit
      (conn (query (:select 
		       '*
		     :from
		     (:as (:select '*
			    (:as (:over (:row-number)
					(:partition-by 'instrument 'timeframe
						       :order-by (:desc 'creation-time)))
			     :idx)
			    :from
			    (:as (:select 'patterns.instrument
				   'patterns.timeframe
				   'patterns.type
				   'trades.id
				   'trades.creation-time
				   'trades.test-trades-won
				   'trades.test-trades-lost
				   'trades.test-avg-revenue
				   'trades.test-avg-activation
				   'trades.test-avg-return
				   'trades.test-total-return
				   'trades.tp
				   'trades.sl
				   'trades.decision
				   'trades.entry-price
				   :distinct-on 'trades.id
				   :from 'trades
				   :inner-join 'patterns-trades
				   :on (:= 'trades.id 'patterns-trades.trade-id)
				   :inner-join 'patterns
				   :on (:= 'patterns-trades.pattern-id 'patterns.id))
				 'full-results))
			  'idx-results)
		     :where (:<= 'idx '$1))
		   limit
		   :alists))
      (conn (query (:order-by (:select 'patterns.instrument
				'patterns.timeframe
				'patterns.type
				'trades.creation-time
				'trades.test-trades-won
				'trades.test-trades-lost
				'trades.test-avg-revenue
				'trades.test-avg-activation
				'trades.test-avg-return
				'trades.test-total-return
				'trades.tp
				'trades.sl
				'trades.decision
				'trades.result
				'trades.entry-price
				:distinct-on 'trades.id
				:from 'trades
				:inner-join 'patterns-trades
				:on (:= 'trades.id 'patterns-trades.trade-id)
				:inner-join 'patterns
				:on (:= 'patterns-trades.pattern-id 'patterns.id))
			      'trades.id
			      (:desc 'trades.creation-time))
		   :alists))))
;; (get-trades 3)

(defun describe-trades (&optional limit)
  (let* ((trades (get-trades limit))
	 (trades-won (loop for trade in trades
			   summing (assoccess trade :test-trades-won)))
	 (trades-lost (loop for trade in trades
			    summing (assoccess trade :test-trades-lost)))
	 (total-revenue (loop for trade in trades
			      summing (to-pips (assoccess trade :instrument)
					       (assoccess trade :test-total-revenue)))))
    (format t "Total trades won: ~a. Total trades lost: ~a. Total trades: ~a. Total revenue: ~a. Avg revenue: ~a.~%~%"
	    trades-won
	    trades-lost
	    (+ trades-won trades-lost)
	    total-revenue
	    (/ total-revenue (+ trades-won trades-lost)))
    (loop for trade in trades
	  do (format t "market: ~a, train-total-revenue: ~5$, train-trades-won: ~a, train-trades-lost: ~a,    test-total-revenue: ~5$, test-trades-won: ~a, test-trades-lost: ~a~%"
		     (assoccess trade :instrument)
		     (to-pips (assoccess trade :instrument)
			      (assoccess trade :train-total-revenue))
		     (assoccess trade :train-trades-won)
		     (assoccess trade :train-trades-lost)
		     (to-pips (assoccess trade :instrument)
			      (assoccess trade :test-total-revenue))
		     (assoccess trade :test-trades-won)
		     (assoccess trade :test-trades-lost)))))

(defun read-str (str)
  (read-from-string str))

(defun get-perception-fn (agent)
  (gen-perception-fn (read-str (slot-value agent 'perception-fns))))

(defun eval-agent (agent rates)
  (let ((perception-fn (get-perception-fn agent)))
    (eval-ifis (funcall perception-fn rates)
	       (read-str (slot-value agent 'antecedents))
	       (read-str (slot-value agent 'consequents)))))

;; (comment
;;  (defparameter *consensus* nil)
;;  (float (/ (count t *consensus*)
;; 	   (length *consensus*))))

(defun eval-agents (instrument timeframe types rates &key (count 5))
  (let (tps sls activations ids)
    (let ((agents (get-agents instrument timeframe types)))
      (loop for agent in agents
	    do (multiple-value-bind (tp sl activation)
		   (eval-agent agent rates)
		 (push tp tps)
		 (push sl sls)
		 (push activation activations)
		 (push (slot-value agent 'id) ids)
		 )))
    ;; (format t "~a, ~a~%" (apply #'min activations) (apply #'max activations))
    (let ((idxs (sorted-indexes activations #'>))
	  (tp 0)
	  (sl 0)
	  (activation 0)
	  ;; (consensus t)
	  ;; (len (min count (length activations)))
	  )
      (setf tp (nth (position 0 idxs) tps))
      (setf sl (nth (position 0 idxs) sls))
      (setf activation (nth (position 0 idxs) activations))
      ;; (when (> activation 0.5)
      ;; (loop for idx from 1 below len
      ;; 	    do (let* ((pos (position idx idxs))
      ;; 		      (nth-tp (nth pos tps))
      ;; 		      (nth-sl (nth pos sls)))

      ;; 		 ;; ;; consensus
      ;; 		 ;; (when (< (* nth-tp tp) 0)
      ;; 		 ;;   (setf consensus nil)
      ;; 		 ;;   (return))

      ;; 		 (when (< (* nth-tp tp) 0)
      ;; 		 	(setf tp 0)
      ;; 		 	(setf sl 0)
      ;; 		 	(return))
      ;; 		 (when (< (* nth-sl sl) 0)
      ;; 		 	(setf tp 0)
      ;; 		 	(setf sl 0)
      ;; 		 	(return))
	      
      ;; 		 ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      ;; 		 ;; 	(setf tp nth-tp))
      ;; 		 ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      ;; 		 ;; 	(setf sl nth-sl))
      ;; 		 ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      ;; 		 ;; 	(setf tp nth-tp))
      ;; 		 ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      ;; 		 ;; 	(setf sl nth-sl))
      ;; 		 ;; (incf tp nth-tp)
      ;; 		 ;; (incf sl nth-sl)
      ;; 		 ))
	;;)
      ;; (push consensus *consensus*)
      (values (/ tp 1)
	      (/ sl 1)
	      activation
	      (list (nth (position 0 idxs) ids))))))

(defun -evaluate-agents (&key instrument timeframe types rates agent idx test-size)
  "Used for EVALUATE-AGENT and EVALUATE-AGENTS."
  (push-to-log "Trying to evaluate agents.")
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
    (push-to-log "Evaluating agent: ")
    (loop while (< idx (length rates))
	  do (let* ((input-dataset (get-input-dataset rates idx))
		    (output-dataset (get-output-dataset rates idx)))
	       (multiple-value-bind (tp sl activation)
		   (if agent
		       (eval-agent agent input-dataset)
		       (eval-agents instrument timeframe types input-dataset))
		 (let* ((trade (evaluate-trade tp sl output-dataset))
			(revenue (assoccess trade :revenue))
			(max-pos (assoccess trade :max-pos))
			(max-neg (assoccess trade :max-neg))
			(exit-time (assoccess trade :exit-time))
			(finish-idx (assoccess trade :finish-idx)))
		   (if (or (= revenue 0)
			   (> (abs sl) (abs tp))
			   (> (* tp sl) 0)
			   (= tp 0)
			   (= sl 0)
			   (< (abs (to-pips instrument sl)) 3)
			   ;; (> (abs (to-pips instrument sl)) 20)
			   )
		       ;; (push-to-log "." :add-newline? nil)
		       (incf num-datapoints)
		       (progn
			 ;; (format t "TP: ~a, SL: ~a, R: ~a~%" tp sl revenue)
			 ;; (push-to-log "+" :add-newline? nil)
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
				   (rate-close-ask (nth idx rates))
				   (rate-close-bid (nth idx rates)))
			       entry-prices)
			 (push (if (plusp tp)
				   (rate-close-bid (nth finish-idx output-dataset))
				   (rate-close-ask (nth finish-idx output-dataset)))
			       exit-prices)
			 (push max-pos max-poses)
			 (push max-neg max-negses)
			 (push revenue revenues)))
		   ;; (incf idx finish-idx)
		   (incf idx)
		   ))))
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
;; (evaluate-agents :EUR_USD :H1 '(:BULLISH) *rates*)

;; (defparameter *activations* nil)
;; (defparameter *tps* nil)
;; (defparameter *sls* nil)
;; (defparameter *revenues* nil)

;; (defun coco ()
;;   (let ((sorted (sort (loop for act in *activations*
;; 			    for rev in *revenues*
;; 			    collect (list act rev))
;; 		      #'<
;; 		      :key #'first)))
;;     ;; (cl-mathstats:correlation
;;     ;;  (mapcar #'second sorted)
;;     ;;  (mapcar #'first sorted))
;;     ;; (loop for s in sorted
;;     ;; 	do (format t "~a ~a~%" (first s) (if (plusp (second s)) 1 -1)))
;;     (let* ((first-half (subseq sorted 0 (round (/ (length sorted) 2))))
;; 	   (second-half (subseq sorted (round (/ (length sorted) 2))))
;; 	   (first-act (float (/ (length (remove-if-not #'plusp first-half :key #'second))
;; 				(length first-half))))
;; 	   (second-act (float (/ (length (remove-if-not #'plusp second-half :key #'second))
;; 				 (length second-half)))))
;;       (format t "~a ~a ~a~%"
;; 	      (> second-act first-act)
;; 	      first-act
;; 	      second-act
;; 	      ))
;;     ))

;; (cl-mathstats:correlation *activations* *revenues*)
;; (ql:system-apropos "math")
;; (ql:quickload :cl-mathstats)

(defun evaluate-agent (agent rates)
  (let ((fitnesses (-evaluate-agents :agent agent :rates rates :idx (slot-value agent 'lookbehind-count))))
    ;; (when (> (assoccess fitnesses :total-revenue) 0)
    ;;   (setf *activations* (assoccess fitnesses :activations))
    ;;   (setf *tps* (assoccess fitnesses :tps))
    ;;   (setf *sls* (assoccess fitnesses :sls))
    ;;   (setf *revenues* (assoccess fitnesses :revenues))
    ;;   (coco))
    ;; (ignore-errors (print (cl-mathstats:correlation *activations* *revenues*)))
    
    (loop for fitness in fitnesses
	  ;; TODO: We should be returning symbols (not kws) from -evaluate-agents.
;;; and then remove this format + read-from-string.
	  do (setf (slot-value agent (read-from-string (format nil "~a" (car fitness))))
		   (if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    agent))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(defun evaluate-agents (instrument timeframe types rates &key (test-size 50))
  (-evaluate-agents :instrument instrument :timeframe timeframe :types types :rates rates :test-size test-size))
;; (time (evaluate-agents :EUR_USD :H1 '(:BULLISH) (subseq *rates* 0 200)))

(defun update-agent-fitnesses (instrument timeframe types agent rates)
  (let* ((agents (gethash (list instrument timeframe types) *agents-cache*))
	 (agent-idx (position agent agents :test (lambda (agent1 agent2) (string= (slot-value agent1 'id)
										  (slot-value agent2 'id))))))
    (setf (nth agent-idx agents) (evaluate-agent agent rates))
    ;; (setf (gethash (list instrument timeframe types) *agents-cache*)
    ;; 	  (evaluate-agent agent rates))
    ))

(defun update-agents-fitnesses (instrument timeframe types agents rates)
  (loop for agent in agents
	do (update-agent-fitnesses instrument timeframe types agent rates))
  agents)

(defun get-max-lookbehind (instrument timeframe types)
  (let* ((types (flatten types))
	 ;; (agent-ids (get-agent-ids-from-patterns instrument timeframe types))
	 )
    (loop for agent in (get-agents instrument timeframe types) maximize (slot-value agent 'lookbehind-count))
    ;; (caar (conn (query (:select (:max 'lookbehind-count) :from 'agents :where (:in 'id (:set agent-ids))))))
    ))

(defun vector-1-similarity (vec1 vec2)
  (loop for v across vec1 do (when (position v vec2) (return t))))
;; (vector-1-similarity #(1 2 3) #(10 20 30))

(defun activations-returns-dominated-p (activations1 returns1 activations2 returns2)
  (loop for a1 across activations1
	for r1 across returns1
	for a2 across activations2
	for r2 across returns2
	when (and (> r1 0) ;; To even consider it, the return1 must be greater than 0.
		  ;; (/= (* a1 r1) 0) ; Checking that activation1 and return1 are not equal to 0.
		  (>= a1 a2)
		  (>= r1 r2))
	  do (return nil)
	finally (return t)))
;; (activations-returns-dominated-p #(1 1 1) #(1 1 1) #(2 2 2) #(2 2 2))
;; (activations-returns-dominated-p #(2 0 0) #(2 0 0) #(1 1 1) #(1 1 1))

(defun is-agent-dominated? (agent agents)
  (if (= (length (slot-value agent 'tps)) 0)
      t ;; AGENT is dominated.
      (let* ((agent-id-0 (slot-value agent 'id))
	     (avg-revenue-0 (slot-value agent 'avg-revenue))
	     (trades-won-0 (slot-value agent 'trades-won))
	     (trades-lost-0 (slot-value agent 'trades-lost))
	     (agent-direction-0 (aref (slot-value agent 'tps) 0))
	     (avg-return-0 (slot-value agent 'avg-return))
	     (total-return-0 (slot-value agent 'total-return))
	     (activations-0 (slot-value agent 'activations))
	     (returns-0 (slot-value agent 'returns))
	     ;; (agent-directions (slot-value agent 'tps))
	     ;; (stdev-revenue-0 (slot-value agent 'stdev-revenue))
	     ;; (entry-times-0 (slot-value agent 'entry-times))
	     (is-dominated? nil))
	;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
	(loop for agent in agents
	      do (when (> (length (slot-value agent 'tps)) 0)
		   (let* ((agent-id (slot-value agent 'id))
			  (avg-revenue (slot-value agent 'avg-revenue))
			  (trades-won (slot-value agent 'trades-won))
			  (trades-lost (slot-value agent 'trades-lost))
			  (agent-direction (aref (slot-value agent 'tps) 0))
			  (avg-return (slot-value agent 'avg-return))
			  (total-return (slot-value agent 'total-return))
			  (activations (slot-value agent 'activations))
			  (returns (slot-value agent 'returns))
			  ;; (agent-directions (slot-value agent 'tps))
			  ;; (stdev-revenue (slot-value agent 'stdev-revenue))
			  ;; (entry-times (slot-value agent 'entry-times))
			  )
		     ;; Fitnesses currently being used.
		     (when (or (<= total-return-0 0)
			       (and (> (* agent-direction-0 agent-direction) 0)
				    ;; (>= avg-revenue avg-revenue-0)
				    ;; (< stdev-revenue stdev-revenue-0)
				    ;; (>= trades-won trades-won-0)
				    ;; (<= trades-lost trades-lost-0)
				    ;; (>= avg-return avg-return-0)
				    (>= total-return total-return-0)
				    (activations-returns-dominated-p activations-0 returns-0 activations returns)
				    ;; (>= (/ trades-won
				    ;; 	      (+ trades-won trades-lost))
				    ;; 	   (/ trades-won-0
				    ;; 	      (+ trades-won-0 trades-lost-0)))
				    ;; (vector-1-similarity entry-times entry-times-0)
				    )
			       )
		       ;; Candidate agent was dominated.
		       (let ((metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST" "AVG-RETURN" "TOTAL-RETURN")))
			 (with-open-stream (s (make-string-output-stream))
			   ;; (push-to-agents-log )
			   (format s "<pre><b>(BETA) </b>Agent ID ~a~%" agent-id-0)
			   (format-table s `((,(format nil "~6$" avg-revenue-0) ,trades-won-0 ,trades-lost-0 ,(format nil "~2$" avg-return-0) ,(format nil "~2$" total-return-0))) :column-label metric-labels)
			   (format s "</pre>")

			   (format s "<pre><b>(ALPHA) </b>Agent ID ~a~%" agent-id)
			   (format-table s `((,(format nil "~6$" avg-revenue) ,trades-won ,trades-lost ,(format nil "~2$" avg-return) ,(format nil "~2$" total-return))) :column-label metric-labels)
			   (format s "</pre><hr/>")
		     
			   ;; (format s "Fitnesses <b>~a</b>:<br/>~a: ~a<br/>~a: ~a<br/>~a: ~a"
			   ;; 	agent-id-0
			   ;; 	(symbol-name 'avg-revenue) avg-revenue-0
			   ;; 	(symbol-name 'trades-won) trades-won-0
			   ;; 	(symbol-name 'trades-lost) trades-lost-0)
			   ;; (format s "Fitnesses (~a):<br/>~a: ~a<br/>~a: ~a<br/>~a: ~a<hr />"
			   ;; 	agent-id
			   ;; 	(symbol-name 'avg-revenue) avg-revenue
			   ;; 	(symbol-name 'trades-won) trades-won
			   ;; 	(symbol-name 'trades-lost) trades-lost)
			   (push-to-agents-log (get-output-stream-string s))))
		 
		       (setf is-dominated? t)
		       (return)))))
	is-dominated?)))

(defun get-agent (instrument timeframe types agent-id)
  (find agent-id (gethash (list instrument timeframe types) *agents-cache*)
	:key (lambda (agent) (slot-value agent 'id))
	:test #'string=))
;; (get-agent :EUR_USD :H1 '(:BULLISH) "48F3970F-36C1-4A49-9E54-95746CFEA9FE")
;; (slot-value (first (get-agents :EUR_USD :H1 '(:BULLISH))) 'id)

(defun optimization (instrument timeframe types gen-agent-fn rates seconds &key report-fn)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe types)))
		  (update-agents-fitnesses instrument timeframe types agents rates)
		  (list (evaluate-agent (funcall gen-agent-fn) rates))))
	(purged-agents))
    (push-to-log (format nil "~a agents retrieved to start optimization." (length agents)))
    (push-to-log (format nil "Performing optimization for ~a seconds." seconds))
    (push-to-agents-log (format nil "<h4>~a (~a, ~a)</h4>~%" instrument (car types) (length agents)))
    (loop with until-timestamp = (local-time:timestamp+ (local-time:now) seconds :sec)
	  do (if (local-time:timestamp> (local-time:now) until-timestamp)
		 (progn
		   ;; Inserting new agents in Pareto Frontier.
		   (push-to-log (format nil "Updating Pareto frontier with ~a agents." (length agents)))
		   (conn (loop for agent in agents
			       do (unless (get-agent instrument timeframe types (slot-value agent 'id))
				    (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
				    (let ((metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST")))
				      (with-open-stream (s (make-string-output-stream))
					(format s "<pre><b>(OMEGA) </b>Agent ID ~a~%" (slot-value agent 'id))
					(format-table s `((,(format nil "~6$" (slot-value agent 'avg-revenue)) ,(slot-value agent 'trades-won) ,(slot-value agent 'trades-lost))) :column-label metric-labels)
					(format s "</pre><hr/>")
					(push-to-agents-log (get-output-stream-string s))))
				    (add-agent agent instrument timeframe types))))
		   (push-to-log "Pareto frontier updated successfully.")
		   (return))
		 (let* ((challenger (list (evaluate-agent (funcall gen-agent-fn) rates)))
			(is-dominated? (is-agent-dominated? (car challenger) agents)))
		   (unless is-dominated?
		     ;; Purging agents.
		     (loop for in-trial in agents
			   do (if (is-agent-dominated? in-trial challenger)
				  (progn
				    (push-to-log (format nil "Removing agent with ID ~a" (slot-value in-trial 'id)))
				    (push-to-agents-log (format nil "Removing agent with ID ~a" (slot-value in-trial 'id)))
				    (remove-agent in-trial instrument timeframe types))
				  (push in-trial purged-agents)))
		     (push (first challenger) purged-agents)
		     (setf agents purged-agents)
		     (setf purged-agents nil)
		     )
		   ;; (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
		   ;; (add-agent agent instrument timeframe types)
		   (when report-fn
		     (funcall report-fn (get-agents instrument timeframe types) rates)))))))

(defun insert-pattern (instrument timeframe type)
  (conn (make-dao 'pattern :type (format nil "~a" type)
			   :instrument (format nil "~a" instrument)
			   :timeframe (format nil "~a" timeframe))))

;; (conn (query (:select '* :from 'patterns) :alists))

(defun get-patterns (instrument timeframe types)
  (let* ((types (flatten types)))
    (conn (query (:select '* :from 'patterns :where (:and (:= 'instrument (format nil "~a" instrument))
							  (:= 'timeframe (format nil "~a" timeframe))
							  (:in 'type (:set (loop for type in types collect (format nil "~a" type))))))
		 :alists))))
;; (get-patterns :EUR_USD :H1 '(:BULLISH (:BEARISH) :STAGNATED))

(defun get-agent-ids-from-patterns (instrument timeframe types)
  (let* ((types (flatten types))
	 (patterns (get-patterns instrument timeframe types))
	 )
    ;; (loop for agent in (get-agents instrument timeframe types)
    ;; 	  collect (slot-value agent 'id))
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (assoccess pattern :id))))) :column))
    ))
;; (get-agent-ids-from-patterns :AUD_USD :H1 '(:bullish))
;; (get-agent-ids-from-patterns :AUD_USD :H1 '((:bullish) (:stagnated)))

(defun wipe-agents ()
  (conn (query (:delete-from 'agents :where (:= 1 1)))
	(query (:delete-from 'agents-patterns :where (:= 1 1)))))
;; (wipe-agents)

(defun sync-agents ()
  (wipe-agents)
  (loop for key being each hash-key of *agents-cache*
	do (loop for agent in (apply #'get-agents key)
		 do (apply #'insert-agent agent key))))

(defun get-agents (instrument timeframe types)
  (flatten
   (remove nil
	   (loop for type in (flatten types)
		 collect
		 (let ((type (list type)))
		   (if (gethash (list instrument timeframe type) *agents-cache*)
		       (gethash (list instrument timeframe type) *agents-cache*)
		       (let* ((agent-ids (get-agent-ids-from-patterns instrument timeframe type))
			      (agents (conn (query (:select '* :from 'agents :where (:in 'id (:set agent-ids)))
						   (:dao agent)))))
			 (setf (gethash (list instrument timeframe type) *agents-cache*) agents)
			 agents)))))))

(defun format-rr (risk reward)
  (format nil "~a / ~2$"
	  (if (= risk 0)
	      0
	      (/ (* 10000 (abs risk))
		 (* 10000 (abs risk))))
	  (if (= reward 0)
	      0
	      (/ (* 10000 (abs risk))
		 (* 10000 (abs reward))))))

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

(defun is-market-close ()
  (let ((day-of-week (local-time:timestamp-day-of-week (local-time:now) :timezone local-time:+utc-zone+))
	(hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (and omage.config:*is-production*
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

(defun insert-trade (agent-id instrument timeframe types train-fitnesses test-fitnesses tp sl rates)
  (conn (let ((patterns (get-patterns instrument timeframe types))
	      (trade (make-dao 'trade
			       :agent-id agent-id
			       :creation-time (local-time:timestamp-to-unix (local-time:now))
			       :decision (if (or (= sl 0) (= tp 0))
					     "HOLD"
					     (if (> tp 0)
						 "BUY"
						 "SELL"))
			       :tp tp
			       :sl sl
			       :entry-price (assoccess (last-elt rates) :close-bid)
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
			     :trade-id (slot-value trade 'id)))
	  )))

;; (require 'sb-sprof)
;; (sb-sprof:start-profiling)
;; (sb-sprof:reset)
;; (sb-sprof:report :type :flat)
;; (sb-sprof:report)

(defun test-agents (instrument timeframe types rates training-dataset testing-dataset &key (test-size 50))
  (multiple-value-bind (tp sl activations agent-ids)
      (eval-agents instrument timeframe types testing-dataset)
    (let* (;; (train-fitnesses (evaluate-agents instrument timeframe types training-dataset))
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
		 (> (abs (to-pips instrument sl)) 3)
		 ;; (< (to-pips instrument (abs sl)) 20)
		 (/= (assoccess test-fitnesses :trades-won) 0)
		 (/= (+ (assoccess test-fitnesses :trades-won)
			(assoccess test-fitnesses :trades-lost))
		     0))
	(push-to-log (format nil "Trying to create trade. Agents IDs: ~a" agent-ids))
	;; (insert-trade (first agent-ids) instrument timeframe types train-fitnesses test-fitnesses tp sl rates)
	(insert-trade (first agent-ids) instrument timeframe types test-fitnesses test-fitnesses tp sl rates)
	(push-to-log "Trade created successfully.")))))

;; General log.
(let (log)
  (defun push-to-log (msg &key (add-newline? t) (size 5000))
    (when omage.config:*is-log*
      (if add-newline?
	  (push (format nil "~a<br/>" msg) log)
	  (push (format nil "~a" msg) log))
      (when (> (length log) size)
	(setf log (butlast log)))))
  (defun read-log ()
    (format nil "<h3>LOG.</h3><hr/><br/>~{~a~%~}"
	    (reverse log)
	    ))
  (defun clear-log ()
    (setf log nil)))
;; (push-to-log (random 10) :size 10)
;; (read-log)

;; Agents log.
(let (log)
  (defun push-to-agents-log (msg &key (add-newline? t) (size 5000))
    (when omage.config:*is-log*
      (if add-newline?
	  (push (format nil "~a<br/>" msg) log)
	  (push (format nil "~a" msg) log))
      (when (> (length log) size)
	(setf log (butlast log)))))
  (defun read-agents-log ()
    (format nil "~a<h3>AGENTS LOG.</h3><hr/><br/>~{~a~%~}"
	    (describe-agents)
	    (reverse log)
	    ))
  (defun clear-agents-log ()
    (setf log nil)))

;; (read-agents-log)
;; (read-log)

(defun clear-logs ()
  (clear-log)
  (clear-agents-log))

(defun init ()
  (bt:make-thread
   (lambda ()
     (swank:create-server :port 4444))))
;; (init)

(defun get-rates-count-from-big (instrument timeframe count from)
  (let* ((instrument (format nil "~a" instrument))
	 (timeframe (format nil "~a" timeframe))
	 (from-str (format nil "~a" from))
	 (try-db (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
												(:= 'timeframe timeframe)
												(:>= 'time from-str)))
							  (:desc 'rates.time))
					       '$1)
				       count
				       :alists)))))
    ;; Checking if we have all the necessary rates on the database already.
    (if (= (length try-db) count)
	try-db
	;; We need to query from broker then.
	(let ((recent (get-rates-count-from instrument timeframe 5000 from :provider :oanda :type :fx)))
	  ;; Updating DB.
	  (insert-rates instrument timeframe recent)
	  (append (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
												 (:= 'timeframe timeframe)
												 (:>= 'time from-str)))
							   (:desc 'rates.time))
						'$1)
					(1- count)
					:alists)))
		  (last recent))))))
;; (length (get-rates-count-from-big :EUR_USD :M1 20000 (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 40 :day)))))
;; (loop for rate in (get-rates-count-from-big :EUR_USD :M1 20000 (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 40 :day))))
;;       do (print (local-time:unix-to-timestamp (/ (read-from-string (assoccess rate :time)) 1000000))))

(defun get-rates-count-big (instrument timeframe count)
  (let ((recent (get-rates-count instrument timeframe 100)))
    ;; Updating DB.
    (insert-rates instrument timeframe recent)
    ;; Getting COUNT - 1 rates, and then appending last rate from RECENT.
    (append (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument (format nil "~a" instrument))
											   (:= 'timeframe (format nil "~a" timeframe))))
						     ;; TODO: It's not a good idea to sort by time, considering it's a string. The good news is that we don't have to worry about this until year ~2200.
						     (:desc 'rates.time))
					  '$1)
				  (1- count)
				  :alists)))
	    (last recent))
    ))
;; (get-rates-count-big :EUR_USD :H1 10)

(defun get-random-rates-count-big (instrument timeframe count)
  "Assumes a minimum of 50K rates"
  (let* ((offset (random-int *rand-gen* 0 (- 50000 count))))
    (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument (format nil "~a" instrument))
										   (:= 'timeframe (format nil "~a" timeframe))))
					     ;; TODO: It's not a good idea to sort by time, considering it's a string. The good news is that we don't have to worry about this until year ~2200.
					     (:desc 'rates.time))
				  '$1 '$2)
			  count offset
			  :alists)))))
;; (get-random-rates-count-big :EUR_USD :H1 10)

(defun get-rates-range-big (instrument timeframe from to)
  (let ((instrument (format nil "~a" instrument))
	(timeframe (format nil "~a" timeframe))
	(from (format nil "~a" from))
	(to (format nil "~a" to)))
    (reverse (conn (query (:order-by (:select '* :from 'rates :where (:and (:= 'instrument instrument)
									   (:= 'timeframe timeframe)
									   (:>= 'time from)
									   (:<= 'time to)))
				     (:desc 'rates.time))
			  :alists)))))
;; (get-rates-range-big :EUR_USD :H1 (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 20 :day))) (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 10 :day))))

;; (get-rates-count-big :EUR_USD :H1 100)

;; (loop for rate in (get-random-rates-count-big :EUR_USD :H1 200)
;;       do (print (local-time:unix-to-timestamp (/ (read-from-string (assoccess rate :time)) 1000000))))

;; (get-random-rates-count-big :AUD_USD :H1
;; 				    6200)

(defun refresh-memory ()
  (fare-memoization:unmemoize 'read-str)
  (sb-ext:gc :full t)
  (fare-memoization:memoize 'read-str))

(defun -loop-optimize-test (&key
			      (report-fn nil)
			      (type-groups '((:bullish) (:bearish) (:stagnated)))
			      (test-size 50)
			      )
  (dolist (instrument omage.config:*instruments*)
    (dolist (timeframe omage.config:*timeframes*)
      (unless (is-market-close)
	(push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
	(let* ((rates (if omage.config:*is-production*
			  (get-rates-count-big instrument timeframe
					       (+ omage.config:*max-creation-dataset-size* omage.config:*max-training-dataset-size* omage.config:*max-testing-dataset-size*))
			  (get-random-rates-count-big instrument timeframe
						      (+ omage.config:*max-creation-dataset-size* omage.config:*max-training-dataset-size* omage.config:*max-testing-dataset-size*))))
	       (dataset-size (length rates)))
	  (push-to-log "Retrieved rates successfully.")
	  (let ((full-training-dataset (subseq rates
					       (- dataset-size
						  omage.config:*max-testing-dataset-size*
						  omage.config:*max-training-dataset-size*)
					       (- dataset-size
						  omage.config:*max-testing-dataset-size*)))
		(full-creation-dataset (subseq rates
					       0
					       (- dataset-size
						  omage.config:*max-testing-dataset-size*
						  omage.config:*max-training-dataset-size*)))
		(testing-dataset (let ((dataset (subseq rates
							(- dataset-size
							   omage.config:*max-testing-dataset-size*))))
				   (push-to-log (format nil "Testing dataset created successfully. Size: ~s. Dataset from ~a to ~a."
							(length dataset)
							(local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
							(local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))))
				   dataset))
		(agents-count (get-agents-count instrument timeframe type-groups)))
	    (if (and omage.config:*is-production* (> agents-count 0))
		(progn
		  (push-to-log "<b>SIGNAL.</b><hr/>")
		  (push-to-log (format nil "Trying to create signal with ~a agents." agents-count))
		  (test-agents instrument timeframe type-groups rates full-training-dataset testing-dataset :test-size test-size))
		(push-to-log "Not enough agents to create a signal."))
	    (loop for types in type-groups
		  ;; (multiple-value-bind (types)
		  ;;     (winning-type-output-dataset rates type-groups
		  ;;    :min-dataset-size omage.config:*max-testing-dataset-size*
		  ;;    :max-dataset-size omage.config:*max-testing-dataset-size*)
		  do (let* ((training-dataset (multiple-value-bind (from to)
						  (get-rates-chunk-of-types full-training-dataset types
									    :slide-step 50
									    :min-chunk-size 300
									    :max-chunk-size 700)
						(let ((dataset (subseq full-training-dataset from to)))
						  (push-to-log (format nil "Training dataset created successfully. Size: ~s. Dataset from ~a to ~a."
								       (length dataset)
								       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
								       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))
								       ))
						  dataset)))
			    (creation-dataset (multiple-value-bind (from to)
						  (get-rates-chunk-of-types full-creation-dataset types
									    :slide-step 50
									    :min-chunk-size 300
									    :max-chunk-size 700)
						(let ((dataset (subseq full-creation-dataset from to)))
						  (push-to-log (format nil "Creation dataset created successfully. Size: ~s. Dataset from ~a to ~a."
								       (length dataset)
								       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
								       (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))))
						  dataset))))
		       (push-to-log "<b>OPTIMIZATION.</b><hr/>")
		       (push-to-log (format nil "~a agents retrieved for pattern ~s." agents-count types))
		       (optimization instrument timeframe types
				     (lambda () (let ((beliefs (gen-random-beliefs omage.config:*number-of-agent-inputs*)))
						  (gen-agent omage.config:*number-of-agent-rules* creation-dataset
							     (assoccess beliefs :perception-fns)
							     (assoccess beliefs :lookahead-count)
							     (assoccess beliefs :lookbehind-count))))
				     training-dataset
				     omage.config:*seconds-to-optimize-per-pattern*
				     :report-fn report-fn)
		       (push-to-log "Optimization process completed.")
		       (when (not omage.config:*is-production*)
			 (push-to-log "<b>SIGNAL.</b><hr/>")
			 (push-to-log (format nil "Trying to create signal with ~a agents." agents-count))
			 (test-agents instrument timeframe type-groups rates full-training-dataset testing-dataset :test-size test-size)
			 (push-to-log "Not enough agents to create a signal."))
		       ))))
	(when omage.config:*is-production*
	  (push-to-log "<b>VALIDATION.</b><hr/>")
	  (push-to-log "Validating trades older than 24 hours.")
	  (validate-trades)))
      (refresh-memory)
      (sync-agents)))
  (unless omage.config:*is-production*
    (wipe-agents)))

(defun loop-optimize-test ()
  (handler-bind ((error (lambda (c)
			  (log-stack c))))
    (when (is-market-close)
      (format t "~%===============================================~%MARKET IS CLOSED~%SWITCH TO DEVELOPMENT MODE IF YOU WANT TO RUN EXPERIMENTS.~%==============================================="))
    (clear-logs)
    (refresh-memory)
    (loop (unless (is-market-close))
	  (-loop-optimize-test))))

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

(defun score-rates (rates &key (lookahead 10) (stagnation-threshold 0.5))
  (let* ((results (loop for i from 0 below (- (length rates) lookahead)
			collect (get-tp-sl (get-output-dataset rates i) lookahead)))
	 (rrs (loop for result in results collect (* (/ (assoccess result :sl)
							(assoccess result :tp))
						     (if (plusp (assoccess result :tp))
							 -1
							 1))))
	 (stagnated (count-if (lambda (elt) (>= (abs elt) stagnation-threshold)) rrs))
	 (uptrend (count-if (lambda (elt) (and (< (abs elt) stagnation-threshold)
					       (plusp elt)))
			    rrs))
	 (downtrend (count-if (lambda (elt) (and (< (abs elt) stagnation-threshold)
						 (minusp elt)))
			      rrs)))
    (let ((lrates (length rates)))
      `((:bullish . ,(/ uptrend lrates))
    	(:bearish . ,(/ downtrend lrates))
    	(:stagnated . ,(/ stagnated lrates))))))
;; (time (score-rates (get-input-dataset *rates* 20)))

(defun get-types-score (scored-rates types)
  (loop for type in types sum (assoccess scored-rates type)))

(defun get-rates-chunk-of-types (rates types &key (min-chunk-size 100) (max-chunk-size 500)
					       (chunk-step 20) (slide-step 20) (lookahead 10) (stagnation-threshold 0.5))
  (let ((score 0)
	(winner-from 0)
	(winner-to 0))
    (loop for chunk-plus from min-chunk-size below (- max-chunk-size min-chunk-size) by chunk-step
	  do (loop for idx from 0 below (- (length rates) chunk-plus) by slide-step
		   do (let ((s (get-types-score (score-rates (subseq rates idx (+ idx chunk-plus)) :lookahead lookahead :stagnation-threshold stagnation-threshold) types)))
			(when (> s score)
			  (setf score s)
			  (setf winner-from idx)
			  (setf winner-to (+ idx chunk-plus))))))
    (values winner-from winner-to)))

;; (get-rates-chunk-of-types *rates* '(:bullish) :min-chunk-size 300 :max-chunk-size 601 :slide-step 50)

;; (get-rates-chunk-of-types (subseq *rates* 0 500) '(:bullish))
;; (get-rates-chunk-of-types *rates* '(:bullish))

(defun winning-type-output-dataset (rates type-groups &key (min-dataset-size 24) (max-dataset-size 100) (chunk-step 5) (lookahead 10) (stagnation-threshold 0.5))
  "Calculates what's the ideal index when calling GET-OUTPUT-DATASET, where an ideal index is one that makes GET-OUTPUT-DATASET return a subset of RATES that has a good ratio of uptrend chunks, downtrend chunks and stagnated chunks of RATES."
  (let* ((score 0)
	 (winner-types)
	 (winner-chunk-size))
    (loop for chunk-size from max-dataset-size downto min-dataset-size by chunk-step
	  do (let ((rates (last rates chunk-size)))
	       (loop for types in type-groups
		     do (let ((s (get-types-score
				  (score-rates rates :lookahead lookahead :stagnation-threshold stagnation-threshold)
				  ;; We can send single type, e.g. (:bullish) or collection of types, e.g. '(:bullish :bearish).
				  types)))
			  (when (> s score)
			    (setf score s)
			    (setf winner-chunk-size chunk-size)
			    (setf winner-types types))))))
    winner-types))

;; (defparameter *rates* (get-random-rates-count-big :EUR_USD :H1 3000))

(defun con (y x0 x1)
  (let* ((m (/ 1
	       (- x1 x0)))
	 (b (+ (* m (- x0)) 0)))
    (/ (- y b) m)))
;; (con 0.0 -5 0)

(defun ant (x x0 x1)
  (if (= (- x1 x0) 0)
      0
      (let* ((m (/ 1 (- x1 x0)))
	     (b (+ (* m (- x0)) 0)))
	(+ (* m x) b))))
;; (ant 3 0 5)

;; (defun make-antecedent (mean spread)
;;   (lambda (x) (exp (* -0.5 (expt (/ (- x mean) spread) 2)))))

;; Keeping for historical reasons.
;; (defun ifis (i antecedents consequents)
;;   (let ((winner-gm 0)
;; 	(winner-idx 0))
;;     (loop
;;        for idx from 0
;;        for ant across antecedents
;;        do (let ((gm (funcall ant i)))
;; 	    (when (and (<= gm 1)
;; 		       (>= gm 0)
;; 		       (>= gm winner-gm))
;; 	      (setf winner-idx idx)
;; 	      (setf winner-gm gm))))
;;     (funcall (aref consequents winner-idx) winner-gm)))

(defun eval-ifis (inputs input-antecedents input-consequents)
  (let ((tp 0)
	(sl 0)
	(activation 0)
	(len (length inputs)))
    (loop
      for input in inputs
      for antecedents across input-antecedents
      for consequents across input-consequents
      do (let ((winner-gm 0)
	       (winner-idx 0))
	   (loop
	     for idx from 0
	     for ant across antecedents
	     do (let ((gm (ant input (aref ant 0) (aref ant 1))))
		  ;; Antecedents will always work with OR (max).
		  (when (and (<= gm 1)
			     (>= gm 0)
			     (>= gm winner-gm))
		    (setf winner-idx idx)
		    (setf winner-gm gm))))
	   ;; Averaging outputs (tp and sl).
	   (incf activation winner-gm)
	   (incf tp (con winner-gm
			 (aref (aref (aref consequents winner-idx) 0) 0)
			 (aref (aref (aref consequents winner-idx) 0) 1)))
	   (incf sl (con winner-gm
			 (aref (aref (aref consequents winner-idx) 1) 0)
			 (aref (aref (aref consequents winner-idx) 1) 1)))))
    (values (/ tp len) (/ sl len) (/ activation len))))

;; Tipping problem.
;; Food Q, Service Q
;; (loop for f from 0 upto 10
;;    do (progn
;; 	(loop for s from 0 upto 10
;; 	 do (let ((inputs `(,f ,s))
;; 		  (input-antecedents (vector (vector #(0 4)
;; 						     #(3 7)
;; 						     #(6 10))
;; 					     (vector #(0 4)
;; 						     #(3 7)
;; 						     #(6 10))
;; 					     ))
;; 		  (input-consequents (vector (vector (vector #(0 5) #(0 1))
;; 						     (vector #(5 10) #(0 1))
;; 						     (vector #(10 15) #(0 1)))
;; 					     (vector (vector #(0 0.01) #(0 1))
;; 						     (vector #(5 10) #(0 1))
;; 						     (vector #(10 15) #(0 1))))))
;; 	      (format t "~2$, " (float (eval-ifis inputs input-antecedents input-consequents)))))
;; 	(format t "~%")))

(defun make-ifis (agent num-rules rates)
  "Analytical version."
  (let* ((perception-fn (get-perception-fn agent))
	 (lookahead-count (slot-value agent 'lookahead-count))
	 (lookbehind-count (slot-value agent 'lookbehind-count))
	 (idxs (remove-duplicates (get-same-direction-outputs-idxs rates num-rules :lookahead-count lookahead-count :lookbehind-count lookbehind-count)))
	 (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
	 (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count)))
	 ;; (inp-sd (mapcar (lambda (inp) (standard-deviation inp)) (apply #'mapcar #'list chosen-inputs)))
	 ;; (tps (loop for output in chosen-outputs collect (assoccess output :tp)))
	 ;; (sls (loop for output in chosen-outputs collect (assoccess output :sl)))
	 ;; (tp-sd (standard-deviation tps))
	 ;; (sl-sd (standard-deviation sls))
	 ;; (mn-inp (- (apply #'min (flatten chosen-inputs)) (apply #'max inp-sd)))
	 ;; (mx-inp (+ (apply #'max (flatten chosen-inputs)) (apply #'max inp-sd)))
	 ;; (mn-out-tp (let ((min-chosen (apply #'min tps)))
	 ;; 	      (if (and (> min-chosen 0) (< (- min-chosen tp-sd) 0))
	 ;; 		  0.0
	 ;; 		  (- min-chosen tp-sd))))
	 ;; (mn-out-sl (let ((min-chosen (apply #'min sls)))
	 ;; 	      (if (and (> min-chosen 0) (< (- min-chosen sl-sd) 0))
	 ;; 		  0.0
	 ;; 		  (- min-chosen sl-sd))))
	 ;; (mx-out-tp (let ((max-chosen (apply #'max tps)))
	 ;; 	      (if (and (< max-chosen 0) (> (+ max-chosen tp-sd) 0))
	 ;; 		  0.0
	 ;; 		  (+ max-chosen tp-sd))))
	 ;; (mx-out-sl (let ((max-chosen (apply #'max sls)))
	 ;; 	      (if (and (< max-chosen 0) (> (+ max-chosen sl-sd) 0))
	 ;; 		  0.0
	 ;; 		  (+ max-chosen sl-sd))))
	 )
    (values
     (let ((v (loop
		for inputs in (apply #'mapcar #'list chosen-inputs)
		for idx from 0
		collect (let ((v (flatten (loop
					    for input in inputs
					    collect (list
						     (if (plusp input)
							 (vector 0 input)
							 (vector input 0))
						     ;; (vector (- input (nth idx inp-sd)) input)
						     ;; (vector input (+ input (nth idx inp-sd)))
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
				       (list (vector (vector ;; (if (and nil (< mn-tp mn-out-tp))
						      ;;     mn-out-tp
						      ;;     mn-tp)
						      0 tp
						      )
						     (vector ;; (if (and nil (< mn-sl mn-out-sl))
						      ;;     mn-out-sl
						      ;;     mn-sl)
						      0 sl
						      ;; (if (plusp sl)
						      ;;     0
						      ;;     sl)
						      ;; (if (plusp sl)
						      ;;     sl
						      ;;     0)
						      ))
					     ;; (vector (vector tp
					     ;; 		 ;; (if (and nil (> mx-tp mx-out-tp))
					     ;; 		 ;;     mx-out-tp
					     ;; 		 ;;     mx-tp)
					     ;; 		 0)
					     ;; 	 (vector sl
					     ;; 		 ;; (if (and nil (> mx-sl mx-out-sl))
					     ;; 		 ;;     mx-out-sl
					     ;; 		 ;;     mx-sl)
					     ;; 		 0
					     ;; 		 )
					     ;; 	 )
					     )))))
	    (one-set-outputs-v (make-array (length one-set-outputs) :initial-contents one-set-outputs))
	    (v (loop repeat (length (first chosen-inputs))
		     collect one-set-outputs-v)))
       (make-array (length v) :initial-contents v)))))
;; (make-ifis (gen-agent 3 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 3 *rates*)
;; (slot-value (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 'perception-fns)
;; (insert-agent (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 16) :EUR_USD :H1 '(:BULLISH))
