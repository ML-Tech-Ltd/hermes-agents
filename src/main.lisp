;; (ql:quickload :overmind-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (loop-optimize-test 1000 :instruments '(:AUD_USD))
;; (loop-optimize-test)
;; (loop-optimize-test 50 :instruments-keys '(:forex) :timeframes-keys '(:all))
;; (loop-optimize-test 50 :instruments-keys '(:all) :timeframes-keys '(:longterm))
;; (loop-optimize-test 50 :instruments-keys '(:metals) :timeframes-keys '(:longterm))
;; (prune-populations)
;; (drop-populations)
;; (drop-tests)

(defpackage overmind-agents
  (:use :cl
	:access
 	:lparallel
        :postmodern
	:alexandria
        :computable-reals
	:random-state
	:defenum
	:fare-mop
	:overmind-code
	:overmind-input
	:overmind-perception
	:overmind-intuition
	:overmind-agents.config
	:overmind-agents.db
	:overmind-agents.km
	:overmind-agents.utilities
	)
  (:export :get-trades
	   :loop-optimize-test
	   :read-log
	   :read-agents-log)
  (:nicknames :omage))
(in-package :overmind-agents)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
			    (if (/= ideal-cores-count 0)
				ideal-cores-count 1))))

(defun assoccess (o k)
  (cdr (assoc k o)))

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

;; (rate-close (first *rates*))
;; (rate-high (first *rates*))

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

;; (ql:update-all-dists)

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

;; (time (get-rates :EUR_USD 2 :H1))
;; (defparameter *rates* (get-rates-count :EUR_USD :H1 30 :provider :oanda :type :fx))

;; (defun )

;; (local-time:unix-to-timestamp )

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (conn
   (unless (table-exists-p 'rates)
     (query (:create-table 'rates
			   ((time :type (or db-null int8))
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
			    (volume :type double-float))
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
			    (avg-max-pos :type (or db-null double-float))
			    (stdev-max-pos :type (or db-null double-float))
			    (avg-max-neg :type (or db-null double-float))
			    (stdev-max-neg :type (or db-null double-float))
			    (avg-tp :type (or db-null double-float))
			    (stdev-tp :type (or db-null double-float))
			    (avg-sl :type (or db-null double-float))
			    (stdev-sl :type (or db-null double-float))
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
			    (sls :type (or db-null float[])))
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
			    (train-begin-time :type int8)
			    (train-end-time :type int8)
			    (test-begin-time :type int8)
			    (test-end-time :type int8)
			    (train-dataset-size :type integer)
			    (test-dataset-size :type integer)
			    (train-avg-revenue :type double-float)
			    (test-avg-revenue :type double-float)
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
			    (test-sls :type float[]))
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

;; (conn (query (:select (:sum 'result) :from 'trades)))
;; (conn (query (:select 'result :from 'trades)))

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
  (let ((trades (conn (query (:order-by (:select '*
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
			     :alists))))
    (loop for trade in trades
       summing (let ((instrument (make-keyword (assoccess trade :instrument))))
		 ;; (if (or (and (string= (assoccess trade :decision) "SELL")
		 ;; 		 (string= (assoccess trade :type) "BEARISH"))
		 ;; 	    (and (string= (assoccess trade :decision) "BUY")
		 ;; 		 (string= (assoccess trade :type) "BULLISH")))
		 ;; 	(to-pips instrument (assoccess trade :result))
		 ;; 	0)
		 (to-pips instrument (assoccess trade :result))))))

;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 3 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 3 :day)))
;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 4 :day)))

;; Running hours.
;; (loop for i from 0 below 72 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :hour)))))

;; Per day.
;; (loop for i from 0 below 10 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) (1+ i) :day)))))

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
										:where (:is-null 'trades.result)
										)
								       'trades.id
								       )
							    'results))
					(:desc 'creation-time))
			     :alists))
	  ))
    (push-to-log (format nil "Trying to validate ~a trades." (length trades)))
    (loop for trade in trades
       do (let* ((from (* (assoccess trade :creation-time) 1000000))
		 (from-timestamp (local-time:unix-to-timestamp (/ from 1000000))))
	    (when (local-time:timestamp< from-timestamp
					 (local-time:timestamp- (local-time:now) 1 :day))
	      (push-to-log (format nil "Using minute rates from ~a to ~a to validate trade."
				   from-timestamp
				   (local-time:timestamp+ from-timestamp 3 :day)))
	      (let* ((instrument (make-keyword (assoccess trade :instrument)))
		     (timeframe :M1)
		     ;; (to (* (local-time:timestamp-to-unix (local-time:timestamp+ from-timestamp 3 :day)) 1000000))
		     ;; (rates (get-rates-range instrument timeframe from to :provider :oanda :type :fx))
		     (rates (get-rates-count-from instrument timeframe 5000 from :provider :oanda :type :fx))
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

;; (let ((fn '((:fn . ->sma-close) (:offset . 0) (:n . 20))))
;;   (apply #'funcall (assoccess fn :fn) *rates* (flatten (rest fn))))

;; (time (loop repeat 1000 do (funcall (gen-perception-fn #(#(0 0 10) #(0 1 10) #(1 0))) *rates*)))

;; (let ((perc-fn (gen-perception-fn #(#(0 0 10) #(0 1 10) #(0 1 10) #(0 1 10) #(0 1 10) #(0 1 10) #(0 1 10) #(0 1 10) #(0 1 10) #(1 0)))))
;;   (time (loop repeat 1000 do (funcall perc-fn *rates*))))

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
   (end-time :col-type (or db-null int8) :initarg :train-end-time :initform :null)
   (dataset-size :col-type (or db-null integer) :initarg :test-dataset-size :initform :null)
   (avg-revenue :col-type (or db-null double-float) :initarg :test-avg-revenue :initform :null)
   (stdev-revenue :col-type (or db-null double-float) :initarg :test-stdev-revenue :initform :null)
   (total-revenue :col-type (or db-null double-float) :initarg :test-total-revenue :initform :null)
   (avg-max-pos :col-type (or db-null double-float) :initarg :test-avg-max-pos :initform :null)
   (stdev-max-pos :col-type (or db-null double-float) :initarg :test-stdev-max-pos :initform :null)
   (avg-max-neg :col-type (or db-null double-float) :initarg :test-avg-max-neg :initform :null)
   (stdev-max-neg :col-type (or db-null double-float) :initarg :test-stdev-max-neg :initform :null)
   (avg-tp :col-type (or db-null double-float) :initarg :test-avg-tp :initform :null)
   (stdev-tp :col-type (or db-null double-float) :initarg :test-stdev-tp :initform :null)
   (avg-sl :col-type (or db-null double-float) :initarg :test-avg-sl :initform :null)
   (stdev-sl :col-type (or db-null double-float) :initarg :test-stdev-sl :initform :null)
   (max-tp :col-type (or db-null double-float) :initarg :test-max-tp :initform :null)
   (min-tp :col-type (or db-null double-float) :initarg :test-min-tp :initform :null)
   (max-sl :col-type (or db-null double-float) :initarg :test-max-sl :initform :null)
   (min-sl :col-type (or db-null double-float) :initarg :test-min-sl :initform :null)
   (trades-won :col-type (or db-null integer) :initarg :test-trades-won :initform :null)
   (trades-lost :col-type (or db-null integer) :initarg :test-trades-lost :initform :null)
   (revenues :col-type (or db-null float[]) :initarg :test-revenues :initform :null)
   (entry-times :col-type (or db-null int8[]) :initarg :test-entry-times :initform :null)
   (exit-times :col-type (or db-null int8[]) :initarg :test-exit-times :initform :null)
   (entry-prices :col-type (or db-null float[]) :initarg :test-entry-prices :initform :null)
   (exit-prices :col-type (or db-null float[]) :initarg :test-exit-prices :initform :null)
   (tps :col-type (or db-null float[]) :initarg :test-tps :initform :null)
   (sls :col-type (or db-null float[]) :initarg :test-sls :initform :null))
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
   (test-sls :col-type float[] :initarg :test-sls))
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

;; (funcall (assoccess *perception-fn* :perception-fn) (subseq *rates* 10 20))

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

;; (funcall (accesses (make-instance 'agent) :beliefs :perception-fn) *rates*)

;; (->delta-close-bid (subseq *rates* 0 20) :offset 0)

(defun get-input-dataset (rates idx)
  (subseq rates 0 idx))

(defun get-output-dataset (rates idx)
  (subseq rates idx))

(defun eq-line-two-points (a b)
  "Calculates the slope `M` and constant `B` of a line equation
`Y = MX + B`, given two points `A` and `B` in a plane."
  (let* ((m (/ (- (assoccess b :y) (assoccess a :y))
	       (- (assoccess b :x) (assoccess a :x))))
	 (b (- (assoccess a :y) (* m (assoccess a :x)))))
    `((:m . ,m) (:b . ,b))))
;; (eq-line-two-points '(:x 3 :y 7) '(:x 5 :y 11))

(defun plot-poly (poly)
  (apply #'plotly-make-plot
	 (plotly-layout)
	 (loop for line in poly
	    collect (let* ((x1 (min (-> line :x1) (-> line :x2)))
			   (x2 (max (-> line :x1) (-> line :x2)))
			   (y1 (if (= x1 (-> line :x1)) (-> line :y1) (-> line :y2)))
			   (y1 (if (= x2 (-> line :x1)) (-> line :y1) (-> line :y2))))
		      (plotly-line (list x1 x2)
				   (list (-> line :y1)
					 (-> line :y2)))))))

;; (plot-poly ')

;; (loop for i below 100 do (gen-ifis (make-instance 'agent) 3 *rates*))

;; (alpha-cut 0.3 (gen-ifis (make-instance 'agent) 3 *rates*))

;; (length (-> (gen-ifis (make-instance 'agent) 4 *rates*) :rules))

;; (if-coa (alpha-cut 0.7 (-> (first (first (-> (gen-if-system (make-instance 'agent) 3 *rates*) :rules))) :antecedent))
;; 	 )
;; (if-coa (fire-rule -0.0001 (first (first (-> (gen-ifis (make-instance 'agent) 3 *rates*) :rules)))))

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
  (length (get-agent-ids-from-patterns instrument timeframe types)))

(defun describe-agents ()
  (format nil "<b>AGENTS POOL.</b><br/><hr/><br/>~{~a<br/>~}<br/>"
	  (flatten (loop for instrument in ominp:*forex*
		      collect (loop for types in '((:bullish) (:bearish) (:stagnated))
				 collect (format nil "~a, ~a, ~a" instrument types (get-agents-count instrument :H1 types)))))))
;; (describe-agents)

(defun describe-agents ()
  (with-open-stream (s (make-string-output-stream))
    (format s "<h3>AGENTS POOL</h3><hr/>")
    (loop for instrument in ominp:*forex*
       do (loop for types in '((:bullish) (:bearish) (:stagnated))
	     do (let* ((agents-props (prepare-agents-properties (get-agents instrument :H1 types :limit -1)))
		       (agents-count (get-agents-count instrument :H1 types))
		       (vals (loop for agent-props in agents-props
				collect (let ((avg-tp (read-from-string (assoccess agent-props :test-avg-tp)))
					      (avg-sl (read-from-string (assoccess agent-props :test-avg-sl))))
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

(defun unix-from-nano (unix-nano &optional (is-string? nil))
  (if is-string?
      (/ (read-from-string unix-nano) 1000000)
      (/ unix-nano 1000000)))

(defun unix-to-nano (unix &optional (is-string? nil))
  (if is-string?
      (* (read-from-string unix) 1000000)
      (* unix 1000000)))

(defparameter +CELL-FORMATS+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

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

(defun prepare-agents-properties (agents)
  (loop for agent in agents
     collect (loop for (key value) on (collect-slots agent) by #'cddr
		unless (or (string= key "CREATION-BEGIN-TIME")
			   (string= key "CREATION-END-TIME")
			   (string= key "TRAIN-BEGIN-TIME")
			   (string= key "TRAIN-END-TIME")
			   (string= key "BEGIN-TIME")
			   (string= key "END-TIME")
			   (string= key "TEST-STDEV-MAX-POS")
			   (string= key "TEST-AVG-MAX-POS")
			   (string= key "TEST-STDEV-MAX-NEG")
			   (string= key "TEST-AVG-MAX-NEG")

			   (string= key "PERCEPTION-FNS")
			   (string= key "LOOKAHEAD-COUNT")
			   (string= key "LOOKBEHIND-COUNT")
			   (string= key "ANTECEDENTS")
			   (string= key "CONSEQUENTS")
			   (string= key "TEST-REVENUES")
			   (string= key "TEST-ENTRY-TIMES")
			   (string= key "TEST-EXIT-TIMES")
			   (string= key "TEST-ENTRY-PRICES")
			   (string= key "TEST-EXIT-PRICES")
			   (string= key "TEST-TPS")
			   (string= key "TEST-SLS"))
		collect (let ((value (cond ((or (string= key "CREATION-BEGIN-TIME")
						(string= key "CREATION-END-TIME")
						(string= key "BEGIN-TIME")
						(string= key "END-TIME")
						(string= key "TRAIN-BEGIN-TIME")
						(string= key "TRAIN-END-TIME"))
					    (local-time:unix-to-timestamp
					     (unix-from-nano value)))
					   ((floatp value) (format nil "~6$" value))
					   (t value))))
			  ;; (format nil "~a: ~a~%" key value)
			  `(,key . ,value)))))

(defun alist-keys (alist)
  (loop for item in alist collect (car item)))
(defun alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-keys (car (prepare-agents-properties (get-agents :EUR_USD :H1 '(:bullish)))))
;; (alist-values (car (prepare-agents-properties (get-agents :EUR_USD :H1 '(:bullish)))))

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

;; (conn (query (:limit (:order-by (:select '* :from 'trades) (:desc 'trades.creation-time)) 2) :alists))

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
;; (get-global-revenue)
;; (describe-agents)
;; (describe-trades 1000)

;; (conn (query (:select 'agent-id :from 'agents-patterns)))
;; TODO: when removing an agent in update-pareto-frontier, also remove agent-pattern

(defun get-perception-fn (agent)
  (gen-perception-fn (read-from-string (slot-value agent 'perception-fns))))
;; (fare-memoization:memoize 'get-perception-fn)

(defun eval-agent (agent rates)
  (let ((perception-fn (get-perception-fn agent)))
    (eval-ifis (funcall perception-fn rates)
	       (read-from-string (slot-value agent 'antecedents))
	       (read-from-string (slot-value agent 'consequents)))))

(defun eval-agents (instrument timeframe types rates &key (count 1) (limit 10))
  (let (tps sls activations ids)
    (loop for offset from 0 below (get-agents-count instrument timeframe types) by limit
       do (let ((agents (get-agents instrument timeframe types :limit limit :offset offset)))
	    (loop for agent in agents
	       do (multiple-value-bind (tp sl activation)
		      (eval-agent agent rates)
		    (push tp tps)
		    (push sl sls)
		    (push activation activations)
		    (push (slot-value agent 'id) ids)
		    ))))
    ;; (format t "~a, ~a~%" (apply #'min activations) (apply #'max activations))
    (let ((idxs (sorted-indexes activations #'>))
	  (tp 0)
	  (sl 0)
	  (len (min count (length activations))))
      (setf tp (nth (position 0 idxs) tps))
      (setf sl (nth (position 0 idxs) sls))
      ;; (loop for idx from 1 below len
      ;; 	 do (let* ((pos (position idx idxs))
      ;; 		   (nth-tp (nth pos tps))
      ;; 		   (nth-sl (nth pos sls)))
      ;; 	      (when (< (* nth-tp tp) 0)
      ;; 	      	(setf tp 0)
      ;; 	      	(setf sl 0)
      ;; 	      	(return))
      ;; 	      (when (< (* nth-sl sl) 0)
      ;; 	      	(setf tp 0)
      ;; 	      	(setf sl 0)
      ;; 	      	(return))
      ;; 	      ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      ;; 	      ;; 	(setf tp nth-tp))
      ;; 	      ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      ;; 	      ;; 	(setf sl nth-sl))
      ;; 	      ;; (when (or (= tp 0) (< (abs nth-tp) (abs tp)))
      ;; 	      ;; 	(setf tp nth-tp))
      ;; 	      ;; (when (or (= sl 0) (< (abs nth-sl) (abs sl)))
      ;; 	      ;; 	(setf sl nth-sl))
      ;; 	      ;; (incf tp nth-tp)
      ;; 	      ;; (incf sl nth-sl)
      ;; 	      ))
      (values (/ tp 1)
	      (/ sl 1)
	      (list (nth (position 0 idxs) ids))))))

(defun -evaluate-agents (&key instrument timeframe types rates agent idx)
  "Used for EVALUATE-AGENT and EVALUATE-AGENTS."
  (push-to-log "Trying to evaluate agents.")
  (let* ((idx (if idx idx (get-max-lookbehind instrument timeframe types)))
	 (revenues)
	 (max-poses)
	 (max-negses)
	 (trades-won 0)
	 (trades-lost 0)
	 (entry-prices)
	 (exit-prices)
	 (tps)
	 (sls)
	 (entry-times)
	 (exit-times))
    (push-to-log "Evaluating agent: ")
    (loop while (< idx (length rates))
       do (let* ((input-dataset (get-input-dataset rates idx))
		 (output-dataset (get-output-dataset rates idx)))
	    (multiple-value-bind (tp sl)
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
		    (push-to-log "." :add-newline? nil)
		    (progn
		      ;; (format t "TP: ~a, SL: ~a, R: ~a~%" tp sl revenue)
		      (push-to-log "+" :add-newline? nil)
		      (if (> revenue 0)
			  (incf trades-won)
			  (incf trades-lost))
		      (push tp tps)
		      (push sl sls)
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
    (push-to-log "<br/>Agents evaluated successfully.")
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
      (:sls . ,(reverse sls)))))

;; (evaluate-agents :EUR_USD :H1 '(:BULLISH) *rates*)

;; (room)

(defun evaluate-agent (agent rates)
  (let ((fitnesses (-evaluate-agents :agent agent :rates rates :idx (slot-value agent 'lookbehind-count))))
    (loop for fitness in fitnesses
       ;; TODO: We should be returning symbols (not kws) from -evaluate-agents.
	 ;;; and then remove this format + read-from-string.
       do (setf (slot-value agent (read-from-string (format nil "~a" (car fitness))))
		(if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    agent))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(defun evaluate-agents (instrument timeframe types rates)
  (-evaluate-agents :instrument instrument :timeframe timeframe :types types :rates rates))
;; (time (evaluate-agents :EUR_USD :H1 '(:BULLISH) (subseq *rates* 0 200)))

(defun update-agent-fitnesses (agent rates)
  (conn (update-dao (evaluate-agent agent rates))))

(defun update-agents-fitnesses (agents rates)
  (loop for agent in agents
     do (update-agent-fitnesses agent rates))
  agents)

;; (let ((agents (get-agents :EUR_USD :H1 '(:BULLISH) :limit -1)))
;;   (print (slot-value (first agents) :test-avg-revenue))
;;   (update-agents-fitnesses agents (subseq *rates* 0 300))
;;   (print (slot-value (first agents) :test-avg-revenue))
;;   nil
;;   )

;; (evaluate-agents (gen-agents 1 4 *input-dataset*) *output-dataset*)

(defun get-max-lookbehind (instrument timeframe types)
  (let ((agent-ids (get-agent-ids-from-patterns instrument timeframe types)))
    (caar (conn (query (:select (:max 'lookbehind-count) :from 'agents :where (:in 'id (:set agent-ids))))))))
;; (get-max-lookbehind :EUR_USD :H1 '(:BULLISH))
;; (get-agent-ids-from-patterns :EUR_USD :H1 '(:BULLISH))

(defun merge-keywords (kw1 kw2)
  (read-from-string (format nil ":~a-~a" kw1 kw2)))
;; (merge-keywords :hello :moto)

(defun vector-1-similarity (vec1 vec2)
  (loop for v across vec1 do (when (position v vec2) (return t))))
;; (vector-1-similarity #(1 2 3) #(10 20 30))

(defun is-agent-dominated? (agent agents)
  (let* ((agent-id-0 (slot-value agent 'id))
	 (avg-revenue-0 (slot-value agent 'avg-revenue))
	 (trades-won-0 (slot-value agent 'trades-won))
	 (trades-lost-0 (slot-value agent 'trades-lost))
	 ;; (stdev-revenue-0 (slot-value agent 'stdev-revenue))
	 (entry-times-0 (slot-value agent 'entry-times))
	 (is-dominated? nil))
    ;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
    (loop for agent in agents
       do (let* ((agent-id (slot-value agent 'id))
		 (avg-revenue (slot-value agent 'avg-revenue))
		 (trades-won (slot-value agent 'trades-won))
		 (trades-lost (slot-value agent 'trades-lost))
		 ;; (stdev-revenue (slot-value agent 'stdev-revenue))
		 (entry-times (slot-value agent 'entry-times))
		 )
	    ;; Fitnesses currently being used.
	    (when (and (>= avg-revenue avg-revenue-0)
		       ;; (< stdev-revenue stdev-revenue-0)
		       (>= trades-won trades-won-0)
		       (<= trades-lost trades-lost-0)
		       ;; (>= (/ trades-won
		       ;; 	      (+ trades-won trades-lost))
		       ;; 	   (/ trades-won-0
		       ;; 	      (+ trades-won-0 trades-lost-0)))
		       (vector-1-similarity entry-times entry-times-0))
	      ;; Candidate agent was dominated.
	      (let ((metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST")))
		(with-open-stream (s (make-string-output-stream))
		  ;; (push-to-agents-log )
		  (format s "<pre><b>(BETA) </b>Agent ID ~a~%" agent-id-0)
		  (format-table s `((,(format nil "~6$" avg-revenue-0) ,trades-won-0 ,trades-lost-0)) :column-label metric-labels)
		  (format s "</pre>")

		  (format s "<pre><b>(ALPHA) </b>Agent ID ~a~%" agent-id)
		  (format-table s `((,(format nil "~6$" avg-revenue) ,trades-won ,trades-lost)) :column-label metric-labels)
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
	      (return))))
    is-dominated?))

(defun plotly-candlestick (rates)
  (let ((x (loop for rate in rates collect (/ (read-from-string (assoccess rate :time)) 1000000)))
	(open (loop for rate in rates collect (assoccess rate :open-bid)))
	(high (loop for rate in rates collect (assoccess rate :high-bid)))
	(low (loop for rate in rates collect (assoccess rate :low-bid)))
	(close (loop for rate in rates collect (assoccess rate :close-bid))))
    `((:type . "candlestick")
      (:xaxis . "x")
      (:yaxis . "y")
      (:x . ,x)
      (:close . ,close)
      (:high . ,high)
      (:low . ,low)
      (:open . ,open))))

(defun plotly-line (xs ys &optional (color "red") (dash "solid"))
  `((:x . ,xs)
    (:y . ,ys)
    (:type . "scatter")
    (:line . ((:color . ,color)
	      (:dash . ,dash)))))

(defun plotly-make-plot (layout &rest traces)
  (plotly-cl:pl-plot traces :layout layout :width 1700 :height 1000)
  (asdf:run-shell-command "mv ~a ~a" "/tmp/*.html" "/home/amherag/amaury/"))

(defun time-to-unix (time)
  (/ (read-from-string time) 1000000))

(defun plotly-layout ()
  `((:dragmode . "zoom")
    (:showlegend . ,*json-false*)
    (:xaxis . ((:rangeslider . ((:visible . ,*json-false*)))))
    (:yaxis . ((:fixedrange ,*json-false*)))
    (:title . "Trades")))

(defun report (agents rates)
  (let ((r (evaluate-agents agents rates)))
    (push (list (slot-value r 'avg-revenue)
		(slot-value r 'trades-won)
		(slot-value r 'trades-lost))
	  *results*)
    (format t "#Agents: ~4a ~tAvg. Revenue: ~5$ ~tTrades Won: ~4a ~tTrades Lost: ~4a~%"
	    (length agents)
	    (slot-value r 'avg-revenue)
	    (slot-value r 'trades-won)
	    (slot-value r 'trades-lost))))

(defun optimization (instrument timeframe types gen-agent-fn rates seconds &key report-fn)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe types :limit -1)))
		  (update-agents-fitnesses agents rates)
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
			 do (unless (get-dao 'agent (slot-value agent 'id))
			      (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
			      (insert-agent agent instrument timeframe types))))
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
			      (delete-agent in-trial instrument timeframe types))
			    (push in-trial purged-agents)))
		  (push (first challenger) purged-agents)
		  (setf agents purged-agents)
		  (setf purged-agents nil)
		  )
		;; (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
		;; (insert-agent agent instrument timeframe types)
		(when report-fn
		  (funcall report-fn (get-agents instrument timeframe types :limit -1) rates)))))))

(defun insert-pattern (instrument timeframe type)
  (conn (make-dao 'pattern :type (format nil "~a" type)
		  :instrument (format nil "~a" instrument)
		  :timeframe (format nil "~a" timeframe))))

;; (conn (query (:select '* :from 'patterns) :alists))

(defun get-patterns (instrument timeframe types)
  (conn (query (:select '* :from 'patterns :where (:and (:= 'instrument (format nil "~a" instrument))
							(:= 'timeframe (format nil "~a" timeframe))
							(:in 'type (:set (loop for type in types collect (format nil "~a" type))))))
	       :alists)))
;; (get-patterns :EUR_USD :H1 '(:BULLISH :BEARISH))

(defun get-agent-ids-from-patterns (instrument timeframe types)
  (let ((patterns (get-patterns instrument timeframe types)))
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (assoccess pattern :id))))) :column))))

(defun get-agents (instrument timeframe types &key (limit 5) (offset 0))
  (let* ((agent-ids (get-agent-ids-from-patterns instrument timeframe types))
	 (agents (if (minusp limit)
		     (conn (query (:select '* :from 'agents :where (:in 'id (:set agent-ids)))
				  (:dao agent)))
		     (conn (query (:limit (:order-by (:select '* :from 'agents :where (:in 'id (:set agent-ids)))
						     'id)
					  '$1 '$2)
				  limit offset (:dao agent))))))
    agents))

;; (length (get-agents :EUR_USD :H1 '(:BULLISH) :limit 10))
;; (length (get-agents :EUR_USD :H1 '(:BULLISH) :limit -1))
;; (slot-value (car (get-agents :EUR_USD :H1 '(:BULLISH) :limit 1)) 'avg-revenue)

;; (conn (query (:select '* :from 'agents)))

;; Get number of agents. Then get agent #1, calculate, then release.
(defun remove-bad-agents (&optional (max-agents-per-pool 600))
  (loop for timeframe in ominp:*timeframes*
     do (loop for instrument in ominp:*instruments*
	   do (let* ((agents (get-agents instrument timeframe))
		     (observations (loop for agent in agents
				      ;; Fitnesses currently being used.
				      collect (let* ((fitnesses (slot-value agent 'fitnesses))
						     (avg-revenue (slot-value fitnesses 'avg-revenue))
						     (stdev-revenue (slot-value fitnesses 'stdev-revenue))
						     (trades-won (slot-value fitnesses 'trades-won))
						     (trades-lost (slot-value fitnesses 'trades-lost)))
						(list avg-revenue stdev-revenue trades-won trades-lost)))))
		;; (when agents
		;;   (insert-agents filtered-agents instrument timeframe))
		(when (> (length agents) max-agents-per-pool)
		  (let* ((clusters (km observations max-agents-per-pool))
			 (centroids (mapcar #'centroid clusters))
			 (representative-agents
			  (loop
			     for centroid in centroids
			     for cluster in clusters
			     collect (nth (position (nth (position 0 (sorted-indexes (loop
											for c in cluster
											collect (norm (vsub c centroid)))))
							 cluster)
						    observations :test #'equal)
					  agents))))
		    (insert-agents representative-agents instrument timeframe)))))))

;; (remove-bad-agents 600)
;; (loop for market in ominp:*forex*
;;    do (print (length (get-agents market :H1))))

(defun format-rr (risk reward)
  (format nil "~a / ~2$"
	  (/ (* 10000 (abs risk))
	     (* 10000 (abs risk)))
	  (/ (* 10000 (abs risk))
	     (* 10000 (abs reward)))))

(defun describe-agent-fitnesses (market)
  (loop for agent in (get-agents market :H1)
     do (let* ((fit (slot-value agent 'fitnesses))
	       (tps (access fit :tps))
	       (sls (access fit :sls)))
	  (when (and tps sls)
	    (let* ((avg-revenue (access fit :avg-revenue))
		   (avg-tp (access fit :avg-tp))
		   (avg-sl (access fit :avg-sl))
		   (trades-won (access fit :trades-won))
		   (trades-lost (access fit :trades-lost))
		   (rr `(:rr . ,(format nil "~2$ / ~2$"
					(* 10000 (abs (mean sls)))
					(* 10000 (abs (mean tps))))))
		   (rr2 `(:rr2 . ,(format-rr (mean sls) (mean tps)))))
	      (format t "AVG-REVENUE: ~t ~a~%AVG-TP: ~t ~a~%AVG-SL: ~t ~a~%TRADES-WON: ~t ~a~%TRADES-LOST: ~t ~a~%RR: ~t ~a~%RR2: ~t ~a~%~%"
		      avg-revenue
		      avg-tp
		      avg-sl
		      trades-won
		      trades-lost
		      rr
		      rr2))))))

(defun insert-agent (agent instrument timeframe types)
  (let ((agent-id (slot-value agent 'id))
	(patterns (get-patterns instrument timeframe types)))
    (conn (insert-dao agent)
	  (loop for pattern in patterns
	     do (make-dao 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id))))
    agent))

(defun delete-agent (agent instrument timeframe types)
  (let ((agent-id (slot-value agent 'id))
	(patterns (get-patterns instrument timeframe types)))
    (conn
     ;; First we remove agent-pattern.
     (loop for pattern in patterns
	do (delete-dao (make-instance 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id))))
     ;; Then we delete the agent only if there are no agent-patterns related to this agent.
     (unless (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id agent-id)))
       (delete-dao agent)))))

;; (loop for agent in (get-agents :EUR_JPY :H1 '(:BULLISH) :limit 100000)
;;       do (delete-agent agent :EUR_JPY :H1 '(:BULLISH)))

;; (conn (query (:select 'id :from 'agents) :alist))
;; (conn (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id "C3FE332F-82E7-41A6-89CA-E36550D1D687"))))


;; (insert-agent (gen-agent 3 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 55) :EUR_USD :H1 '(:BEARISH))
;; (get-agents-count :EUR_USD :H1 '(:STAGNATED :BULLISH :BEARISH))
;; (get-agents :EUR_USD :H1 '(:STAGNATED :BULLISH :BEARISH) :limit 2)
;; (get-agents :EUR_USD :H1 '(:STAGNATED))
;; (get-agents :EUR_USD :H1 '(:BULLISH))
;; (get-agents :EUR_USD :H1 '(:BEARISH))

(defun is-market-close ()
  (let ((day-of-week (local-time:timestamp-day-of-week (local-time:now) :timezone local-time:+utc-zone+))
	(hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (or
     ;; Friday
     (and (= day-of-week 5)
	  (>= hour 20))
     ;; Saturday
     (= day-of-week 6)
     ;; Sunday
     (and (= day-of-week 0)
	  (< hour 21)))))
;; (is-market-close)

;; (time (get-last-tests ominp:*forex* ominp:*timeframes* 3))

;; (accesses (first (get-last-tests '(:EUR_USD :GBP_USD) '(:H1 :D) 3)) :fitnesses :min-tp)

;; (get-last-tests '(:EUR_USD :GBP_USD) '(:H1 :D) 3)
;; (get-last-tests '(:GBP_USD) '(:H1))

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
			       )))
	  (loop for pattern in patterns
	     do (make-dao 'pattern-trade
			  :pattern-id (assoccess pattern :id)
			  :trade-id (slot-value trade 'id)))
	  )))

;; (require 'sb-sprof)
;; (sb-sprof:start-profiling)
;; (sb-sprof:report :type :flat)

;; (ql:quickload :function-cache)
;; (function-cache:purge-all-caches)

(defun test-agents (instrument timeframe types rates training-dataset testing-dataset)
  (multiple-value-bind (tp sl agent-ids)
      (eval-agents instrument timeframe types testing-dataset)
    (let* ((train-fitnesses (evaluate-agents instrument timeframe types training-dataset))
	   (test-fitnesses (evaluate-agents instrument timeframe types testing-dataset)))
      (when train-fitnesses
	(push-to-log "Training process successful."))
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
	(insert-trade (first agent-ids) instrument timeframe types train-fitnesses test-fitnesses tp sl rates)
	(push-to-log "Trade created successfully.")))))

;; General log.
(let (log)
  (defun push-to-log (msg &key (add-newline? t) (size 100000))
    (if add-newline?
	(push (format nil "~a<br/>" msg) log)
	(push (format nil "~a" msg) log))
    (when (> (length log) size)
      (setf log (butlast log))))
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
  (defun push-to-agents-log (msg &key (add-newline? t) (size 100000))
    (if add-newline?
	(push (format nil "~a<br/>" msg) log)
	(push (format nil "~a" msg) log))
    (when (> (length log) size)
      (setf log (butlast log))))
  (defun read-agents-log ()
    (format nil "~a<h3>AGENTS LOG.</h3><hr/><br/>~{~a~%~}"
	    (describe-agents)
	    (reverse log)
	    ))
  (defun clear-agents-log ()
    (setf log nil)))

(defun clear-logs ()
  (clear-log)
  (clear-agents-log))

(defun init ()
  (bt:make-thread
   (lambda ()
     (swank:create-server :port 4444))))
(init)

(defun -loop-optimize-test (&key
			      (seconds 20)
			      (max-creation-dataset-size 1000)
			      (max-training-dataset-size 1000)
			      (max-testing-dataset-size 200)
			      (num-rules 100)
			      (num-inputs 5)
			      (report-fn nil)
			      (type-groups '((:bullish) (:bearish) (:stagnated)))
			      (instruments ominp:*forex*)
			      (timeframes ominp:*shortterm*))
  (dolist (instrument instruments)
    (dolist (timeframe timeframes)
      (unless (is-market-close)
	(push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
	(let* ((rates (if *is-production*
			  (get-rates-count instrument timeframe
					   (+ max-creation-dataset-size max-training-dataset-size max-testing-dataset-size)
					   :provider :oanda :type :fx)
			  (get-random-rates-count instrument timeframe
						  (+ max-creation-dataset-size max-training-dataset-size max-testing-dataset-size)
						  :provider :oanda :type :fx)))
	       (dataset-size (length rates)))
	  (push-to-log "Retrieved rates successfully.")
	  (let ((full-training-dataset (subseq rates
					       (- dataset-size
						  max-testing-dataset-size
						  max-training-dataset-size)
					       (- dataset-size
						  max-testing-dataset-size)))
		(full-creation-dataset (subseq rates
					       0
					       (- dataset-size
						  max-testing-dataset-size
						  max-training-dataset-size)))
		(testing-dataset (let ((dataset (subseq rates
							(- dataset-size
							   max-testing-dataset-size))))
				   (push-to-log (format nil "Testing dataset created successfully. Size: ~s. Dataset from ~a to ~a."
							(length dataset)
							(local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
							(local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))))
				   dataset))
		(agents-count (get-agents-count instrument timeframe (flatten type-groups))))
	    (if (and *is-production* (> agents-count 0))
		(progn
		  (push-to-log "<b>SIGNAL.</b><hr/>")
		  (push-to-log (format nil "Trying to create signal with ~a agents." agents-count))
		  (test-agents instrument timeframe (flatten type-groups) rates full-training-dataset testing-dataset))
		(push-to-log "Not enough agents to create a signal."))
	    (loop for types in type-groups
	       ;; (multiple-value-bind (types)
	       ;;     (winning-type-output-dataset rates type-groups
	       ;;    :min-dataset-size max-testing-dataset-size
	       ;;    :max-dataset-size max-testing-dataset-size)
	       do (let* ((training-dataset (multiple-value-bind (from to)
					       (get-rates-chunk-of-types full-training-dataset types
									 :min-chunk-size 300
									 :max-chunk-size max-creation-dataset-size)
					     (let ((dataset (subseq full-training-dataset from to)))
					       (push-to-log (format nil "Training dataset created successfully. Size: ~s. Dataset from ~a to ~a."
								    (length dataset)
								    (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
								    (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))
								    ))
					       dataset)))
			 (creation-dataset (multiple-value-bind (from to)
					       (get-rates-chunk-of-types full-creation-dataset types
									 :min-chunk-size 300
									 :max-chunk-size max-creation-dataset-size)
					     (let ((dataset (subseq full-creation-dataset from to)))
					       (push-to-log (format nil "Creation dataset created successfully. Size: ~s. Dataset from ~a to ~a."
								    (length dataset)
								    (local-time:unix-to-timestamp (/ (read-from-string (assoccess (first dataset) :time)) 1000000))
								    (local-time:unix-to-timestamp (/ (read-from-string (assoccess (last-elt dataset) :time)) 1000000))))
					       dataset))))
		    (push-to-log "<b>OPTIMIZATION.</b><hr/>")
		    (push-to-log (format nil "~a agents retrieved for pattern ~s." agents-count types))
		    (optimization instrument timeframe types
				  (lambda () (let ((beliefs (gen-random-beliefs num-inputs)))
					       (gen-agent num-rules creation-dataset
							  (assoccess beliefs :perception-fns)
							  (assoccess beliefs :lookahead-count)
							  (assoccess beliefs :lookbehind-count))))
				  training-dataset
				  seconds
				  :report-fn report-fn)
		    (push-to-log "Optimization process completed.")
		    (when (not *is-production*)
		      (push-to-log "<b>SIGNAL.</b><hr/>")
		      (push-to-log (format nil "Trying to create signal with ~a agents." agents-count))
		      (test-agents instrument timeframe (flatten type-groups) rates full-training-dataset testing-dataset)
		      (push-to-log "Not enough agents to create a signal."))
		    (when *is-production*
		      (push-to-log "<b>VALIDATION.</b><hr/>")
		      (push-to-log "Validating trades older than 24 hours.")
		      (validate-trades))
		    )))))))
  (unless *is-production*
    (conn (query (:delete-from 'agents :where (:= 1 1)))
	  (query (:delete-from 'agents-patterns :where (:= 1 1))))))

(defun loop-optimize-test ()
  (clear-logs)
  (loop (unless (is-market-close))
     (-loop-optimize-test)))

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

(defun pentropy (rates &optional (dimension 3) (rho 1))
  (let* (;; (rates (loop for rate in rates by (lambda (sublist) (nthcdr rho sublist)) collect rate))
	 (rates (loop for rate in rates by (lambda (sublist) (nthcdr rho sublist)) collect (assoccess rate :close-bid)))
	 (tsize (- (length rates) (1- dimension)))
	 (partitions (mapcar #'sorted-indexes
			     (loop for rate on rates
				when (>= (length rate) dimension)
				collect (subseq rate 0 dimension))))
	 (pis))
    ;; Calculating pi for each permutation.
    (map-permutations (lambda (perm)
			(let ((p (/ (count perm partitions :test #'equal) tsize)))
			  (when (> p 0)
			    (push p pis))))
		      (iota dimension))
    (- (/ (loop for p in pis
	     summing (* p (log p 2)))
	  (log (factorial dimension) 2)))))





;; (defun generate-plot (plot-code width height)
;;   (let ((style (cl-css:css `((html :height 100%)
;;                              (body :height 100%
;;                                    :display flex
;;                                    :justify-content center
;;                                    :align-items center)
;;                              ("#plot" :width ,#?"${width}px"
;;                                       :height ,#?"${height}px")))))
;;     (who:with-html-output-to-string (_)
;;       (:html
;;        (:head
;;         (:script :src "https://cdn.plot.ly/plotly-latest.min.js")
;;         (:style (who:str style)))
;;        (:body
;;         (:div :id "plot")
;;         (:script (who:str plot-code)))))))

;; (defun open-plot (plot-code width height)
;;   "Write output to the file and open browser"
;;   (uiop/stream:with-temporary-file (:pathname pn :stream stream :direction :output :keep t :type "html")
;;     (write-string (generate-plot plot-code width height) stream)
;;     (sb-ext:run-program (or (uiop:getenv "BROWSER") "xdg-open") (list (namestring pn)) :wait nil :search t)))

;; (defun pl-plot (traces &key layout (width 1000) (height 700))
;;   "Plot the data (list of traces)"
;;   (let* ((json-traces (format nil "[~{~a~^,~}]" (mapcar #'json:encode-json-alist-to-string traces)))
;;          (json-layout (json:encode-json-alist-to-string layout))
;;          (plot-code (ps:ps
;;                       (let ((div ((ps:@ document get-element-by-id) "plot")))
;;                         (*plotly.plot div ((ps:@ *json* parse) (ps:lisp json-traces))
;;                                       ((ps:@ *json* parse) (ps:lisp json-layout)))))))
;;     (open-plot plot-code width height)))

(defun describe-agent (agent)
  (let* ((beliefs (beliefs agent))
	 (rules (rules agent))
	 (fitnesses (fitnesses agent))
	 (fuzzy-rules (-> rules :rules)))
    fuzzy-rules

    
    ))
;; (defun describe-agents (agents)
;;   )

;; (describe-agent (gen-agent 4 *rates*))

;; (pentropy (iota 600) 3 10)
;; (loop for i from (- (length *rates*) 100) downto 0
;;    collect (pentropy (get-output-dataset *rates* i) 5 20))

;; (loop for rate in *rates* by (lambda (sublist) (nthcdr 10 sublist)) collect (assoccess rate :close-bid))

;; (loop for rate in '(1 2 3 4 5 6 7 8 9 10) by (lambda (sublist) (nthcdr 4 sublist)) collect rate)

;; ;; (pentropy (subseq *rates* 0 40))

;; (time (apply #'max (loop for i from (- (length *rates*) 10) downto 0
;; 		      collect (pentropy (get-output-dataset *rates* i) 5 2))))
;; (pentropy (subseq *rates* 0 4))

;; (position 0.9107517f0 (loop for i from (- (length *rates*) 10) downto 0 by 10
;; 			 collect (pentropy (get-output-dataset *rates* i) 3)))

;; (loop for rate in *rates*)

;; (map nil (lambda (num) (format t "~5$~%" num))
;;      ')

;; (get-last-tests '(:AUD_USD) '(:H1) 3)
;; (get-last-tests '(:EUR_JPY) '(:H1) 3)

;; (evaluate-agents (gen-agents 3 3 *rates*) (get-output-dataset *rates* (- (length *rates*) 5)))

;; (get-prediction (gen-agents 3 3 *rates*) *rates*)

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

;; (get-rates-chunk-of-types (subseq *rates* 0 500) '(:bullish))
;; (get-rates-chunk-of-types *rates* '(:bullish))
;; (loop for rate in (subseq *rates* 140 240)
;;    do (print (assoccess rate :close-bid)))
;; (loop for rate in (subseq *rates* 340 440)
;;    do (print (assoccess rate :close-bid)))
;; (loop for rate in (subseq *rates* 120 220)
;;    do (print (assoccess rate :close-bid)))

(defun get-winning-type (scored-rates)
  (caar (sort scored-rates #'< :key #'cdr)))

(defun get-types-score (scored-rates types)
  (loop for type in types sum (assoccess scored-rates type)))

;; (get-balance-score (score-rates *rates*))

;; (get-winning-type (score-rates (get-output-dataset *rates* 400)))

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




;; RENEWAL
;; (defparameter *rates* (get-rates-count :EUR_USD :H1 1500 :provider :oanda :type :fx))
;; (gen-random-beliefs 2)

(defun con (y x0 x1)
  (let* ((m (/ 1
	       (- x1 x0)))
	 (b (+ (* m (- x0)) 0)))
    (/ (- y b) m)))
;; (con 0.0 -5 0)
;; (funcall (con 0 5 0) 0.0)

(defun ant (x x0 x1)
  (if (= (- x1 x0) 0)
      0
      (let* ((m (/ 1 (- x1 x0)))
	     (b (+ (* m (- x0)) 0)))
	(+ (* m x) b))))
;; (ant 3 0 5)
;; (ant 3 5 0)

;; (defun make-antecedent (mean spread)
;;   (lambda (x) (exp (* -0.5 (expt (/ (- x mean) spread) 2)))))
;; ;; (funcall (make-antecedent 0.5 0.05) 0.5)

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

(defun eval-activation (inputs input-antecedents)
  (let ((activation 0))
    (loop
       for input in inputs
       for antecedents across input-antecedents
       do (let ((winner-gm 0))
	    (loop
	       for idx from 0
	       for ant across antecedents
	       do (let ((gm (ant input (aref ant 0) (aref ant 1))))
		    ;; Antecedents will always work with OR (max).
		    (when (and (<= gm 1)
			       (>= gm 0)
			       (>= gm winner-gm))
		      (setf winner-gm gm))))
	    (incf activation winner-gm)
	    ))
    (/ activation (length inputs))
    ))

;; (let* ((agent (gen-agent 3 *rates* (assoccess (gen-random-beliefs 15) :perception-fns) 10 30))
;;        (perception-fn (get-perception-fn agent))
;;        (antecedents (read-from-string (assoccess agent :antecedents))))
;;   (time (loop repeat 10
;; 	   do (print (eval-activation (funcall perception-fn *rates*)
;; 				      (read-from-string (assoccess agent :antecedents)))))))

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

(defmacro :is (inp fn)
  `(funcall ,fn ,inp))
(defmacro :and (&rest body)
  `(min ,@body))
(defmacro :or (&rest body)
  `(max ,@body))

;; `((:or (:is height (ant 1 3))
;;        (:is weight (ant 6 8))
;;        (:is weight (ant 1 2)))
;;   (:and (:is height (ant 2 8))
;; 	(:is weight (ant 3 9))))

(defun make-ifis (agent num-rules rates)
  "Analytical version."
  (let* ((perception-fn (get-perception-fn agent))
	 (lookahead-count (slot-value agent 'lookahead-count))
	 (lookbehind-count (slot-value agent 'lookbehind-count))
	 (idxs (get-same-direction-outputs-idxs rates num-rules :lookahead-count lookahead-count :lookbehind-count lookbehind-count))
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

;; (let* ((agent (gen-agent 30 *rates* (assoccess (gen-random-beliefs 10) :perception-fns) 10 16))
;;        (perception-fn (gen-perception-fn (slot-value agent 'perception-fns)))
;;        (antecedents (slot-value agent 'antecedents))
;;        (consequents (slot-value agent 'consequents)))
;;   (time (loop repeat 1000
;; 	   do (eval-ifis (funcall perception-fn *rates*)
;; 			 (slot-value agent 'antecedents)
;; 			 (slot-value agent 'consequents)))))

;; (time (let ((inputs '(10 10))
;; 	    (input-antecedents #(#(#(0 4)
;; 				   #(3 7)
;; 				   #(6 10))
;; 				 #(#(0 4)
;; 				   #(3 7)
;; 				   #(6 10))))
;; 	    (input-consequents #(#(#(#(0 5) #(0 1))
;; 				   #(#(5 10) #(0 1))
;; 				   #(#(10 15) #(0 1)))
;; 				 #(#(#(0 0.01) #(0 1))
;; 				   #(#(5 10) #(0 1))
;; 				   #(#(10 15) #(0 1))))))
;;         (eval-ifis inputs input-antecedents input-consequents)))









;; (winning-type-output-dataset *rates* '((:bullish) (:bearish) (:stagnated)))
;; (winning-type-output-dataset (get-input-dataset *rates* 1400) '((:bullish) (:bearish) (:stagnated)) :max-dataset-size 500)
;; (get-rates-chunk-of-types (subseq *rates* 0 1300) '(:bearish))
;; (get-rates-chunk-of-types (subseq *rates* 0 900) '(:bearish))

;; (loop for rate in (get-output-dataset *rates* 0)
;;    do (print (assoccess rate :close-bid)))
;; (loop for rate in (subseq *rates* 900 1020)
;;    do (print (assoccess rate :close-bid)))
;; (loop for rate in (subseq *rates* 500 600)
;;    do (print (assoccess rate :close-bid)))

;; (loop for rate in (get-output-dataset *rates* (ideal-output-dataset-idx *rates* '(:stagnated) :max-dataset-size 1500))
;;    do (print (assoccess rate :close-bid)))

;; (defparameter *rates* (get-rates-count :EUR_JPY :H1 1500 :provider :oanda :type :fx))
;; (let* ((testing-idx (ideal-output-dataset-idx *rates*))
;;        (training-idx (ideal-output-dataset-idx (get-input-dataset *rates* testing-idx)))
;;        (creation-idx (ideal-output-dataset-idx (get-input-dataset *rates* training-idx))))

;;   (defparameter *testing-dataset* (get-output-dataset *rates* testing-idx))
;;   (defparameter *training-dataset* (get-output-dataset (get-input-dataset *rates* testing-idx) training-idx))
;;   (defparameter *creation-dataset* (get-output-dataset (get-input-dataset *rates* training-idx) creation-idx))

;;   (list (list creation-idx training-idx testing-idx)
;; 	(list (length *creation-dataset*)
;; 	      (length *training-dataset*)
;; 	      (length *testing-dataset*))))

;; (defparameter *results* nil)
;; (defparameter *agents* nil)

;; (defparameter *agents* (optimization *agents*
;; 				     (lambda () (gen-agent 3 *creation-dataset*))
;; 				     *training-dataset*
;; 				     1000
;; 				     (lambda (agents rates)
;; 				       (format t "Training:~%")
;; 				       (report agents rates)
;; 				       (format t "Testing:~%")
;; 				       (report agents *testing-dataset*)
;; 				       (format t "~%-------------~%"))))

;; (let* ((idx 0)
;;        (training-results (reverse (loop
;; 				     for i from 0
;; 				     for result in *results*
;; 				     when (oddp i)
;; 				     collect (nth idx result))))
;;        (testing-results (let ((results (reverse (loop
;; 				    for i from 0
;; 				    for result in *results*
;; 				    when (evenp i)
;; 						   collect (nth idx result)))))
;; 			  (if (/= (length results) (length training-results))
;; 			      (butlast results)))))
;;   (format t "Correlation: ~a~%"
;;   	  (cl-mathstats:correlation training-results testing-results))
;;   (format t "Absolute Correlation: ~a~%"
;; 	  (let* (;; (mean-pos (mean (remove-if-not #'plusp training-results)))
;; 		 ;; (possible-good-predictions (remove-if-not (lambda (res) (> res mean-pos)) training-results))
;; 		 (possible-good-predictions (remove-if-not (lambda (res) (> res 0)) training-results))
;; 		 (good-predictions (reduce #'+ (mapcar (lambda (training testing)
;; 							 (if (and (> training 0)
;; 							      ;; (> training mean-pos)
;; 								  (> testing 0))
;; 							     1
;; 							     0))
;; 						       training-results
;; 						       testing-results))))
;; 	    (format nil "~a/~a (~2$%)" good-predictions
;; 		    (length possible-good-predictions)
;; 		    (* 100 (float (/ good-predictions (length possible-good-predictions)))))))
;;   (print (length training-results))
;;   (plotly-make-plot (plotly-layout)
;;   		    (plotly-line (iota (length training-results)) training-results "black" "dot")
;;   		    (plotly-line (iota (length testing-results)) testing-results "blue")
;;   		    )
;;   )




;; (loop repeat 1 collect (let* ((idx (random-int *rand-gen* 0 (length *rates*)))
;; 			       (input-dataset (get-input-dataset *rates* idx))
;; 			       (output-dataset (get-output-dataset *rates* idx)))
;; 			  (let ((result (ignore-errors
;; 					  (evaluate-agents (gen-agents 20 3 input-dataset) output-dataset))))
;; 			    (if result
;; 				(let ((revenues (assoccess result :revenues))
;; 				      (total-revenue (assoccess result :total-revenue))
;; 				      (avg-revenue (assoccess result :avg-revenue))
;; 				      (trades-won (assoccess result :trades-won))
;; 				      (trades-lost (assoccess result :trades-lost))
;; 				      (entry-times (mapcar #'time-to-unix (assoccess result :entry-times)))
;; 				      (exit-times (mapcar #'time-to-unix (assoccess result :exit-times)))
;; 				      (entry-prices (assoccess result :entry-prices))
;; 				      (exit-prices (assoccess result :exit-prices))
;; 				      (tps (assoccess result :tps))
;; 				      (sls (assoccess result :sls)))
;; 				  (format t "Total Revenue: ~a~%" total-revenue)
;; 				  (format t "Average Revenue: ~a~%~%" avg-revenue)
;; 				  (format t "Trades Won: ~a~%~%" trades-won)
;; 				  (format t "Trades Lost: ~a~%~%" trades-lost)

;; 				  (apply #'plotly-make-plot (plotly-layout)
;; 				       	 (append (list (plotly-candlestick output-dataset))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for exit-price in exit-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (> revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price tp))
;; 				       					   "blue")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (> revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price sl))
;; 				       					   "red" "dot")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for exit-price in exit-prices
;; 				  		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (< revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price sl))
;; 				       					   "red")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (< revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price tp))
;; 				       					   "blue" "dot")))))
;; 				  )
;; 				0))))

;; (-> (gen-ifis (make-instance 'agent) 4 *rates*) :rules)
