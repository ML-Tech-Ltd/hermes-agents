;; (ql:quickload :overmind-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (loop-optimize-test 10 :instruments '(:AUD_USD))
;; (loop-optimize-test 10)
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
	   :read-log)
  (:nicknames :omage))
(in-package :overmind-agents)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
			    (if (/= ideal-cores-count 0)
				ideal-cores-count 1))))

(defun rate-close (rate)
  (/ (+ (access rate :close-bid)
	(access rate :close-ask))
     2))
(defun rate-high (rate)
  (/ (+ (access rate :high-bid)
	(access rate :high-ask))
     2))
(defun rate-low (rate)
  (/ (+ (access rate :low-bid)
	(access rate :low-ask))
     2))
(defun rate-open (rate)
  (/ (+ (access rate :open-bid)
	(access rate :open-ask))
     2))
(defun rate-close-bid (rate)
  (access rate :close-bid))
(defun rate-close-ask (rate)
  (access rate :close-bid))
(defun rate-low-bid (rate)
  (access rate :low-bid))
(defun rate-low-ask (rate)
  (access rate :low-ask))
(defun rate-high-bid (rate)
  (access rate :high-bid))
(defun rate-high-ask (rate)
  (access rate :high-ask))
(defun rate-open-bid (rate)
  (access rate :open-bid))
(defun rate-open-ask (rate)
  (access rate :open-ask))

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

;; (progn (drop-database) (init-database) (init-patterns))

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

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (conn
   (unless (table-exists-p 'rates)
     (query (:create-table 'rates
		((id :type string)
		 (perception-fns :type string))
	      (:primary-key id))))
   (unless (table-exists-p 'agents)
     (query (:create-table 'agents
		((id :type string)
		 (perception-fns :type string)
		 (lookahead-count :type integer)
		 (lookbehind-count :type integer)
		 (ifis :type string)
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
				 (when (and (> tp 0) (< (- (access rate low-type) entry-price) sl))
				   (return sl))
				 ;; Then it's a sell. Lose.
				 (when (and (< tp 0) (> (- (access rate high-type) entry-price) sl))
				   (return (- sl)))
				 ;; Then it's a buy. Win.
				 (when (and (> tp 0) (> (- (access rate high-type) entry-price) tp))
				   (return tp))
				 ;; Then it's a sell. Win.
				 (when (and (< tp 0) (< (- (access rate low-type) entry-price) tp))
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
  (let ((trades (conn (query (:select 'trades.result 'patterns.instrument
				      :from 'trades
				      :inner-join 'patterns-trades
				      :on (:= 'trades.id 'patterns-trades.trade-id)
				      :inner-join 'patterns
				      :on (:= 'patterns.id 'patterns-trades.pattern-id)
				      :where (:and (:not (:is-null 'trades.result))
						   (:>= 'creation-time from)
						   (:not (:= 'patterns.instrument "USD_CNH"))
						   ;; (:not (:= 'patterns.type "STAGNATED"))
						   ;; (:= 'patterns.type "STAGNATED")
						   ;; (:= 'patterns.instrument "USD_CNH")
						   (:<= 'creation-time to)))
			     :alists))))
    (loop for trade in trades
       summing (let ((instrument (make-keyword (access trade :instrument))))
		 (to-pips instrument (access trade :result))))))

;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 1 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 5 :day)))

;; (loop for i from 23 below 72 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :hour)))))

(defun debug-trade ()
  (let* ((trade (nth 5 (conn (query (:select 'trades.* 'patterns.*
					     :from 'trades
					     :inner-join 'patterns-trades
					     :on (:= 'trades.id 'patterns-trades.trade-id)
					     :inner-join 'patterns
					     :on (:= 'patterns.id 'patterns-trades.pattern-id)
					     :where (:= 'patterns.instrument "USD_CNH")
					     ) :alists))))
	 (from (print (* (access trade :creation-time) 1000000)))
	 (from-timestamp (local-time:unix-to-timestamp (/ from 1000000))))
    (let* ((instrument (make-keyword (access trade :instrument)))
	   (timeframe :M1)
	   (to (* (local-time:timestamp-to-unix (local-time:timestamp+ from-timestamp 3 :day)) 1000000))
	   (rates (get-rates-range instrument timeframe from to :provider :oanda :type :fx))
	   (result (get-trade-result (access trade :entry-price)
				     (access trade :tp)
				     (access trade :sl)
				     rates)))

      (format t "Result: ~a~%" result)
      (format t "Entry: ~a~%" (access trade :entry-price))
      (format t "TP: ~a~%" (access trade :tp))
      (format t "SL: ~a~%" (access trade :sl))
      (if (plusp (access trade :tp))
	  (loop for rate in rates do (print (access rate :high-bid)))
	  (loop for rate in rates do (print (access rate :high-bid)))))))

(defun validate-trades ()
  (let ((trades (conn (query (:select 'trades.* 'patterns.*
				      :from 'trades
				      :inner-join 'patterns-trades
				      :on (:= 'trades.id 'patterns-trades.trade-id)
				      :inner-join 'patterns
				      :on (:= 'patterns.id 'patterns-trades.pattern-id)
				      :where (:is-null 'trades.result)) :alists))))
    (push-to-log (format nil "Trying to validate ~a trades." (length trades)))
    (loop for trade in trades
       do (let* ((from (* (access trade :creation-time) 1000000))
		 (from-timestamp (local-time:unix-to-timestamp (/ from 1000000))))
	    ;; (print (make-keyword (access trade :instrument)))
	    ;; (print (local-time:unix-to-timestamp (/ from 1000000)))
	    (when (local-time:timestamp< from-timestamp
					 (local-time:timestamp- (local-time:now) 1 :day))
	      (push-to-log (format nil "Using minute rates from ~a to ~a to validate trade."
				   from-timestamp
				   (local-time:timestamp+ from-timestamp 3 :day)))
	      (let* ((instrument (make-keyword (access trade :instrument)))
		     (timeframe :M1)
		     (to (* (local-time:timestamp-to-unix (local-time:timestamp+ from-timestamp 3 :day)) 1000000))
		     (rates (get-rates-range instrument timeframe from to :provider :oanda :type :fx))
		     (result (get-trade-result (access trade :entry-price)
					       (access trade :tp)
					       (access trade :sl)
					       rates)))
		(push-to-log (format nil "Result obtained for trade: ~a." result))
		(when result
		  (conn
		   (let ((dao (get-dao 'trade (access trade :id))))
		    (setf (access dao :result) result)
		    (update-dao dao))))
		(sleep 1)))))))
;; (validate-trades)

(defun ->delta-close (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates))
	 (penultimate-candle (nth (- lrates (1+ offset) 1) rates)))
    (- (rate-close last-candle)
       (rate-close penultimate-candle))))
;; (->delta-close *rates*)

(defun ->sma-close (rates &key (offset 0) (n 10))
  (mean (loop for i below n
	   collect (->delta-close rates :offset (+ i offset)))))

(defun ->high-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (rate-high last-candle)
       (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-open last-candle)
	   (rate-close last-candle)))))

(defun ->low-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-close last-candle)
	   (rate-open last-candle))
       (rate-low last-candle))))

(defun ->candle-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (abs (- (rate-close last-candle)
	    (rate-open last-candle)))))

;; (->delta-close-bid *rates* :offset 1)

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->high-height) (:offset . 2))
			((:fn . ->low-height) (:offset . 2))
			((:fn . ->candle-height) (:offset . 2))
			((:fn . ->high-height) (:offset . 1))
			((:fn . ->low-height) (:offset . 1))
			((:fn . ->candle-height) (:offset . 1))
			((:fn . ->high-height) (:offset . 0))
			((:fn . ->low-height) (:offset . 0))
			((:fn . ->candle-height) (:offset . 0))))
    (:lookahead-count . 10)
    (:lookbehind-count . 5)))

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->sma-close) (:offset . 9))
			((:fn . ->sma-close) (:offset . 8))
			((:fn . ->sma-close) (:offset . 7))
			((:fn . ->sma-close) (:offset . 6))
			((:fn . ->sma-close) (:offset . 5))
			((:fn . ->sma-close) (:offset . 4))
			((:fn . ->sma-close) (:offset . 3))
			((:fn . ->sma-close) (:offset . 2))
			((:fn . ->sma-close) (:offset . 1))
			((:fn . ->sma-close) (:offset . 0))))
    (:lookahead-count . 10)
    (:lookbehind-count . 20)))

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->delta-close) (:offset . 2))
			((:fn . ->delta-close) (:offset . 1))
			((:fn . ->delta-close) (:offset . 0))))
    (:lookahead-count . 10)
    (:lookbehind-count . 4)))

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->high-height) (:offset . 2))
			((:fn . ->low-height) (:offset . 2))
			((:fn . ->candle-height) (:offset . 2))
			((:fn . ->high-height) (:offset . 1))
			((:fn . ->low-height) (:offset . 1))
			((:fn . ->candle-height) (:offset . 1))
			((:fn . ->high-height) (:offset . 0))
			((:fn . ->low-height) (:offset . 0))
			((:fn . ->candle-height) (:offset . 0))))
    (:lookahead-count . 10)
    (:lookbehind-count . 5)))

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->sma-close) (:offset . 0) (:n . 15))
			((:fn . ->sma-close) (:offset . 0) (:n . 5))
			))
    (:lookahead-count . 10)
    (:lookbehind-count . 16)))

(defun gen-random-perception-fns (fns-count)
  (let ((fns-bag '(((:fn . ->sma-close) (:offset . 0) (:n . 13))
		   ((:fn . ->sma-close) (:offset . 0) (:n . 11))
		   ((:fn . ->sma-close) (:offset . 0) (:n . 9))
		   ((:fn . ->sma-close) (:offset . 0) (:n . 7))
		   ((:fn . ->sma-close) (:offset . 0) (:n . 5))
		   ((:fn . ->sma-close) (:offset . 0) (:n . 3))
		   ((:fn . ->high-height) (:offset . 2))
		   ((:fn . ->low-height) (:offset . 2))
		   ((:fn . ->candle-height) (:offset . 2))
		   ((:fn . ->high-height) (:offset . 1))
		   ((:fn . ->low-height) (:offset . 1))
		   ((:fn . ->candle-height) (:offset . 1))
		   ((:fn . ->high-height) (:offset . 0))
		   ((:fn . ->low-height) (:offset . 0))
		   ((:fn . ->candle-height) (:offset . 0))
		   ((:fn . ->delta-close) (:offset . 2))
		   ((:fn . ->delta-close) (:offset . 1))
		   ((:fn . ->delta-close) (:offset . 0))
		   )))
    `((:perception-fns . ,(subseq (shuffle fns-bag) 0 fns-count))
      (:lookahead-count . 10)
      (:lookbehind-count . 16))))
;; (gen-random-perception-fns 2)

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn in perception-fns
       collect (apply #'funcall (access fn :fn) rates (flatten (rest fn))))))

;; (let ((fn '((:fn . ->sma-close) (:offset . 0) (:n . 20))))
;;   (apply #'funcall (access fn :fn) *rates* (flatten (rest fn))))

;; (funcall (gen-perception-fn (access *beliefs* :perception-fns)) *rates*)

(defclass agent ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (perception-fns :col-type string :initarg :perception-fn)
   (lookahead-count :col-type integer :initarg :lookahead-count)
   (lookbehind-count :col-type integer :initarg :lookbehind-count)
   (ifis :col-type string :initarg :ifis)
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

;; (conn
;;   (let ((agent (make-instance 'agent)))
;;     (setf (access agent :beliefs) (format nil "~s" (ms:marshal *beliefs*)))
;;     (setf (access agent :rules) (format nil "~s" (ms:marshal (gen-ifis agent 4 *rates*))))
;;     (print (access agent :id))
;;     (insert-dao agent)
;;     ))
;; (conn
;;   (access (get-dao 'agent "A8E60D60-1E90-4191-B225-E151C72FD455") :beliefs))
;; (access (make-instance 'agent) :rules)
;; (funcall (gen-perception-fn (access (ms:unmarshal (ms:marshal *beliefs*)) :perception-fns)) *rates*)

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules fitnesses))

;; (let* ((agents (update-agents-fitnesses (gen-agents 10 3 *rates*) *rates*))
;;        (ds-agents (ms:unmarshal (ms:marshal agents))))
;;   (list (access (evaluate-agents agents *rates*) :avg-revenue)
;; 	(access (evaluate-agents ds-agents *rates*) :avg-revenue)))

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
      (:sl . ,(if (>= (abs max-neg) max-pos) max-pos max-neg)))))
;; (time (get-tp-sl (get-input-dataset *rates* 400)))

;; (funcall (access *perception-fn* :perception-fn) (subseq *rates* 10 20))

(defun get-same-direction-outputs-idxs (rates count &key (lookahead-count 10) (lookbehind-count 10) direction-fn)
  (let* ((r (random-float *rand-gen* 0 1))
	(pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
	(opposite-pred (if (> r 0.5) #'minusp #'plusp))
	(idxs (shuffle (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count)))
	(result))
    (loop for idx in idxs
       do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
	    (when (and (< (length result) count)
		       (funcall pred (access tp-sl :tp))
		       (/= (access tp-sl :sl) 0))
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
  (let* ((m (/ (- (access b :y) (access a :y))
	       (- (access b :x) (access a :x))))
	 (b (- (access a :y) (* m (access a :x)))))
    `((:m . ,m) (:b . ,b))))
;; (eq-line-two-points '(:x 3 :y 7) '(:x 5 :y 11))

(defun gen-ifis (agent num-rules rates)
  "Analytical version."
  (let* ((perception-fn (gen-perception-fn (ms:unmarshal (read-from-string (access agent :perception-fns)))))
	 (lookahead-count (access agent :lookahead-count))
	 (lookbehind-count (access agent :lookbehind-count))
	 (idxs (get-same-direction-outputs-idxs rates num-rules :lookahead-count lookahead-count :lookbehind-count lookbehind-count))
	 (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
	 (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count)))
	 (inp-sd (mapcar (lambda (inp) (standard-deviation inp)) (apply #'mapcar #'list chosen-inputs)))
	 (tps (loop for output in chosen-outputs collect (access output :tp)))
	 (sls (loop for output in chosen-outputs collect (access output :sl)))
	 (tp-sd (standard-deviation tps))
	 (sl-sd (standard-deviation sls))
	 ;; (mn-inp (- (apply #'min (flatten chosen-inputs)) (apply #'max inp-sd)))
	 ;; (mx-inp (+ (apply #'max (flatten chosen-inputs)) (apply #'max inp-sd)))
	 (mn-out-tp (let ((min-chosen (apply #'min tps)))
		      (if (and (> min-chosen 0) (< (- min-chosen tp-sd) 0))
			  0.0
			  (- min-chosen tp-sd))))
	 (mn-out-sl (let ((min-chosen (apply #'min sls)))
		      (if (and (> min-chosen 0) (< (- min-chosen sl-sd) 0))
			  0.0
			  (- min-chosen sl-sd))))
	 (mx-out-tp (let ((max-chosen (apply #'max tps)))
		      (if (and (< max-chosen 0) (> (+ max-chosen tp-sd) 0))
			  0.0
			  (+ max-chosen tp-sd))))
	 (mx-out-sl (let ((max-chosen (apply #'max sls)))
		      (if (and (< max-chosen 0) (> (+ max-chosen sl-sd) 0))
			  0.0
			  (+ max-chosen sl-sd)))))
    ;; (format t "~a~%" (> (reduce #'* tps) 0))
    ;; (format t "~a~%" (> (reduce #'* sls) 0))
    ;; (format t "~a~%" chosen-outputs)
    ;; (format t "~a .. ~a ~t ~a .. ~a~%" mn-out-tp mx-out-tp mn-out-sl mx-out-sl)
    (make-if-system :domains (make-domains :antecedents-min 0
					   :antecedents-max 0
					   :consequents-min 0
					   :consequents-max 0)
		    ;; Handling two outputs.
		    :rules (loop for j below (* 2 (length chosen-outputs))
			      collect (loop for i below (length (first chosen-inputs))
					 collect (let* ((input (nth (floor (/ j 2)) chosen-inputs))
							(output (if (evenp j)
								    (nth (floor (/ j 2)) tps)
								    (nth (floor (/ j 2)) sls)))
							(out-sd (if (evenp j)
								    tp-sd
								    sl-sd))
							(mn-out (if (evenp j)
								    mn-out-tp
								    mn-out-sl))
							(mx-out (if (evenp j)
								    mx-out-tp
								    mx-out-sl))
							;; TODO: Consider creating different a,b,c for nmf.
							(ai (* (- (nth i input) (nth i inp-sd)) 1.0d0))
							(bi (* (nth i input) 1.0d0))
							(ci (* (+ (nth i input) (nth i inp-sd)) 1.0d0))
							
							(ac (let ((out (* (- output out-sd) 1.0d0)))
							      (if (<= out mn-out)
								  mn-out
								  out)))
							(bc (* output 1.0d0))
							(cc (let ((out (* (+ output out-sd) 1.0d0)))
							      (if (>= out mx-out)
								  mx-out
								  out)))
							;; TODO: Later we must change this to handle indeterminacy.
							(mf-height 1d0)
							(nmf-height 1d0))
						   (make-rule :antecedent (make-ifs :mf (make-mf :type :triangular
												 :parameters
												 (make-ifs-params
												  := (let ((ab-line (eq-line-two-points `(:x ,ai :y 0d0) `(:x ,bi :y ,mf-height)))
													   (bc-line (eq-line-two-points `(:x ,bi :y ,mf-height) `(:x ,ci :y 0d0))))
												       `((:a . ,ai)
													 (:b . ,bi)
													 (:c . ,ci)
													 (:ab-m . ,(access ab-line :m))
													 (:ab-b . ,(access ab-line :b))
													 (:bc-m . ,(access bc-line :m))
													 (:bc-b . ,(access bc-line :b))
													 (:height . ,mf-height)))))
										    :nmf (make-nmf :type :triangular
												   :parameters
												   (make-ifs-params
												    := (let ((ab-line (eq-line-two-points `(:x ,ai :y 0d0) `(:x ,bi :y ,nmf-height)))
													     (bc-line (eq-line-two-points `(:x ,bi :y ,nmf-height) `(:x ,ci :y 0d0))))
													 `((:a . ,ai)
													   (:b . ,bi)
													   (:c . ,ci)
													   (:ab-m . ,(access ab-line :m))
													   (:ab-b . ,(access ab-line :b))
													   (:bc-m . ,(access bc-line :m))
													   (:bc-b . ,(access bc-line :b))
													   (:height . ,nmf-height))))))
							      :consequent (make-ifs :mf (make-mf :type :triangular
												 :parameters
												 (make-ifs-params
												  := (let ((ab-line (eq-line-two-points `(:x ,ac :y 0d0) `(:x ,bc :y ,mf-height)))
													   (bc-line (eq-line-two-points `(:x ,bc :y ,mf-height) `(:x ,cc :y 0d0))))
												       `((:a . ,ac)
													 (:b . ,bc)
													 (:c . ,cc)
													 (:ab-m . ,(access ab-line :m))
													 (:ab-b . ,(access ab-line :b))
													 (:bc-m . ,(access bc-line :m))
													 (:bc-b . ,(access bc-line :b))
													 (:height . ,mf-height)))))
										    :nmf (make-nmf :type :triangular
												   :parameters
												   (make-ifs-params
												    := (let ((ab-line (eq-line-two-points `(:x ,ac :y 0d0) `(:x ,bc :y ,nmf-height)))
													     (bc-line (eq-line-two-points `(:x ,bc :y ,nmf-height) `(:x ,cc :y 0d0))))
													 `((:a . ,ac)
													   (:b . ,bc)
													   (:c . ,cc)
													   (:ab-m . ,(access ab-line :m))
													   (:ab-b . ,(access ab-line :b))
													   (:bc-m . ,(access bc-line :m))
													   (:bc-b . ,(access bc-line :b))
													   (:height . ,nmf-height)))))))))))
    ))

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
    (setf (access agent :creation-begin-time) (read-from-string (access (first rates) :time)))
    (setf (access agent :creation-end-time) (read-from-string (access (last-elt rates) :time)))
    (setf (access agent :perception-fns) (format nil "~s" (ms:marshal perception-fns)))
    (setf (access agent :lookahead-count) lookahead-count)
    (setf (access agent :lookbehind-count) lookbehind-count)
    (setf (access agent :ifis) (format nil "~s" (ms:marshal (gen-ifis agent num-rules rates))))
    agent))
;; (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55)

(defun gen-agents (num-agents num-rules rates perception-fns lookahead-count lookbehind-count)
  (loop repeat num-agents collect (gen-agent num-rules rates perception-fns lookahead-count lookbehind-count)))
;; (gen-agents 2 3 *rates* (access *beliefs* :perception-fns) 10 55)

(defun evaluate-ifis (agent inputs)
  (let ((rule-groups (-> (ms:unmarshal (read-from-string (access agent :ifis))) :rules))
	(tp-outputs)
	(sl-outputs))
    ;; Handling two outputs.
    (loop
       for rule-group in rule-groups
       for i from 0
       ;; As all the consequents are the same for each `rule-group`,
       ;; we'll just find the minimum if-memebership and use that
       ;; to fire the rule, by directly using `alpha-cut` (not using `fire-rule`).
       do (let ((y (apply #'min (loop
				   for input in inputs
				   for rule in rule-group
				   collect (if-membership input (-> rule :antecedent)))))
		;; All the consequents are the same in this `rule-group`.
		(consequent (-> (first rule-group) :consequent)))
	    (if (evenp i)
		;; Take profit.
		(push (alpha-cut y consequent) tp-outputs)
		;; Stop loss.
		(push (alpha-cut y consequent) sl-outputs))))
    `((:tp . ,(if-coa (reduce #'ifunion tp-outputs)))
      (:sl . ,(if-coa (reduce #'ifunion sl-outputs))))))
;; (fare-memoization:memoize 'evaluate-ifis)
;; (time (evaluate-ifis (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) '(0 0 0)))

;; (loop repeat 10 collect (-> (access (evaluate-ifis (gen-agent 4 *rates*) '(0 0 0)) :tp) :cx))

(defun evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (if (plusp tp)
			   (rate-close-bid (first rates))
			   (rate-close-ask (first rates))))
	(revenue 0)
	(max-pos 0)
	(max-neg 0)
	(exit-time)
	;; Needs to be (length rates) in case the trade never finishes.
	(finish-idx (length rates)))
    (loop for rate in (rest rates)
       for idx from 1 below (length (rest rates))
       do (let ((low (if (plusp tp)
			 (rate-low-ask rate)
			 (rate-low-bid rate)))
		(high (if (plusp tp)
			  (rate-high-ask rate)
			  (rate-high-bid rate)))
		(time (access rate :time)))
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
  (format nil "<b>AGENTS POOL.</b><br/><hr/><br/>~{~a<br/>~}<br/>" (flatten (loop for instrument in ominp:*forex*
					collect (loop for types in '((:bullish) (:bearish) (:stagnated))
						   collect (format nil "~a, ~a, ~a" instrument types (get-agents-count instrument :H1 types)))))))
;; (describe-agents)

(defun get-trades (&optional limit)
  (if limit
      (conn (query (:select '* :from
			    (:as (:select 'trades.* 'patterns.*
					  (:as (:over (:row-number)
						      (:partition-by 'patterns.instrument 'patterns.timeframe
								     :order-by (:desc 'trades.creation-time)))
					       :idx)
					  :from 'trades
					  :inner-join 'patterns-trades
					  :on (:= 'trades.id 'patterns-trades.trade-id)
					  :inner-join 'patterns
					  :on (:= 'patterns-trades.pattern-id 'patterns.id))
				 'results)
			    :where (:<= 'idx '$1))
		   limit
		   :alists))
      (conn (query (:order-by (:select 'trades.* 'patterns.* :from 'trades :inner-join 'patterns-trades :on (:= 'trades.id 'patterns-trades.trade-id) :inner-join 'patterns :on (:= 'patterns-trades.pattern-id 'patterns.id)) (:desc 'trades.creation-time)) :alists))))
;; (get-trades 1)

(defun describe-trades (&optional limit)
  (loop for trade in (get-trades limit)
     do (format t "market: ~a, train-avg-revenue: ~5$, train-trades-won: ~a, train-trades-lost: ~a,    test-avg-revenue: ~5$, test-trades-won: ~a, test-trades-lost: ~a~%"
		(access trade :instrument)
		(access trade :train-avg-revenue)
		(access trade :train-trades-won)
		(access trade :train-trades-lost)
		(access trade :test-avg-revenue)
		(access trade :test-trades-won)
		(access trade :test-trades-lost))))
;; (get-global-revenue)
;; (describe-agents)
;; (describe-trades 1000)

;; (conn (query (:select 'agent-id :from 'agents-patterns)))
;; TODO: when removing an agent in update-pareto-frontier, also remove agent-pattern

(defun pick-most-relevant-agent (agents rates)
  (let ((biggest-activation 0)
	(winner 0))
    (loop
       for idx from 0
       for agent in agents
       collect (let* ((perception-fn (gen-perception-fn (accesses agent :beliefs :perception-fns)))
		      (inputs (funcall perception-fn rates))
		      (activation (let ((rule-groups (-> (rules agent) :rules)))
				    ;; Collecting TP rules only (same antecedents as SL).
				    (mean (flatten
					   (loop for i below (length rule-groups) by 2
					      collect (loop
							 for input in inputs
							 for rule in (nth i rule-groups)
							 collect (if-membership input (-> rule :antecedent)))))))))
		 (when (> activation biggest-activation)
		   (setf biggest-activation activation)
		   (setf winner idx))))
    (nth winner agents)))

(defun get-activation (rules inputs)
  ;; Collecting TP rules only (same antecedents as SL).
  (mean (flatten
	 (loop for i below (length rules) by 2
	    collect (loop
		       for input in inputs
		       for rule in (nth i rules)
		       collect (if-membership input (-> rule :antecedent)))))))

(defun get-perception-fn (agent)
  (gen-perception-fn (ms:unmarshal (read-from-string (access agent :perception-fns)))))
;; (fare-memoization:memoize 'get-perception-fn)

(defun get-agent-activation (agent rates)
  (let* ((perception-fn (get-perception-fn agent))
	 (ifis (ms:unmarshal (read-from-string (access agent :ifis))))
	 (inputs (funcall perception-fn rates))
	 (activation (get-activation (-> ifis :rules) inputs))
	 (agent-activation `(,activation . ,(access agent :id))))
    (values activation agent-activation)))
;; (fare-memoization:memoize 'get-agent-activation)

(defun get-most-activated-agents (instrument timeframe types rates &key (count 1) (limit 1000))
  (let (activations)
    (loop for offset from 0 below (get-agents-count instrument timeframe types) by limit
       do (let ((agents (get-agents instrument timeframe types :limit limit :offset offset)))
	    (loop for agent in agents
	       do (multiple-value-bind (activation agent-activation)
		      (get-agent-activation agent rates)
		    (if (< (length activations) count)
			(progn
			  (push agent-activation activations)
			  (setf activations (sort activations #'> :key #'first)))
			(when (> activation (first (last-elt activations)))
			  (setf (last-elt activations) agent-activation)
			  (setf activations (sort activations #'> :key #'first))))))))
    (let ((agent-ids (loop for act in activations collect (cdr act))))
      (conn (query (:select '* :from 'agents :where (:in 'id (:set agent-ids)))
		   (:dao agent))))))
;; (time (get-most-activated-agents :EUR_USD :H1 '(:BULLISH) *rates* :count 1 :limit 5))

;; (evaluate-agents :EUR_USD :H1 '(:BULLISH) *rates*)

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
		 (output-dataset (get-output-dataset rates idx))
		 (agent (if agent agent (first (get-most-activated-agents instrument timeframe types input-dataset))))
		 (perception-fn (gen-perception-fn (ms:unmarshal (read-from-string (access agent :perception-fns)))))
		 (response (evaluate-ifis agent (funcall perception-fn input-dataset)))
		 (tp (-> (access response :tp) :cx))
		 (sl (-> (access response :sl) :cx))
		 (trade (evaluate-trade tp sl output-dataset))
		 (revenue (access trade :revenue))
		 (max-pos (access trade :max-pos))
		 (max-neg (access trade :max-neg))
		 (exit-time (access trade :exit-time))
		 (finish-idx (access trade :finish-idx)))
	    (if (= revenue 0)
		(push-to-log "." :add-newline? nil)
		(progn
		  (push-to-log "+" :add-newline? nil)
		  (if (> revenue 0)
		      (incf trades-won)
		      (incf trades-lost))
		  (push tp tps)
		  (push sl sls)
		  (push (read-from-string (access (nth idx rates) :time)) entry-times)
		  (push (read-from-string exit-time) exit-times)
		  (push (if (plusp tp)
			    (rate-close-bid (nth idx rates))
			    (rate-close-ask (nth idx rates)))
			entry-prices)
		  (push (if (plusp tp)
			    (rate-close-ask (nth finish-idx output-dataset))
			    (rate-close-bid (nth finish-idx output-dataset)))
			exit-prices)
		  (push max-pos max-poses)
		  (push max-neg max-negses)
		  (push revenue revenues)))
	    (incf idx finish-idx)))
    (push-to-log "<br/>Agents evaluated successfully.")
    `((:begin-time . ,(read-from-string (access (first rates) :time)))
      (:end-time . ,(read-from-string (access (last-elt rates) :time)))
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

;; (room)

(defun evaluate-agent (agent rates)
  (let ((fitnesses (-evaluate-agents :agent agent :rates rates :idx (access agent :lookbehind-count))))
    (loop for fitness in fitnesses
       do (setf (access agent (car fitness))
		(if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    agent))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(defun evaluate-agents (instrument timeframe types rates)
  (-evaluate-agents :instrument instrument :timeframe timeframe :types types :rates rates))
;; (time (evaluate-agents :EUR_USD :H1 '(:BULLISH) (subseq *rates* 0 200)))

(defun get-prediction (instrument timeframe types rates)
  (let* ((agent (first (get-most-activated-agents instrument timeframe types rates)))
	 (perception-fn (gen-perception-fn (ms:unmarshal (read-from-string (access agent :perception-fns)))))
	 (response (evaluate-ifis agent (funcall perception-fn rates)))
	 (tp (-> (access response :tp) :cx))
	 (sl (-> (access response :sl) :cx)))
    `((:tp . ,tp)
      (:sl . ,sl))))
;; (get-prediction :EUR_USD :H1 '(:BULLISH) (subseq *rates* 0 100))

(defun update-agent-fitnesses (agent rates)
  (conn (update-dao (evaluate-agent agent rates))))

(defun update-agents-fitnesses (agents rates)
  (loop for agent in agents
     do (update-agent-fitnesses agent rates))
  agents)

;; (let ((agents (get-agents :EUR_USD :H1 '(:BULLISH) :limit -1)))
;;   (print (access (first agents) :test-avg-revenue))
;;   (update-agents-fitnesses agents (subseq *rates* 0 300))
;;   (print (access (first agents) :test-avg-revenue))
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

(defun update-pareto-frontier (agent agents instrument timeframe types)
  (let* ((avg-revenue-0 (access agent :avg-revenue))
	 (trades-won-0 (access agent :trades-won))
	 (trades-lost-0 (access agent :trades-lost))
	 ;; (stdev-revenue-0 (access agent :stdev-revenue))
	 (is-dominated? nil))
    ;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
    (loop for agent in agents
       do (let* ((avg-revenue (access agent :avg-revenue))
		 (trades-won (access agent :trades-won))
		 (trades-lost (access agent :trades-lost))
		 ;; (stdev-revenue (access agent :stdev-revenue))
		 )
	    ;; Fitnesses currently being used.
	    (if (and (> avg-revenue avg-revenue-0)
		     ;; (< stdev-revenue stdev-revenue-0)
		     (> trades-won trades-won-0)
		     (< trades-lost trades-lost-0))
		;; Candidate agent was dominated.
		(progn (setf is-dominated? t)
		       (return))
		(when (and (<= avg-revenue avg-revenue-0)
			   ;; (> stdev-revenue stdev-revenue-0)
			   (<= trades-won trades-won-0)
			   (>= trades-lost trades-lost-0))
		  ;; Candidate agent dominated another agent. Remove it.
		  (delete-agent agent instrument timeframe types)))))
    (unless is-dominated?
      (insert-agent agent instrument timeframe types))))

(defun plotly-candlestick (rates)
  (let ((x (loop for rate in rates collect (/ (read-from-string (access rate :time)) 1000000)))
	(open (loop for rate in rates collect (access rate :open-bid)))
	(high (loop for rate in rates collect (access rate :high-bid)))
	(low (loop for rate in rates collect (access rate :low-bid)))
	(close (loop for rate in rates collect (access rate :close-bid))))
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
    (push (list (access r :avg-revenue)
		(access r :trades-won)
		(access r :trades-lost))
	  *results*)
    (format t "#Agents: ~4a ~tAvg. Revenue: ~5$ ~tTrades Won: ~4a ~tTrades Lost: ~4a~%"
	    (length agents)
	    (access r :avg-revenue)
	    (access r :trades-won)
	    (access r :trades-lost))))

(defun optimization (instrument timeframe types gen-agent-fn rates minutes &key report-fn)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe types :limit -1)))
		  (update-agents-fitnesses agents rates)
		  (let ((agent (evaluate-agent (funcall gen-agent-fn) rates)))
		    (list (insert-agent agent instrument timeframe types))))))
    (push-to-log (format nil "~a agents retrieved to start optimization." (length agents)))
    (push-to-log (format nil "Performing optimization for ~a seconds." minutes))
    (loop with until-timestamp = (local-time:timestamp+ (local-time:now) minutes :sec)
       do (if (local-time:timestamp> (local-time:now) until-timestamp)
	      (return)
	      (let ((agents (get-agents instrument timeframe types :limit -1))
		    (agent (evaluate-agent (funcall gen-agent-fn) rates)))
		(push-to-log (format nil "Updating Pareto frontier with ~a agents." (length agents)))
		(update-pareto-frontier agent agents instrument timeframe types)
		(push-to-log "Pareto frontier updated successfully.")
		(when report-fn
		  (funcall report-fn (get-agents instrument timeframe types :limit -1) rates))))))
  ;; (loop repeat minutes
  ;;    do (ignore-errors
  ;; 	    (let ((agent (evaluate-agent (funcall gen-agent-fn) rates)))
  ;; 	      (update-pareto-frontier agent agents instrument timeframe types)
  ;; 	      (when report-fn
  ;; 		(funcall report-fn (get-agents instrument timeframe types :limit -1) rates)))))
  )

(defun decompress-object (compressed-object)
  "Decompresses an object represented by `compressed-object`."
  ;; (decompress-object (compress-object (gen-agents 1 3 *rates*)))
  (ms:unmarshal
   (read-from-string
    (flexi-streams:octets-to-string
     (zlib:uncompress compressed-object)))))

(defun compress-object (object)
  "Compresses `object`, which can be any lisp data structure."
  ;; (compress-object (gen-agents 5 3 *rates*))
  (salza2:compress-data (flexi-streams:string-to-octets
			 (format nil "~s" (marshal:marshal object)))
			'salza2:zlib-compressor))

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
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (access pattern :id))))) :column))))

(defun get-agents (instrument timeframe types &key (limit 1) (offset 0))
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
;; (access (car (get-agents :EUR_USD :H1 '(:BULLISH) :limit 1)) :avg-revenue)

;; (conn (query (:select '* :from 'agents)))

;; Get number of agents. Then get agent #1, calculate, then release.
(defun remove-bad-agents (&optional (max-agents-per-pool 600))
  (loop for timeframe in ominp:*timeframes*
     do (loop for instrument in ominp:*instruments*
	   do (let* ((agents (get-agents instrument timeframe))
		     (observations (loop for agent in agents
				      ;; Fitnesses currently being used.
				      collect (let* ((fitnesses (access agent :fitnesses))
						     (avg-revenue (access fitnesses :avg-revenue))
						     (stdev-revenue (access fitnesses :stdev-revenue))
						     (trades-won (access fitnesses :trades-won))
						     (trades-lost (access fitnesses :trades-lost)))
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

(defun describe-agent-fitnesses (market)
  (loop for agent in (get-agents market :H1)
     do (let* ((fit (access agent :fitnesses))
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
		   (rr2 `(:rr2 . ,(format nil "~a / ~2$"
					  (/ (* 10000 (abs (mean sls)))
					     (* 10000 (abs (mean sls))))
					  (/ (* 10000 (abs (mean tps)))
					     (* 10000 (abs (mean sls))))))))
	      (format t "AVG-REVENUE: ~t ~a~%AVG-TP: ~t ~a~%AVG-SL: ~t ~a~%TRADES-WON: ~t ~a~%TRADES-LOST: ~t ~a~%RR: ~t ~a~%RR2: ~t ~a~%~%"
		      avg-revenue
		      avg-tp
		      avg-sl
		      trades-won
		      trades-lost
		      rr
		      rr2))))))

(defun insert-agent (agent instrument timeframe types)
  (let ((agent-id (access agent :id))
	(patterns (get-patterns instrument timeframe types)))
    (conn (insert-dao agent)
	  (loop for pattern in patterns
	     do (make-dao 'agent-pattern :agent-id agent-id :pattern-id (access pattern :id))))
    agent))

(defun delete-agent (agent instrument timeframe types)
  (let ((agent-id (access agent :id))
	(patterns (get-patterns instrument timeframe types)))
    (conn
     ;; First we remove agent-pattern.
     (loop for pattern in patterns
	do (delete-dao (make-instance 'agent-pattern :agent-id agent-id :pattern-id (access pattern :id))))
     ;; Then we delete the agent only if there are no agent-patterns related to this agent.
     (unless (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id agent-id)))
       (delete-dao agent)))))

;; (conn (query (:select 'id :from 'agents) :alist))
;; (conn (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id "C3FE332F-82E7-41A6-89CA-E36550D1D687"))))


;; (insert-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) :EUR_USD :H1 '(:BULLISH))

(defun is-market-close ()
  (let ((day-of-week (local-time:timestamp-day-of-week (local-time:now) :timezone local-time:+utc-zone+))
	(hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (or
     ;; Friday
     (and (= day-of-week 5)
	  (>= hour 20)
	  )
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

(defun insert-trade (instrument timeframe types train-fitnesses test-fitnesses prediction rates)
  (conn (let ((patterns (get-patterns instrument timeframe types))
	      (trade (make-dao 'trade
			       :creation-time (local-time:timestamp-to-unix (local-time:now))
			       :decision (if (= (access prediction :tp) 0)
					     "HOLD"
					     (if (> (access prediction :tp) 0)
						 "BUY"
						 "SELL"))
			       :tp (access prediction :tp)
			       :sl (access prediction :sl)
			       :entry-price (access (last-elt rates) :close-bid)
			       :train-begin-time (access train-fitnesses :begin-time)
			       :test-begin-time (access test-fitnesses :begin-time)
			       :train-end-time (access train-fitnesses :end-time)
			       :test-end-time (access test-fitnesses :end-time)
			       :train-dataset-size (access train-fitnesses :dataset-size)
			       :test-dataset-size (access test-fitnesses :dataset-size)
			       :train-avg-revenue (access train-fitnesses :avg-revenue)
			       :test-avg-revenue (access test-fitnesses :avg-revenue)
			       :train-stdev-revenue (access train-fitnesses :stdev-revenue)
			       :test-stdev-revenue (access test-fitnesses :stdev-revenue)
			       :train-total-revenue (access train-fitnesses :total-revenue)
			       :test-total-revenue (access test-fitnesses :total-revenue)
			       :train-avg-max-pos (access train-fitnesses :avg-max-pos)
			       :test-avg-max-pos (access test-fitnesses :avg-max-pos)
			       :train-stdev-max-pos (access train-fitnesses :stdev-max-pos)
			       :test-stdev-max-pos (access test-fitnesses :stdev-max-pos)
			       :train-avg-max-neg (access train-fitnesses :avg-max-neg)
			       :test-avg-max-neg (access test-fitnesses :avg-max-neg)
			       :train-stdev-max-neg (access train-fitnesses :stdev-max-neg)
			       :test-stdev-max-neg (access test-fitnesses :stdev-max-neg)
			       :train-avg-tp (access train-fitnesses :avg-tp)
			       :test-avg-tp (access test-fitnesses :avg-tp)
			       :train-stdev-tp (access train-fitnesses :stdev-tp)
			       :test-stdev-tp (access test-fitnesses :stdev-tp)
			       :train-avg-sl (access train-fitnesses :avg-sl)
			       :test-avg-sl (access test-fitnesses :avg-sl)
			       :train-stdev-sl (access train-fitnesses :stdev-sl)
			       :test-stdev-sl (access test-fitnesses :stdev-sl)
			       :train-max-tp (access train-fitnesses :max-tp)
			       :test-max-tp (access test-fitnesses :max-tp)
			       :train-min-tp (access train-fitnesses :min-tp)
			       :test-min-tp (access test-fitnesses :min-tp)
			       :train-max-sl (access train-fitnesses :max-sl)
			       :test-max-sl (access test-fitnesses :max-sl)
			       :train-min-sl (access train-fitnesses :min-sl)
			       :test-min-sl (access test-fitnesses :min-sl)
			       :train-trades-won (access train-fitnesses :trades-won)
			       :test-trades-won (access test-fitnesses :trades-won)
			       :train-trades-lost (access train-fitnesses :trades-lost)
			       :test-trades-lost (access test-fitnesses :trades-lost)
			       :train-revenues (apply #'vector (access train-fitnesses :revenues))
			       :test-revenues (apply #'vector (access test-fitnesses :revenues))
			       :train-entry-times (apply #'vector (access train-fitnesses :entry-times))
			       :test-entry-times (apply #'vector (access test-fitnesses :entry-times))
			       :train-exit-times (apply #'vector (access train-fitnesses :exit-times))
			       :test-exit-times (apply #'vector (access test-fitnesses :exit-times))
			       :train-entry-prices (apply #'vector (access train-fitnesses :entry-prices))
			       :test-entry-prices (apply #'vector (access test-fitnesses :entry-prices))
			       :train-exit-prices (apply #'vector (access train-fitnesses :exit-prices))
			       :test-exit-prices (apply #'vector (access test-fitnesses :exit-prices))
			       :train-tps (apply #'vector (access train-fitnesses :tps))
			       :test-tps (apply #'vector (access test-fitnesses :tps))
			       :train-sls (apply #'vector (access train-fitnesses :sls))
			       :test-sls (apply #'vector (access test-fitnesses :sls))
			       )))
	  (loop for pattern in patterns
	     do (make-dao 'pattern-trade
			  :pattern-id (access pattern :id)
			  :trade-id (access trade :id)))
	  )))


;; (require 'sb-sprof)
;; (sb-sprof:start-profiling)
;; (sb-sprof:report :type :flat)

;; (ql:quickload :function-cache)
;; (function-cache:purge-all-caches)

(defun test-agents (instrument timeframe types rates training-dataset testing-dataset)
  (let ((train-fitnesses (evaluate-agents instrument timeframe types training-dataset))
	(test-fitnesses (evaluate-agents instrument timeframe types testing-dataset))
	(prediction (get-prediction instrument timeframe types testing-dataset)))
    (when train-fitnesses
      (push-to-log "Training process successful."))
    (when test-fitnesses
      (push-to-log "Testing process successful."))
    (when prediction
      (push-to-log (format nil "Prediction: ~s." prediction)))
    (when (and (/= (access prediction :tp) 0)
	       (if (not (eq instrument :USD_CNH)) (< (access prediction :tp) 100) t)
	       (/= (access prediction :sl) 0)
	       (< (access prediction :sl) 100)
	       (/= (access test-fitnesses :trades-won) 0)
	       (/= (+ (access test-fitnesses :trades-won)
		      (access test-fitnesses :trades-lost))
		   0))
      (push-to-log "Trying to create trade.")
      (insert-trade instrument timeframe types train-fitnesses test-fitnesses prediction rates)
      (push-to-log "Trade created successfully."))))

(let (log)
  (defun push-to-log (msg &key (add-newline? t) (size 100000))
    (if add-newline?
	(push (format nil "~a<br/>" msg) log)
	(push (format nil "~a" msg) log))
    (when (> (length log) size)
      (setf log (butlast log))))
  (defun read-log ()
    (format nil "~a<b>LOG.</b><hr/><br/>~{~a~%~}"
	    (describe-agents)
	    (reverse log))))
;; (push-to-log (random 10) :size 10)
;; (read-log)

(defun loop-optimize-test (minutes &key (max-creation-dataset-size 500) (max-training-dataset-size 500) (max-testing-dataset-size 500) (num-rules 4) (report-fn nil) (type-groups '((:bullish) (:bearish) (:stagnated))) (instruments ominp:*forex*) (timeframes ominp:*shortterm*))
  (loop (unless (is-market-close))
     (dolist (instrument instruments)
       (dolist (timeframe timeframes)
	 (unless (is-market-close)
	   (push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
	   (let ((rates (get-rates-count instrument timeframe
					 (+ max-creation-dataset-size max-training-dataset-size max-testing-dataset-size)
					 :provider :oanda :type :fx)))
	     (push-to-log "Retrieved rates successfully.")
	     (multiple-value-bind (testing-dataset types)
		 (winning-type-output-dataset rates type-groups
					      :max-dataset-size max-testing-dataset-size)
	       (push-to-log (format nil "Testing dataset created successfully. Size: ~s. Dataset from ~a to ~a."
				    (length testing-dataset)
				    (local-time:unix-to-timestamp (/ (read-from-string (access (first testing-dataset) :time)) 1000000))
				    (local-time:unix-to-timestamp (/ (read-from-string (access (last-elt testing-dataset) :time)) 1000000))))
	       (let* ((dataset-size (length rates))
		      (testing-dataset-size (length testing-dataset))
		      (training-dataset (let ((dataset (subseq rates
							       (- dataset-size testing-dataset-size max-training-dataset-size)
							       (- dataset-size testing-dataset-size))))
					  (multiple-value-bind (from to)
					      (get-rates-chunk-of-types dataset types
									:min-chunk-size 200
									:max-chunk-size 500)
					    (subseq dataset from to))))
		      (creation-dataset (let ((dataset (subseq rates
							       (- dataset-size testing-dataset-size (length training-dataset) max-creation-dataset-size)
							       (- dataset-size testing-dataset-size (length training-dataset)))))
					  (multiple-value-bind (from to)
					      (get-rates-chunk-of-types dataset types
									:min-chunk-size 40
									:max-chunk-size 200)
					    (subseq dataset from to))))
		      (agents-count (get-agents-count instrument timeframe types)))
		 (push-to-log (format nil "Creation dataset created successfully. Size: ~s. Dataset from ~a to ~a."
				      (length creation-dataset)
				      (local-time:unix-to-timestamp (/ (read-from-string (access (first creation-dataset) :time)) 1000000))
				      (local-time:unix-to-timestamp (/ (read-from-string (access (last-elt creation-dataset) :time)) 1000000))))
		 (push-to-log (format nil "Training dataset created successfully. Size: ~s. Dataset from ~a to ~a."
				      (length training-dataset)
				      (local-time:unix-to-timestamp (/ (read-from-string (access (first training-dataset) :time)) 1000000))
				      (local-time:unix-to-timestamp (/ (read-from-string (access (last-elt training-dataset) :time)) 1000000))
				      ))
		 (push-to-log (format nil "~a agents retrieved for pattern ~s." agents-count types))
		 (let ()
		   (push-to-log "<b>SIGNAL.</b><hr/>")
		   (push-to-log (format nil "Trying to create signal with ~a agents." agents-count))
		   (when (> agents-count 0)
		     (test-agents instrument timeframe types rates training-dataset testing-dataset))
		   (push-to-log "<b>OPTIMIZATION.</b><hr/>")
		   (optimization instrument timeframe types
				 (lambda () (let ((beliefs (gen-random-perception-fns 2)))
					      (gen-agent num-rules creation-dataset
							 (access beliefs :perception-fns)
							 (access beliefs :lookahead-count)
							 (access beliefs :lookbehind-count))))
				 training-dataset
				 minutes
				 :report-fn report-fn)
		   ;; (test-agents instrument timeframe types rates training-dataset testing-dataset)
		   (push-to-log "Optimization process completed.")
		   (push-to-log "<b>VALIDATION.</b><hr/>")
		   (push-to-log "Validating trades older than 24 hours.")
		   (validate-trades))
		 ))))))
     ;; (remove-bad-agents max-agents-per-pool)
     ))

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
	 (rates (loop for rate in rates by (lambda (sublist) (nthcdr rho sublist)) collect (access rate :close-bid)))
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

;; (loop for rate in *rates* by (lambda (sublist) (nthcdr 10 sublist)) collect (access rate :close-bid))

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
	 (rrs (loop for result in results collect (* (/ (access result :sl)
							(access result :tp))
						     (if (plusp (access result :tp))
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
;; (time (score-rates (get-input-dataset *rates* 1500)))

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
;;    do (print (access rate :close-bid)))
;; (loop for rate in (subseq *rates* 340 440)
;;    do (print (access rate :close-bid)))
;; (loop for rate in (subseq *rates* 120 220)
;;    do (print (access rate :close-bid)))

(defun get-winning-type (scored-rates)
  (caar (sort scored-rates #'< :key #'cdr)))

(defun get-types-score (scored-rates types)
  (loop for type in types sum (access scored-rates type)))

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
    (values (last rates winner-chunk-size) winner-types)))




;; RENEWAL
;; (defparameter *rates* (get-rates-count :EUR_JPY :H1 1500 :provider :oanda :type :fx))
;; (gen-random-perception-fns 2)

;; (defun make-consequent (x0 x1)
;;   (let* ((m (/ 1
;; 	       (- x1 x0)))
;; 	 (b (+ (* m (- x0)) 0)))
;;     (lambda (y) (/ (- y b) m))))
;; ;; (funcall (make-consequent 5 10) 0.0)
;; ;; (funcall (make-consequent 5 10) 1.0)

;; ;; (funcall (make-consequent 10 5) 0.0)
;; ;; (funcall (make-consequent 10 5) 1.0)

;; (defun make-antecedent (x0 x1)
;;   (let* ((m (/ 1
;; 	       (- x1 x0)))
;; 	 (b (+ (* m (- x0)) 0)))
;;     (lambda (x) (+ (* m x) b))))

;; ;; (funcall (make-antecedent 5 10) 10)
;; ;; (funcall (make-antecedent 5 10) 1.0)

;; ;; (funcall (make-antecedent 10 5) 0.0)
;; ;; (funcall (make-antecedent 10 5) 1.0)

;; (defun make-antecedent (mean spread)
;;   (lambda (x) (exp (* -0.5 (expt (/ (- x mean) spread) 2)))))
;; ;; (funcall (make-antecedent 0.5 0.05) 0.5)

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

;; (defun make-ifis (agent num-rules rates)
;;   "Analytical version."
;;   (let* ((perception-fn (gen-perception-fn (ms:unmarshal (read-from-string (access agent :perception-fns)))))
;; 	 (lookahead-count (access agent :lookahead-count))
;; 	 (lookbehind-count (access agent :lookbehind-count))
;; 	 (idxs (get-same-direction-outputs-idxs rates num-rules :lookahead-count lookahead-count :lookbehind-count lookbehind-count))
;; 	 (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
;; 	 (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count)))
;; 	 (inp-sd (mapcar (lambda (inp) (standard-deviation inp)) (apply #'mapcar #'list chosen-inputs)))
;; 	 (tps (loop for output in chosen-outputs collect (access output :tp)))
;; 	 (sls (loop for output in chosen-outputs collect (access output :sl)))
;; 	 (tp-sd (standard-deviation tps))
;; 	 (sl-sd (standard-deviation sls))
;; 	 ;; (mn-inp (- (apply #'min (flatten chosen-inputs)) (apply #'max inp-sd)))
;; 	 ;; (mx-inp (+ (apply #'max (flatten chosen-inputs)) (apply #'max inp-sd)))
;; 	 (mn-out-tp (let ((min-chosen (apply #'min tps)))
;; 		      (if (and (> min-chosen 0) (< (- min-chosen tp-sd) 0))
;; 			  0.0
;; 			  (- min-chosen tp-sd))))
;; 	 (mn-out-sl (let ((min-chosen (apply #'min sls)))
;; 		      (if (and (> min-chosen 0) (< (- min-chosen sl-sd) 0))
;; 			  0.0
;; 			  (- min-chosen sl-sd))))
;; 	 (mx-out-tp (let ((max-chosen (apply #'max tps)))
;; 		      (if (and (< max-chosen 0) (> (+ max-chosen tp-sd) 0))
;; 			  0.0
;; 			  (+ max-chosen tp-sd))))
;; 	 (mx-out-sl (let ((max-chosen (apply #'max sls)))
;; 		      (if (and (< max-chosen 0) (> (+ max-chosen sl-sd) 0))
;; 			  0.0
;; 			  (+ max-chosen sl-sd)))))
;;     ;; (format t "~a~%" (> (reduce #'* tps) 0))
;;     ;; (format t "~a~%" (> (reduce #'* sls) 0))
;;     ;; (format t "~a~%" chosen-outputs)
;;     ;; (format t "~a .. ~a ~t ~a .. ~a~%" mn-out-tp mx-out-tp mn-out-sl mx-out-sl)
;;     (make-if-system :domains (make-domains :antecedents-min 0
;; 					   :antecedents-max 0
;; 					   :consequents-min 0
;; 					   :consequents-max 0)
;; 		    ;; Handling two outputs.
;; 		    :rules (loop for j below (* 2 (length chosen-outputs))
;; 			      collect (loop for i below (length (first chosen-inputs))
;; 					 collect (let* ((input (nth (floor (/ j 2)) chosen-inputs))
;; 							(output (if (evenp j)
;; 								    (nth (floor (/ j 2)) tps)
;; 								    (nth (floor (/ j 2)) sls)))
;; 							(out-sd (if (evenp j)
;; 								    tp-sd
;; 								    sl-sd))
;; 							(mn-out (if (evenp j)
;; 								    mn-out-tp
;; 								    mn-out-sl))
;; 							(mx-out (if (evenp j)
;; 								    mx-out-tp
;; 								    mx-out-sl))
;; 							;; TODO: Consider creating different a,b,c for nmf.
;; 							(ai (* (- (nth i input) (nth i inp-sd)) 1.0d0))
;; 							(bi (* (nth i input) 1.0d0))
;; 							(ci (* (+ (nth i input) (nth i inp-sd)) 1.0d0))
							
;; 							(ac (let ((out (* (- output out-sd) 1.0d0)))
;; 							      (if (<= out mn-out)
;; 								  mn-out
;; 								  out)))
;; 							(bc (* output 1.0d0))
;; 							(cc (let ((out (* (+ output out-sd) 1.0d0)))
;; 							      (if (>= out mx-out)
;; 								  mx-out
;; 								  out)))
;; 							;; TODO: Later we must change this to handle indeterminacy.
;; 							(mf-height 1d0)
;; 							(nmf-height 1d0))
;; 						   (make-rule :antecedent (make-ifs :mf (make-mf :type :triangular
;; 												 :parameters
;; 												 (make-ifs-params
;; 												  := (let ((ab-line (eq-line-two-points `(:x ,ai :y 0d0) `(:x ,bi :y ,mf-height)))
;; 													   (bc-line (eq-line-two-points `(:x ,bi :y ,mf-height) `(:x ,ci :y 0d0))))
;; 												       `((:a . ,ai)
;; 													 (:b . ,bi)
;; 													 (:c . ,ci)
;; 													 (:ab-m . ,(access ab-line :m))
;; 													 (:ab-b . ,(access ab-line :b))
;; 													 (:bc-m . ,(access bc-line :m))
;; 													 (:bc-b . ,(access bc-line :b))
;; 													 (:height . ,mf-height)))))
;; 										    :nmf (make-nmf :type :triangular
;; 												   :parameters
;; 												   (make-ifs-params
;; 												    := (let ((ab-line (eq-line-two-points `(:x ,ai :y 0d0) `(:x ,bi :y ,nmf-height)))
;; 													     (bc-line (eq-line-two-points `(:x ,bi :y ,nmf-height) `(:x ,ci :y 0d0))))
;; 													 `((:a . ,ai)
;; 													   (:b . ,bi)
;; 													   (:c . ,ci)
;; 													   (:ab-m . ,(access ab-line :m))
;; 													   (:ab-b . ,(access ab-line :b))
;; 													   (:bc-m . ,(access bc-line :m))
;; 													   (:bc-b . ,(access bc-line :b))
;; 													   (:height . ,nmf-height))))))
;; 							      :consequent (make-ifs :mf (make-mf :type :triangular
;; 												 :parameters
;; 												 (make-ifs-params
;; 												  := (let ((ab-line (eq-line-two-points `(:x ,ac :y 0d0) `(:x ,bc :y ,mf-height)))
;; 													   (bc-line (eq-line-two-points `(:x ,bc :y ,mf-height) `(:x ,cc :y 0d0))))
;; 												       `((:a . ,ac)
;; 													 (:b . ,bc)
;; 													 (:c . ,cc)
;; 													 (:ab-m . ,(access ab-line :m))
;; 													 (:ab-b . ,(access ab-line :b))
;; 													 (:bc-m . ,(access bc-line :m))
;; 													 (:bc-b . ,(access bc-line :b))
;; 													 (:height . ,mf-height)))))
;; 										    :nmf (make-nmf :type :triangular
;; 												   :parameters
;; 												   (make-ifs-params
;; 												    := (let ((ab-line (eq-line-two-points `(:x ,ac :y 0d0) `(:x ,bc :y ,nmf-height)))
;; 													     (bc-line (eq-line-two-points `(:x ,bc :y ,nmf-height) `(:x ,cc :y 0d0))))
;; 													 `((:a . ,ac)
;; 													   (:b . ,bc)
;; 													   (:c . ,cc)
;; 													   (:ab-m . ,(access ab-line :m))
;; 													   (:ab-b . ,(access ab-line :b))
;; 													   (:bc-m . ,(access bc-line :m))
;; 													   (:bc-b . ,(access bc-line :b))
;; 													   (:height . ,nmf-height)))))))))))
;;     ))

;; (quote
;;  (let ((antecedents `#(,(make-antecedent 1.0 1.6)
;; 		       ,(make-antecedent 1.5 1.8)
;; 		       ,(make-antecedent 1.7 2)))
;;        (consequents `#(,(make-consequent 0 4)
;; 		       ,(make-consequent 4 8)
;; 		       ,(make-consequent 8 10))))
;;    ;; (time (loop repeat 1000000
;;    ;; 	    do (ifis 0.0 antecedents consequents)))
;;    (ifis -2.0 antecedents consequents))
;;  )

;; (quote
;;  (let ((antecedents `#(,(make-antecedent 1.0 1.5)
;; 		       ,(make-antecedent 1.5 1.7)
;; 		       ,(make-antecedent 1.6 1.7)
;; 		       ,(make-antecedent 1.7 1.8)
;; 		       ,(make-antecedent 1.7 2)
;; 		       ))
;;        (consequents `#(,(make-consequent 0 4)
;; 		       ,(make-consequent 4 6)
;; 		       ,(make-consequent 5 6)
;; 		       ,(make-consequent 5 8)
;; 		       ,(make-consequent 8 10))))
;;    ;; (time (loop repeat 1000000
;;    ;; 	    do (ifis 0.0 antecedents consequents)))
;;    (ifis 1.6 antecedents consequents))
;;  )

;; (winning-type-output-dataset *rates* '((:bullish) (:bearish) (:stagnated)))
;; (winning-type-output-dataset (get-input-dataset *rates* 1400) '((:bullish) (:bearish) (:stagnated)) :max-dataset-size 500)
;; (get-rates-chunk-of-types (subseq *rates* 0 1300) '(:bearish))
;; (get-rates-chunk-of-types (subseq *rates* 0 900) '(:bearish))

;; (loop for rate in (get-output-dataset *rates* 0)
;;    do (print (access rate :close-bid)))
;; (loop for rate in (subseq *rates* 900 1020)
;;    do (print (access rate :close-bid)))
;; (loop for rate in (subseq *rates* 500 600)
;;    do (print (access rate :close-bid)))

;; (loop for rate in (get-output-dataset *rates* (ideal-output-dataset-idx *rates* '(:stagnated) :max-dataset-size 1500))
;;    do (print (access rate :close-bid)))

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
;; 				(let ((revenues (access result :revenues))
;; 				      (total-revenue (access result :total-revenue))
;; 				      (avg-revenue (access result :avg-revenue))
;; 				      (trades-won (access result :trades-won))
;; 				      (trades-lost (access result :trades-lost))
;; 				      (entry-times (mapcar #'time-to-unix (access result :entry-times)))
;; 				      (exit-times (mapcar #'time-to-unix (access result :exit-times)))
;; 				      (entry-prices (access result :entry-prices))
;; 				      (exit-prices (access result :exit-prices))
;; 				      (tps (access result :tps))
;; 				      (sls (access result :sls)))
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
