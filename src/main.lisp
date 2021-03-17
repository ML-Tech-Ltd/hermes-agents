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
	   #:get-nested-trades
	   #:loop-optimize-test
	   #:read-log
	   #:read-agents-log
	   #:read-agent-directions-log
	   #:unix-to-nano
	   #:unix-from-nano
	   #:get-rates-range-big
	   #:get-rates-count-big
	   #:get-all-agents
	   #:format-datasets
	   #:update-creation-training-dataset)
  (:nicknames #:omage))
(in-package :overmind-agents)

(defparameter *agents-cache* (make-hash-table :test 'equal :synchronized t))
;; (progn (drop-database) (init-database) (init-patterns) (clear-log) (when omage.config:*is-production* (clear-jobs)))
(defparameter *creation-training-datasets* (make-hash-table :test 'equal :synchronized t))
(defparameter +CELL-FORMATS+ '(:left   "~vA"
			       :center "~v:@<~A~>"
			       :right  "~v@A"))

;; FARE-MEMOIZATION configuration.
(setf fare-memoization::*memoized* (make-hash-table :test #'equal :synchronized t))

;; (describe fare-memoization::*memoized*)
;; (refresh-memory)

(setf lparallel:*kernel* (lparallel:make-kernel
			  (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
			    (if (/= ideal-cores-count 0)
				ideal-cores-count 1))))

(setf cl:*random-state* (make-random-state t))

(defun random-int (from to)
  (+ from (random (- (1+ to) from))))

(defun random-float (from to)
  (+ from (random (- to (float from)))))

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
  (assoccess rate :close-ask))

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

(defun clear-jobs ()
  (when (> (length clerk:*jobs*) 0)
    (ignore-errors
     (clerk:stop)
     (clerk:empty-jobs-queue))))

(defun drop-database ()
  (conn
   ;; (drop-table 'datasets)
   (drop-table 'agents)
   (drop-table 'agents-patterns)
   (drop-table 'patterns)
   (drop-table 'patterns-trades)
   (drop-table 'trades)))
;; (drop-database)

(comment
 (progn
   (defparameter *proof* nil)
   (progn (drop-database) (init-database) (init-patterns) (clear-log))
   (loop-optimize-test))
 )
;; (float (/ (count t *proof* :key #'first) (length *proof*)))
;; (float (/ (count t *proof*) (length *proof*)))

;; Deciles.
(comment
 (loop for i from 0 below 10
       do (let ((proof (remove-if-not (lambda (elt) (equal elt (list i (1+ i)))) *proof* :key #'second)))
	    (if (> (length proof) 0)
		(print (list (1+ i) (float (/ (count t proof :key #'first) (length proof)))
			     (length proof)))
		(print "N/A"))))
 )

(comment
 (sort (loop for act in *activations*
	     for rev in *revenues*
	     collect (list act rev))
       #'<
       :key #'first))

(defun -init-patterns (instrument timeframe)
  (unless (get-patterns instrument timeframe '(:BULLISH))
    (insert-pattern instrument timeframe :BULLISH))
  (unless (get-patterns instrument timeframe '(:BEARISH))
    (insert-pattern instrument timeframe :BEARISH))
  (unless (get-patterns instrument timeframe '(:STAGNATED))
    (insert-pattern instrument timeframe :STAGNATED)))

(defun init-patterns ()
  (loop for instrument in omage.config:*instruments*
	do (loop for timeframe in omage.config:*all-timeframes*
		 do (-init-patterns instrument timeframe))))
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

(defun init-rates (howmany-batches &key (timeframes '(:H1 :M1)))
  (let ((instruments ominp:*forex*))
    (loop for instrument in instruments
	  do (loop for timeframe in timeframes
		   do (let ((rates (get-rates-batches instrument timeframe howmany-batches)))
			(insert-rates instrument timeframe rates))))))
;; (init-rates 2 :timeframes '(:M1))

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (conn
   (unless (table-exists-p 'datasets)
     (query (:create-table 'datasets
		((id :type string)
		 (begin-time :type int8)
		 (end-time :type int8))
	      (:primary-key id))))
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
		 (activation :type double-float)
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
;; (to-pips :USD_CNH 0.0001)

(defun from-pips (instrument quantity)
  (let ((str-instrument (format nil "~a" instrument)))
    (cond ((or (cl-ppcre:scan "JPY" str-instrument)
	       (cl-ppcre:scan "HUF" str-instrument)
	       (cl-ppcre:scan "KRW" str-instrument)
	       (cl-ppcre:scan "THB" str-instrument))
	   (/ quantity 100))
	  ((or (cl-ppcre:scan "CZK" str-instrument)
	       (cl-ppcre:scan "CNY" str-instrument)
	       (cl-ppcre:scan "INR" str-instrument))
	   (/ quantity 1000))
	  (t (/ quantity 10000)))))
;; (float (from-pips :EUR_JPY 100))

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

(defun -validate-trades (trades older-than)
  "We use `older-than` to determine what trades to ignore due to possible lack of prices for validation."
  (push-to-log (format nil "Trying to validate ~a trades." (length trades)))
  (loop for trade in trades
	do (let* ((from (let ((entry-time (assoccess trade :entry-time))
			      (creation-time (assoccess trade :creation-time)))
			  (ceiling (* (if omage.config:*is-production*
					  ;; The exact time when the trade got created, e.g 4:33 PM.
					  creation-time
					  (if (not (equal entry-time :null))
					      ;; The time of the last traded candle in the testing dataset.
					      ;; This time will be a rounded hour (if using hours), e.g. 4:00 PM.
					      entry-time
					      creation-time))
				      1000000))))
		  (from-timestamp (local-time:unix-to-timestamp (ceiling (/ from 1000000)))))
	     (when (or (not omage.config:*is-production*)
		       (local-time:timestamp< from-timestamp
					      (local-time:timestamp- (local-time:now) older-than :day)))
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
		 (sleep 1))))))

(defun re-validate-trades (&optional (older-than 0) (last-n-days 30))
  (let ((trades (conn (query (:order-by (:select '*
					  :from (:as (:order-by (:select 'trades.* 'patterns.*
									 :distinct-on 'trades.id
									 :from 'trades
									 :inner-join 'patterns-trades
									 :on (:= 'trades.id 'patterns-trades.trade-id)
									 :inner-join 'patterns
									 :on (:= 'patterns.id 'patterns-trades.pattern-id)
									 :where (:> :creation-time (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) last-n-days :day))))
								'trades.id)
						     'results))
					(:desc 'creation-time))
			     :alists))))
    (-validate-trades trades older-than)))
;; (re-validate-trades 0 1)

(defun validate-trades (&optional (older-than 1))
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
    (-validate-trades trades older-than)))
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

(defun ->sma-close-strategy-1 (rates offset n)
  (let ((close-0 (->delta-close rates offset))
	(close-1 (->delta-close rates (1+ offset)))
	(sma (->sma-close rates offset n)))
    (if (or (and (< close-0 sma)
		 (> close-1 sma))
	    (and (> close-0 sma)
		 (< close-1 sma)))
	(- sma close-0)
	0)))
(defun ->sma-close-strategy-2 (rates offset n-short-sma n-long-sma)
  (let ((short-sma-0 (->sma-close rates offset (min n-short-sma n-long-sma)))
	(short-sma-1 (->sma-close rates (1+ offset) (min n-short-sma n-long-sma)))
	(long-sma (->sma-close rates offset (max n-short-sma n-long-sma))))
    (if (or (and (< short-sma-0 long-sma)
		 (> short-sma-1 long-sma))
	    (and (> short-sma-0 long-sma)
		 (< short-sma-1 long-sma)))
	(- short-sma-0 long-sma)
	0)))
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

(defun ->macd-close (rates offset n-short-sma n-short-ema n-long-sma n-long-ema n-signal)
  (let ((last-macd 0)
	(signal 0))
    (loop for off from (1- n-signal) downto 0
	  do (let* ((short-ema (->ema-close rates (+ off offset) n-short-sma n-short-ema))
		    (long-ema (->ema-close rates (+ off offset) n-long-sma n-long-ema))
		    (macd (- short-ema long-ema)))
	       (incf signal macd)
	       (setf last-macd macd)))
    (- last-macd (/ signal n-signal))))

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
     ->sma-close-strategy-1
     ->sma-close-strategy-2
     ->wma-close
     ->ema-close
     ->rsi-close

     ;; ->macd-close
     ;; ->delta-close
     ;; ->high-height
     ;; ->low-height
     ;; ->candle-height
     ))

(defun random->sma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,->sma-close ,offset ,n)
	    (+ offset n))))
(defun random->sma-close-strategy-1 ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,->sma-close-strategy-1 ,offset ,n)
	    (+ offset n 1))))
(defun random->sma-close-strategy-2 ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 50))
	(n-long-sma (random-int 3 50)))
    (values `#(,->sma-close-strategy-2 ,offset ,n-short-sma ,n-long-sma)
	    (+ offset n-short-sma n-long-sma 1))))
(defun random->wma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,->wma-close ,offset ,n)
	    (+ offset n))))
(defun random->ema-close ()
  (let ((offset (random-int 0 50))
	(n-sma (random-int 3 25))
	(n-ema (random-int 3 25)))
    (values `#(,->ema-close ,offset ,n-sma ,n-ema)
	    (+ offset n-sma n-ema))))
(defun random->macd-close ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 25))
	(n-short-ema (random-int 3 25))
	(n-long-sma (random-int 3 25))
	(n-long-ema (random-int 3 25))
	(n-signal (random-int 3 25)))
    (values `#(,->macd-close ,offset ,n-short-sma ,n-short-ema ,n-long-sma ,n-long-ema ,n-signal)
	    (+ offset (* 2 (max n-short-sma n-short-ema n-long-sma n-long-ema)) n-signal))))
(defun random->rsi-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 10 20)))
    (values `#(,->rsi-close ,offset ,n)
	    (+ offset n))))
(defun random->high-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,->high-height ,offset)
	    offset)))
(defun random->low-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,->low-height ,offset)
	    offset)))
(defun random->candle-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,->candle-height ,offset)
	    offset)))
(defun random->delta-close ()
  (let ((offset (random-int 0 50)))
    (values `#(,->delta-close ,offset)
	    offset)))

(defun gen-random-beliefs (fns-count)
  (let ((fns-bag `(,#'random->sma-close
		   ,#'random->sma-close-strategy-1
		   ,#'random->sma-close-strategy-2
		   ,#'random->wma-close
		   ,#'random->ema-close
		   ,#'random->rsi-close
		   ;; ,#'random->macd-close
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
      (:lookahead-count . ,(if omage.config:*random-lookahead-p*
			       (random-int
				omage.config:*random-lookahead-min*
				omage.config:*random-lookahead-max*)
			       omage.config:*lookahead*))
      (:lookbehind-count . ,(+ 10 max-lookbehind)))))
;; (gen-random-beliefs 30)

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn across perception-fns
	  collect (apply #'funcall
			 (nth-enum-tag (aref fn 0) 'perceptions)
			 rates (coerce (subseq fn 1) 'list)))))
;; (time (loop repeat 1000 do (funcall (gen-perception-fn #(#(0 0 10) #(0 1 10) #(1 0))) *rates*)))

(defclass dataset ()
  ((id :col-type string :initarg :id)
   (begin-time :col-type int8 :initarg :begin-time)
   (end-time :col-type int8 :initarg :end-time))
  (:metaclass dao-class)
  (:table-name datasets)
  (:keys id))

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
  (:metaclass dao-class)
  (:table-name trades)
  (:keys id))

(defun get-tp-sl (rates &optional (lookahead-count 10) (symmetricp nil))
  ;; We need to use `rate-open` because we're starting at that price after
  ;; calculating the inputs before this rate.
  (let* ((init-rate-ask (rate-open-ask (first rates)))
	 (init-rate-bid (rate-open-bid (first rates)))
	 (max-pos-ask 0)
	 (max-pos-bid 0)
	 (max-neg-ask 0)
	 (max-neg-bid 0))
    (loop for rate in (subseq rates 0 lookahead-count)
	  do (let ((delta-high-ask (- (rate-high-ask rate) init-rate-bid)) ;; Started as sell order, then close as ask.
		   (delta-high-bid (- (rate-high-bid rate) init-rate-ask)) ;; Started as buy order, then close as bid.
		   (delta-low-ask (- (rate-low-ask rate) init-rate-bid))
		   (delta-low-bid (- (rate-low-bid rate) init-rate-ask)))
	       ;; Checking for possible price stagnation. If true, ignore.
	       ;; (unless (and (< init-rate delta-high)
	       ;; 		    (> init-rate delta-low))
	       ;; 	 (when (> delta-high max-pos)
	       ;; 	   (setf max-pos delta-high))
	       ;; 	 (when (< delta-low max-neg)
	       ;; 	   (setf max-neg delta-low)))
	       (when (> delta-high-ask max-pos-ask)
		 (setf max-pos-ask delta-high-ask))
	       (when (> delta-high-bid max-pos-bid)
		 (setf max-pos-bid delta-high-bid))
	       (when (< delta-low-ask max-neg-ask)
		 (setf max-neg-ask delta-low-ask))
	       (when (< delta-low-bid max-neg-bid)
		 (setf max-neg-bid delta-low-bid))))
    ;; `((:tp . ,(if (>= snd-max-pos (abs snd-max-neg)) snd-max-pos snd-max-neg))
    ;;   ,(if symmetricp
    ;; 	   `(:sl . ,(if (>= snd-max-pos (abs snd-max-neg)) (* snd-max-pos -1.0) (* snd-max-neg -1.0)))
    ;; 	   `(:sl . ,(if (>= snd-max-pos (abs snd-max-neg)) snd-max-neg snd-max-pos))
    ;; 	   ))

    ;; Can we just use (>= max-pos-ask (abs max-neg-ask)) and totally ignore `-bid` in the condition?
    `((:tp . ,(if (>= max-pos-ask (abs max-neg-ask)) max-pos-bid max-neg-ask))
      ,(if symmetricp
	   `(:sl . ,(if (>= max-pos-ask (abs max-neg-ask)) (* max-pos-bid -1.0) (* max-neg-ask -1.0)))
	   `(:sl . ,(if (>= max-pos-ask (abs max-neg-ask)) max-neg-bid max-pos-ask))
	   ))
    
    ;; `((:tp . ,(if (>= max-pos (abs max-neg)) max-pos max-neg))
    ;;   ,(if symmetricp
    ;; 	   ;; `(:sl . ,(if (>= max-pos (abs max-neg)) (* max-pos -1.0) (* max-neg -1.0)))
    ;; 	   `(:sl . ,(if (>= max-pos (abs max-neg))
    ;; 			(if (> (/ max-pos 2) (abs max-neg))
    ;; 			    (* -1 (/ max-pos 2))
    ;; 			    max-neg)
    ;; 			(if (> (abs (/ max-neg 2)) max-pos)
    ;; 			    (* -1 (/ max-neg 2))
    ;; 			    max-pos)))
    ;; 	   `(:sl . ,(if (>= max-pos (abs max-neg)) max-neg max-pos))
    ;; 	   ;; `(:sl . ,(if (>= max-pos (abs max-neg)) (* max-pos -1.0) (* max-neg -1.0)))
    ;; 	   ))
    ))
;; (time (get-tp-sl (get-input-dataset *rates* 10)))

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
			  (> (abs (assoccess tp-sl :sl)) (from-pips instrument omage.config:*min-sl*))
			  (> (abs (/ (assoccess tp-sl :tp)
				     (assoccess tp-sl :sl)))
			     omage.config:*agents-min-rr-creation*)
			  (or (eq instrument :USD_CNH)
			      (< (abs (assoccess tp-sl :tp)) (from-pips instrument omage.config:*max-tp*))))
		 (push idx result))))
    (if (> (length result) 1)
	result
	(get-same-direction-outputs-idxs instrument rates count
					 :lookahead-count lookahead-count
					 :lookbehind-count lookbehind-count
					 :direction-fn opposite-pred))))
;; (get-same-direction-outputs-idxs *rates* :lookahead-count 5)

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
;; (gen-agent 3 *rates* (gen-random-beliefs 2) 10 55)

(defun gen-agents (num-agents num-rules instrument rates perception-fns lookahead-count lookbehind-count)
  (loop repeat num-agents collect (gen-agent instrument num-rules rates perception-fns lookahead-count lookbehind-count)))
;; (gen-agents 2 3 *rates* (assoccess *beliefs* :perception-fns) 10 55)

(defun evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (if (plusp tp)
			   ;; We need to use `open` because it's when we start.
			   (rate-open-ask (first rates))
			   (rate-open-bid (first rates))))
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
			      (rate-low-bid rate)
			      (rate-low-ask rate)))
		     (high (if (plusp tp)
			       (rate-high-bid rate)
			       (rate-high-ask rate)))
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

(comment
 (length (remove-if-not #'zerop
			(loop for i from 0 below 9000
			      collect (let* ((rates (get-output-dataset *rates* i))
					     (tp-sl (get-tp-sl rates))
					     (tp (assoccess tp-sl :tp))
					     (sl (assoccess tp-sl :sl)))
					(assoccess (evaluate-trade tp sl rates) :revenue))))))

(defun get-agents-count (instrument timeframe types)
  (length (get-agents instrument timeframe types)))

(defun alist-keys (alist)
  (loop for item in alist collect (car item)))
;; (alist-keys (car (prepare-agents-properties (get-agents :AUD_USD omage.config:*train-tf* '(:bullish)))))

(defun alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-values (car (prepare-agents-properties (get-agents :EUR_USD omage.config:*train-tf* '(:bullish)))))

(defun describe-agents ()
  (with-open-stream (s (make-string-output-stream))
    (format s "<h3>AGENTS POOL.</h3><hr/>")
    (loop for instrument in ominp:*forex*
	  do (loop for types in '((:bullish) (:bearish) (:stagnated))
		   do (let* ((agents-props (prepare-agents-properties (get-agents instrument omage.config:*train-tf* types)))
			     (agents-count (get-agents-count instrument omage.config:*train-tf* types))
			     (vals (loop for agent-props in agents-props
					 collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
						       (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
						   (append (alist-values agent-props)
							   (list (format-rr avg-sl avg-tp)))))))
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
;; (prepare-agents-properties (get-agents :AUD_USD omage.config:*train-tf* '(:stagnated)))

(defun get-all-agents ()
  (let ((markets (make-hash-table)))
    (loop for instrument in ominp:*forex*
	  do (let ((agents (make-hash-table)))
	       (loop for types in '(:bullish :bearish :stagnated)
		     do (let ((values (let* ((agents-props (prepare-agents-properties (get-agents instrument omage.config:*train-tf* (list types))))
					     (vals (loop for agent-props in agents-props
							 collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
								       (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
								   (append agent-props
									   `((:r/r . ,(format-rr avg-sl avg-tp))))))))
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
				   'trades.activation
				   'trades.decision
				   'trades.result
				   'trades.entry-price
				   'trades.entry-time
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
				'trades.activation
				'trades.decision
				'trades.result
				'trades.entry-price
				'trades.entry-time
				:distinct-on 'trades.id
				:from 'trades
				:inner-join 'patterns-trades
				:on (:= 'trades.id 'patterns-trades.trade-id)
				:inner-join 'patterns
				:on (:= 'patterns-trades.pattern-id 'patterns.id))
			      'trades.id
			      (:desc 'trades.creation-time))
		   :alists))))
;; (get-trades)

(defun -get-nested-trades (nested-limit)
  (conn (query (:select 
		   '*
		 :from
		 (:as (:select '*
			(:as (:over (:row-number)
				    (:partition-by 'instrument 'timeframe 'decision
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
			       'trades.activation
			       'trades.decision
			       'trades.entry-price
			       'trades.entry-time
			       :distinct-on 'trades.id
			       :from 'trades
			       :inner-join 'patterns-trades
			       :on (:= 'trades.id 'patterns-trades.trade-id)
			       :inner-join 'patterns
			       :on (:= 'patterns-trades.pattern-id 'patterns.id))
			     'full-results))
		      'idx-results)
		 :where (:and (:<= 'idx '$1)
			      (:in 'creation-time (:select 
			      			      (:max 'trades.creation-time)
			      			    :from 'trades
			      			    :inner-join 'patterns-trades
			      			    :on (:= 'trades.id 'patterns-trades.trade-id)
			      			    :inner-join 'patterns
			      			    :on (:= 'patterns-trades.pattern-id 'patterns.id)
			      			    :group-by 'patterns.instrument))
			      ))
	       nested-limit
	       :alists)))
;; (length (-get-nested-trades 20))

(defun get-nested-trades (nested-limit)
  (let (result)
    (loop for instrument in omage.config:*instruments*
	  do (let ((trades (remove-if-not (lambda (elt)
					    (string= elt (format nil "~a" instrument)))
					  (-get-nested-trades nested-limit)
					  :key (lambda (elt) (assoccess elt :instrument))))
		   (bullish)
		   (bearish))
	       ;; Separating trades into bearish and bullish.
	       (loop for trade in trades
		     do (if (plusp (assoccess trade :tp))
			    (push trade bullish)
			    (push trade bearish)))
	       ;; Creating nests with top activated and rest.
	       ;; (print (first bullish))
	       (let ((rbullish (reverse bullish))
		     (rbearish (reverse bearish)))
		 (when (first rbullish)
		   (push `((:first . ,(first rbullish))
			   (:rest . ,(rest rbullish)))
			 result))
		 (when (first rbearish)
		   (push `((:first . ,(first rbearish))
			   (:rest . ,(rest rbearish)))
			 result)))))
    (nreverse result)))
;; (get-nested-trades 20)

(defun describe-trades (&optional limit filter-fn)
  (let* ((trades (remove-if-not filter-fn (get-trades limit)))
	 (trades-won (loop for trade in trades
			   summing (assoccess trade :test-trades-won)))
	 (trades-lost (loop for trade in trades
			    summing (assoccess trade :test-trades-lost)))
	 (total-return (loop for trade in trades
			     summing (assoccess trade :test-total-return))))
    (when trades
      ;; (format t "Total trades won: ~a. Total trades lost: ~a. Total trades: ~a. ~%Total return: ~a. Avg return: ~a.~%~%"
      ;; 	      trades-won
      ;; 	      trades-lost
      ;; 	      (+ trades-won trades-lost)
      ;; 	      total-return
      ;; 	      (/ total-return (+ trades-won trades-lost)))
      (/ (loop for trade in trades
	       ;; when (and (not (eq (assoccess trade :result) :null))
	       ;; 		 ;; (not (string= (assoccess trade :instrument) "USD_CNH"))
	       ;; 		 )
	       ;; summing (to-pips
	       ;; 	  (assoccess trade :instrument)
	       ;; 	  (assoccess trade :result))
	       summing (to-pips
			(assoccess trade :instrument)
			(assoccess trade :test-avg-revenue)))
      	 (length trades))
      ;; (loop for trade in trades
      ;; 	    do (format t "market: :~a, result: ~a, test-total-return: ~5$, test-trades-won: ~a, test-trades-lost: ~a, rr: ~a~%"
      ;; 		       (assoccess trade :instrument)
      ;; 		       (assoccess trade :result)
      ;; 		       (assoccess trade :test-total-return)
      ;; 		       (assoccess trade :test-trades-won)
      ;; 		       (assoccess trade :test-trades-lost)
      ;; 		       (format-rr (assoccess trade :sl)
      ;; 				  (assoccess trade :tp))))
      )))
;; (get-trades 1)
;; (describe-trades nil (lambda (trade) (> (assoccess trade :activation) 0.0)))
;; (describe-trades nil (lambda (trade) t))

(comment
 (loop for act from 0 to 1 by 0.1
       do (format t "~a: ~a~%"
		  act
		  (describe-trades nil (lambda (trade) (> (assoccess trade :activation) act)))))
 )

(comment
 (loop for instrument in omage.config:*instruments*
       do (progn
	    (format t "~a: " instrument)
	    (loop for type in '((:bullish) (:bearish) (:stagnated))
		  do (format t "~a, " (length (get-agents instrument omage.config:*train-tf* type))))
	    (format t "~%"))
       finally (describe-trades nil (lambda (trade) (> (assoccess trade :activation) 0.8))))
 )

(defun read-str (str)
  (read-from-string str))

(defun get-perception-fn (agent)
  (gen-perception-fn (read-str (slot-value agent 'perception-fns))))

(defun eval-agent (agent rates)
  (let ((perception-fn (get-perception-fn agent)))
    (eval-ifis (funcall perception-fn rates)
	       (read-str (slot-value agent 'antecedents))
	       (read-str (slot-value agent 'consequents)))))

(defun buy-and-hold (rates)
  (- (rate-close (last-elt rates))
     (rate-close (first rates))))

;; (comment
;;  (loop for instrument in '(:EUR_USD :USD_JPY :USD_CHF :GBP_USD :USD_CAD :AUD_USD)
;;        collect (list instrument
;; 		     (buy-and-hold
;; 		      (get-rates-range-big instrument :D
;; 					   (unix-to-nano (local-time:timestamp-to-unix (local-time:parse-timestring "2004-08-28")))
;; 					   (unix-to-nano (local-time:timestamp-to-unix (local-time:parse-timestring "2020-05-18")))

;; (comment
;;  (defparameter *consensus* nil)
;;  (float (/ (count t *consensus*)
;; 	   (length *consensus*))))

(defun eval-agents (instrument timeframe types rates)
  (let (tps sls activations ids)
    (let ((agents (get-agents instrument timeframe types)))
      (loop for agent in agents
	    do (multiple-value-bind (tp sl activation)
		   (eval-agent agent rates)
		 (let* ((last-rate (last-elt rates))
			;; Checking if calculated SL is greater than Nx the current spread.
			;; If not, we set Nx the current spread as the SL.
			(corrected-sl (let ((nx-spread (* omage.config:*min-n-times-spread-sl*
							  (abs (- (rate-close-bid last-rate)
								  (rate-close-ask last-rate))))))
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
    (let ((idxs (sorted-indexes activations #'>))
	  (tp 0)
	  (sl 0)
	  (dir 0)
	  (activation 0)
	  ;; (consensus t)
	  (len (min omage.config:*consensus-threshold* (length activations)))
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
	  do (let* ((input-dataset (get-input-dataset rates idx))
		    (output-dataset (get-output-dataset rates idx)))
	       (multiple-value-bind (tp sl activation)
		   (if agent
		       (eval-agent agent input-dataset)
		       (eval-agents instrument timeframe types input-dataset))
		 (if (< activation omage.config:*evaluate-agents-activation-threshold*)
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
			       	  omage.config:*agents-min-rr-trading*)
			       (= tp 0)
			       (= sl 0)
			       (< (abs (to-pips instrument sl)) omage.config:*min-pips-sl*)
			       (> (abs (to-pips instrument sl)) omage.config:*max-pips-sl*)
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
		       (if omage.config:*trade-every-dp-p*
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
    	(:returns . ,(reverse returns))))
    ))
;; (evaluate-agents :EUR_USD omage.config:*train-tf* '(:BULLISH) *rates*)

(comment
 (defparameter *activations* nil)
 (defparameter *tps* nil)
 (defparameter *sls* nil)
 (defparameter *revenues* nil)

 (defparameter *proof* nil))

;; (format t "~a, ~a, ~a~%"
;; 	(length *activations*)
;; 	(length *tps*)
;; 	(length *sls*))

(comment
 (defun coco ()
   (let* ((sorted (sort (loop for act in *activations*
			      for rev in *revenues*
			      collect (list act rev))
			#'<
			:key #'first))
	  ;; (mean-act (/ (reduce #'+ sorted :key #'first) (length sorted)))
	  ;; (sorted (remove-if-not (lambda (elt) (> elt mean-act)) sorted :key #'first))
	  )
     ;; (cl-mathstats:correlation
     ;;  (mapcar #'second sorted)
     ;;  (mapcar #'first sorted))
     ;; (loop for s in sorted
     ;; 	do (format t "~a ~a~%" first s) (if (plusp (second s)) 1 -1)))

     ;; Extremes revenue.
     (when (and (> (length sorted) 1)
		(< (* (second (first sorted))
		      (second (last-elt sorted))) 0))
       (push (< (second (first sorted))
		(second (last-elt sorted)))
	     *proof*))
     ;; Extremes wins.
     ;; (when (and (> (length sorted) 1)
     ;; 	       (< (* (second (first sorted))
     ;; 		     (second (last-elt sorted))) 0))
     ;;   (push (and (< (second (first sorted)) 0)
     ;; 		 (> (second (last-elt sorted)) 0))
     ;; 	    *proof*))

     
     ;; Chunks.
     (let* ((step (floor (/ (length sorted) 2)))
	    (idx0 0)
	    (idx1 step))
       (when (and *activations*
		  ;; (> (mean *activations*) 0.5)
		  (> step 0))
	 (loop while (< (+ idx1 step) (length sorted))
	       do (let* ((fst (subseq sorted idx0 idx1))
			 (snd (subseq sorted idx1 (+ idx1 step)))
			 (fst-wins (remove-if-not #'plusp fst :key #'second))
			 (snd-wins (remove-if-not #'plusp snd :key #'second)))
		    (when (and (> (length fst-wins) 0)
			       (> (length snd-wins) 0)
			       (/= (length snd-wins) (length fst-wins)))
		      ;; Wins.
		      ;; (push (list (> (length snd-wins) (length fst-wins))
		      ;; 		 (list (/ idx0 step) (/ idx1 step)))
		      ;; 	   *proof*)
		      ;; Positive revenue.
		      ;; (push (list (> (/ (reduce #'+ snd-wins :key #'second) (length snd-wins))
		      ;; 		    (/ (reduce #'+ fst-wins :key #'second) (length fst-wins)))
		      ;; 		 (list (/ idx0 step) (/ idx1 step)))
		      ;; 	   *proof*)
		      ;; Revenue.
		      ;; (push (list (> (/ (reduce #'+ snd :key #'second) (length snd))
		      ;; 		    (/ (reduce #'+ fst :key #'second) (length fst)))
		      ;; 		 (list (/ idx0 step) (/ idx1 step)))
		      ;; 	   *proof*)
		      )
		    (incf idx0 step)
		    (incf idx1 step))))
       nil)



     
     
     ;; (ignore-errors
     ;;  (let* ((first-half (subseq sorted (* (round (/ (length sorted) 10)) 0) (* (round (/ (length sorted) 10)) 5)))
     ;; 	    (second-half (subseq sorted (* (round (/ (length sorted) 10)) 5)))

     ;; 	    (first-half-wins (remove-if-not #'plusp first-half :key #'second))
     ;; 	    (second-half-wins (remove-if-not #'plusp second-half :key #'second))

     ;; 	    (first-half-losses (remove-if-not #'minusp first-half :key #'second))
     ;; 	    (second-half-losses (remove-if-not #'minusp second-half :key #'second))
     
     ;; 	    (first-half-avg-revenue (/ (reduce #'+ first-half :key #'second) (length first-half)))
     ;; 	    (second-half-avg-revenue (/ (reduce #'+ second-half :key #'second) (length second-half)))

     ;; 	    (first-half-wins-avg-revenue (/ (reduce #'+ first-half-wins :key #'second) (length first-half-wins)))
     ;; 	    (second-half-wins-avg-revenue (/ (reduce #'+ second-half-wins :key #'second) (length second-half-wins)))

     ;; 	    (first-half-losses-avg-revenue (/ (reduce #'+ first-half-losses :key #'second) (length first-half-losses)))
     ;; 	    (second-half-losses-avg-revenue (/ (reduce #'+ second-half-losses :key #'second) (length second-half-losses)))
     ;; 	    )

     ;;    ;; Pushing how many times having a greater activation resulted in greater avg. revenue.
     ;;    ;; (format t "~a, ~a~%" second-half-avg-revenue first-half-avg-revenue)
     ;;    ;; (push (> second-half-avg-revenue first-half-avg-revenue) *proof*)
     ;;    (push (> (length second-half-wins) (length first-half-wins)) *proof*)
     ;;    ;; (format t "~a, ~a~%" second-half-wins-avg-revenue first-half-wins-avg-revenue)
     ;;    ;; (push (> second-half-wins-avg-revenue first-half-wins-avg-revenue) *proof*)
     ;;    ;; (push (> second-half-losses-avg-revenue first-half-losses-avg-revenue) *proof*)
     
     ;;    ;; (format t "~a ~a ~a~%"
     ;;    ;; 	      (> second-act first-act)
     ;;    ;; 	      first-act
     ;;    ;; 	      second-act
     ;;    ;; 	      )
     ;;    ))
     )))

;; (cl-mathstats:correlation *activations* *revenues*)
;; (ql:system-apropos "math")
;; (ql:quickload :cl-mathstats)

(defun evaluate-agent (agent rates &key test-size (return-fitnesses-p nil))
  (let ((fitnesses (-evaluate-agents :agent agent :rates rates :idx (slot-value agent 'lookbehind-count) :test-size test-size)))
    ;; (when (> (assoccess fitnesses :total-revenue) 0)
    ;;   (setf *activations* (assoccess fitnesses :activations))
    ;;   (setf *tps* (assoccess fitnesses :tps))
    ;;   (setf *sls* (assoccess fitnesses :sls))
    ;;   (setf *revenues* (assoccess fitnesses :revenues))
    ;;   (coco))
    ;; ACTPROOF
    ;; (comment
    ;;  (setf *activations* (assoccess fitnesses :activations))
    ;;  (setf *tps* (assoccess fitnesses :tps))
    ;;  (setf *sls* (assoccess fitnesses :sls))
    ;;  (setf *revenues* (assoccess fitnesses :revenues))
    ;;  (coco))
    ;; (ignore-errors (print (cl-mathstats:correlation *activations* *revenues*)))
    
    (loop for fitness in fitnesses
	  ;; TODO: We should be returning symbols (not kws) from -evaluate-agents.
;;; and then remove this format + read-from-string.
	  do (setf (slot-value agent (read-from-string (format nil "~a" (car fitness))))
		   (if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    (if return-fitnesses-p
	fitnesses
	agent)))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(defun evaluate-agents (instrument timeframe types rates &key (test-size 50))
  (-evaluate-agents :instrument instrument :timeframe timeframe :types types :rates rates :test-size test-size))
;; (time (evaluate-agents :EUR_USD omage.config:*train-tf* '(:BULLISH) (subseq *rates* 0 200)))

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

(defun is-agent-dominated? (agent agents &optional (logp nil))
  (if (or (= (length (slot-value agent 'tps)) 0)
  	  (<= (slot-value agent 'total-return) 0)
  	  (< (length (slot-value agent 'tps))
  	     omage.config:*min-num-trades-training*))
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
  				  (> avg-return-0 omage.config:*min-agent-avg-return*)
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
  					omage.config:*evaluate-agents-activation-threshold*))
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
;; (get-agent :EUR_USD omage.config:*train-tf* '(:BULLISH) "48F3970F-36C1-4A49-9E54-95746CFEA9FE")
;; (slot-value (first (get-agents :EUR_USD omage.config:*train-tf* '(:BULLISH))) 'id)

;; (comment
;;  (defmacro every-n-secs-run (seconds &rest body)
;;    )

;;  (ql:system-apropos "clerk")
;;  (ql:quickload :clerk)
;;  (clerk:*jobs*)
;;  (clerk:calendar)
;;  (clerk:empty-jobs-queue)
;;  (clerk:job "Creating signals" every 2.minutes (print "Hi"))
;;  (clerk:job "Creating signals" every 2.seconds (print "Hi"))

;;  (create-signals-job omage.config:*seconds-interval-testing*)

;;  (clerk:start)
;;  (clerk:stop)

;;  ;; (let ((now (local-time:now)))
;;  ;;   (loop when (local-time:timestamp- )))
;; )

(defun create-signals-job (seconds)
  (eval `(clerk:job "Creating signals" every ,(read-from-string (format nil "~a.seconds" seconds)) (-loop-test-all)))
  (clerk:start))

(defun optimization (instrument timeframe types gen-agent-fn rates seconds)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe types)))
		  (update-agents-fitnesses instrument timeframe types agents rates)
		  ;; (list (evaluate-agent (funcall gen-agent-fn) rates))
		  (loop repeat omage.config:*initial-agent-count*
			collect (evaluate-agent (funcall gen-agent-fn) rates))))
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
		 (let* ((challenger (list (evaluate-agent (funcall gen-agent-fn) rates)))
			(is-dominated? (when omage.config:*optimize-p*
					 (is-agent-dominated? (car challenger) agents t))))
		   ;; No longer the first iteration after this.
		   (setf first-iteration-p nil)
		   ;; Logging agent direction.
		   (push-to-agent-directions-log instrument timeframe types (slot-value (first challenger) 'avg-tp))
		   (unless is-dominated?
		     ;; Purging agents.
		     (loop for in-trial in agents
			   do (if (and omage.config:*optimize-p*
				       (is-agent-dominated? in-trial challenger))
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
		   )))))

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
;; (get-patterns :EUR_JPY omage.config:*train-tf* '(:BULLISH (:BEARISH) :STAGNATED))

(defun get-agent-ids-from-patterns (instrument timeframe types)
  (let* ((types (flatten types))
	 (patterns (get-patterns instrument timeframe types))
	 )
    ;; (loop for agent in (get-agents instrument timeframe types)
    ;; 	  collect (slot-value agent 'id))
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (assoccess pattern :id))))) :column))
    ))
;; (get-agent-ids-from-patterns :AUD_USD omage.config:*train-tf* '(:bullish))
;; (get-agent-ids-from-patterns :AUD_USD omage.config:*train-tf* '((:bullish) (:stagnated)))

(defun wipe-agents ()
  (conn (query (:delete-from 'agents :where (:= 1 1)))
	(query (:delete-from 'agents-patterns :where (:= 1 1)))))
;; (wipe-agents)

(defun get-agents-from-cache (instrument timeframe types)
  (gethash (list instrument timeframe types) *agents-cache*))

;; (get-agents :EUR_JPY omage.config:*train-tf* '(:BULLISH))
;; (get-agents-from-cache :EUR_JPY omage.config:*train-tf* '(:BULLISH))

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
;; (time (sync-agents :AUD_USD omage.config:*train-tf* '(:BULLISH)))

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
;; (time (get-agents :EUR_USD omage.config:*train-tf* '(:bullish :stagnated)))

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
						(rate-close-ask (last-elt rates))
						(rate-close-bid (last-elt rates)))
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
;; (sb-sprof:stop-profiling)

(defun get-agent-ids-patterns (agent-ids)
  "AGENT-IDS are the IDs of agents that participated in the creation of a signal. We retrieve a list of patterns associated to these AGENT-IDS."
  (loop for key being each hash-key of *agents-cache*
	for value being each hash-value of *agents-cache*
	when (find agent-ids value
		   :key (lambda (agent) (slot-value agent 'id))
		   :test (lambda (ids id) (find id ids :test #'string=)))
	  collect (car (third key))))
;; (get-agent-ids-patterns (list (slot-value (first (get-agents :AUD_USD omage.config:*train-tf* '(:bearish))) 'id) (slot-value (first (get-agents :AUD_USD omage.config:*train-tf* '(:stagnated))) 'id)))

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
		 (> (abs (to-pips instrument sl)) 3)
		 ;; (< (to-pips instrument (abs sl)) 20)
		 (/= (assoccess test-fitnesses :trades-won) 0)
		 (/= (+ (assoccess test-fitnesses :trades-won)
			(assoccess test-fitnesses :trades-lost))
		     0))
	(push-to-log (format nil "Trying to create trade. Agents IDs: ~a" agent-ids))
	(insert-trade (first agent-ids) instrument timeframe (first (get-agent-ids-patterns agent-ids)) test-fitnesses test-fitnesses tp sl activation testing-dataset (local-time:timestamp-to-unix (local-time:now)))
	(push-to-log "Trade created successfully.")))))

(defun trade-most-activated-agents (instrument timeframe types agents testing-dataset creation-time &key (test-size 50))
  "Used in `test-most-activated-agents`."
  (loop for agent in agents
	do (let ((test-fitnesses (evaluate-agent agent testing-dataset :test-size test-size :return-fitnesses-p t))
		 (agent-id (slot-value agent 'id)))
	     (when test-fitnesses
	       (push-to-log "Testing process successful."))
	     (multiple-value-bind (tp sl activation)
		 (eval-agent agent testing-dataset)
	       (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
	       (when (/= (+ (assoccess test-fitnesses :trades-won)
			    (assoccess test-fitnesses :trades-lost))
			 0)
		 (push-to-log (format nil "Trying to create trade. Agents ID: ~a" agent-id))
		 (insert-trade agent-id instrument timeframe types test-fitnesses test-fitnesses tp sl activation testing-dataset creation-time)
		 (push-to-log "Trade created successfully."))))))

(defun test-most-activated-agents (instrument timeframe types testing-dataset &key (test-size 50))
  (let ((creation-time (local-time:timestamp-to-unix (local-time:now))))
    (multiple-value-bind (bullish-agents bearish-agents)
	(get-most-activated-agents instrument timeframe types)
      (trade-most-activated-agents instrument timeframe '(:BULLISH) bullish-agents testing-dataset creation-time :test-size test-size)
      (trade-most-activated-agents instrument timeframe '(:BEARISH) bearish-agents testing-dataset creation-time :test-size test-size))))

(defun get-clerk-jobs ()
  (with-open-stream (s (make-string-output-stream))
    (clerk:calendar s)
    (get-output-stream-string s)))
;; (get-clerk-jobs)

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
    (when omage.config:*is-log*
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
    (when omage.config:*is-log*
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

;; (push-to-agent-directions-log :AUD_USD omage.config:*train-tf* '(:bullish) 1.1)
;; (read-agent-directions-log)

(defun clear-logs ()
  (clear-log)
  (clear-agents-log)
  (clear-agent-directions-log))

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
;; (get-rates-count-big :EUR_USD omage.config:*train-tf* 10)

(defun get-random-rates-count-big (instrument timeframe count)
  "Assumes a minimum of 50K rates"
  (let* ((offset (random-int 0 (- 50000 count))))
    (reverse (conn (query (:limit (:order-by (:select '* :from 'rates :where (:and (:= 'instrument (format nil "~a" instrument))
										   (:= 'timeframe (format nil "~a" timeframe))))
					     ;; TODO: It's not a good idea to sort by time, considering it's a string. The good news is that we don't have to worry about this until year ~2200.
					     (:desc 'rates.time))
				  '$1 '$2)
			  count offset
			  :alists)))))
;; (get-random-rates-count-big :EUR_USD omage.config:*train-tf* 10)

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
;; (get-rates-range-big :EUR_USD omage.config:*train-tf* (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 20 :day))) (unix-to-nano (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 10 :day))))

;; (get-rates-count-big :EUR_USD omage.config:*train-tf* 100)

;; (loop for rate in (get-random-rates-count-big :EUR_USD omage.config:*train-tf* 200)
;;       do (print (local-time:unix-to-timestamp (/ (read-from-string (assoccess rate :time)) 1000000))))

;; (get-random-rates-count-big :AUD_USD omage.config:*train-tf*
;; 				    6200)

(defun refresh-memory ()
  (fare-memoization:unmemoize 'read-str)
  (sb-ext:gc :full t)
  (fare-memoization:memoize 'read-str))

(defun update-creation-training-dataset (type pattern instrument timeframe start end)
  (setf (gethash (list instrument timeframe pattern type) *creation-training-datasets*)
	(list start end))
  "")

(defun sync-datasets-to-database ()
  (conn
   (loop for id being each hash-key of *creation-training-datasets*
	 for begin-end being each hash-value of *creation-training-datasets*
	 do (let* ((id (format nil "~s" id))
		   (begin-time (first begin-end))
		   (end-time (second begin-end))
		   (dataset (get-dao 'dataset id)))
	      (if dataset
		  ;; Checking if we need to update the dataset.
		  ;; Reading the DB is fast, but writing to it is a costly operation, so it's worth checking.
		  (when (and (/= (slot-value dataset 'begin-time) begin-time)
			     (/= (slot-value dataset 'end-time) end-time))
		    (setf (slot-value dataset 'begin-time) begin-time)
		    (setf (slot-value dataset 'end-time) end-time)
		    (update-dao dataset))
		  ;; It doesn't exist. Let's initialize it.
		  (make-dao 'dataset
			    :id id
			    :begin-time begin-time
			    :end-time end-time))))))

(defun sync-datasets-from-database ()
  (loop for dataset in (conn (query (:select '* :from 'datasets) :alists))
	do (setf (gethash (read-from-string (assoccess dataset :id)) *creation-training-datasets*)
		 (list (assoccess dataset :begin-time)
		       (assoccess dataset :end-time)))))

(defun get-datasets ()
  (let ((datasets (alexandria:hash-table-alist *creation-training-datasets*)))
    (if datasets
	(loop for dataset in datasets
	      collect `((:segment . ,(first dataset))
			(:from . ,(local-time:unix-to-timestamp (unix-from-nano (second dataset))))
			(:to . ,(local-time:unix-to-timestamp (unix-from-nano (third dataset))))))
	nil)))
;; (get-datasets)

(defun format-datasets ()
  (let ((data (get-datasets)))
    (when data
      (let ((datasets (sort data ;; TODO: Sorting in a very, very naughty way.
			    #'string< :key (lambda (elt) (format nil "~a" (assoccess elt :segment)))))
	    (table-labels '("FROM" "TO")))
	(with-open-stream (s (make-string-output-stream))
	  (loop for dataset in datasets
		do (let ((segment (assoccess dataset :segment))
			 (from (assoccess dataset :from))
			 (to (assoccess dataset :to)))
		     (format s "~%~{~a~^, ~}~%" segment)
		     (format s "------------------------~%")
		     (format-table s `((,from
					,to
					))
				   :column-label table-labels)
		     ;; (format s "</pre><hr/>")
		     ))
	  (get-output-stream-string s)
	  )))))
;; (format-datasets)

(defun -loop-validate ()
  (when omage.config:*is-production*
    (push-to-log "<b>VALIDATION.</b><hr/>")
    (push-to-log "Validating trades older than 24 hours.")
    (validate-trades)))

(defun -loop-test (instrument timeframe type-groups testing-dataset)
  (push-to-log "<b>SIGNAL.</b><hr/>")
  (push-to-log (format nil "Trying to create signal for ~a ~a." instrument timeframe))
  (if omage.config:*use-nested-signals-p*
      (test-most-activated-agents instrument timeframe type-groups testing-dataset :test-size omage.config:*test-size*)
      (test-agents instrument timeframe type-groups testing-dataset :test-size omage.config:*test-size*)))

;; TODO: Rename everywhere from TYPE -> STAGE where applicable (type being :creation, :training or :testing).
;; Most of the time TYPE is '(:BULLISH), for example.
;; TODO: Rename everywhere from TYPE/TYPES to PATTERN/PATTERNS.
(defun -loop-get-dataset (instrument timeframe types stage dataset)
  "Stage can be :training or :creation."
  (let ((type (first (flatten types))))
    (if-let ((begin-end (gethash (list instrument timeframe type stage) *creation-training-datasets*)))
      (let ((begin-time (first begin-end))
	    (end-time (second begin-end)))
	(get-rates-range-big instrument timeframe begin-time end-time))
      (multiple-value-bind (from to)
	  (get-rates-chunk-of-types dataset types
				    :slide-step (if (eq stage :training)
						    omage.config:*train-slide-step*
						    omage.config:*creation-slide-step*)
				    :min-chunk-size (if (eq stage :training)
							omage.config:*train-min-chunk-size*
							omage.config:*creation-min-chunk-size*)
				    :max-chunk-size (if (eq stage :training)
							omage.config:*train-max-chunk-size*
							omage.config:*creation-max-chunk-size*)
				    :stagnation-threshold omage.config:*stagnation-threshold*)
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
		  (lambda () (let ((beliefs (gen-random-beliefs omage.config:*number-of-agent-inputs*)))
			       (gen-agent omage.config:*number-of-agent-rules*
					  instrument
					  creation-dataset
					  (assoccess beliefs :perception-fns)
					  (assoccess beliefs :lookahead-count)
					  (assoccess beliefs :lookbehind-count))))
		  training-dataset
		  omage.config:*seconds-to-optimize-per-pattern*)
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
  (if omage.config:*is-production*
      (get-rates-count-big instrument timeframe
			   (+ omage.config:*max-creation-dataset-size* omage.config:*max-training-dataset-size* omage.config:*max-testing-dataset-size*))
      (get-random-rates-count-big instrument timeframe
				  (+ omage.config:*max-creation-dataset-size* omage.config:*max-training-dataset-size* omage.config:*max-testing-dataset-size*))))

(defun -loop-test-all ()
  "We run this every `omage.config:*seconds-interval-testing*` seconds."
  (dolist (instrument omage.config:*instruments*)
    (dolist (timeframe omage.config:*timeframes*)
      omage.config:*max-testing-dataset-size*
      (unless (is-market-close)
	(let ((agents-count (get-agents-count instrument timeframe omage.config:*type-groups*)))
	  (when (> agents-count 0)
	    (let* ((testing-dataset (get-rates-count-big instrument timeframe
							 omage.config:*max-testing-dataset-size*)))
	      (-loop-test instrument timeframe omage.config:*type-groups* testing-dataset))))
	;; We don't want our data feed provider to ban us.
	;; We also want to go easy on those database reads (`agents-count`).
	(sleep 1)))))

(defun -loop-optimize-test-validate ()
  ;; Signal creation. Production. We create a cron job for this to be
  ;; run every `omage.config:*seconds-interval-testing*` seconds.
  (when omage.config:*is-production*
    (create-signals-job omage.config:*seconds-interval-testing*))
  (pmap nil (lambda (instrument)
	      (dolist (timeframe omage.config:*timeframes*)
		(unless (is-market-close)
		  (push-to-log (format nil "<br/><b>STARTING ~s ~s.</b><hr/>" instrument timeframe))
		  (let* ((rates (-loop-get-rates instrument timeframe))
			 (dataset-size (length rates)))
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
			  (testing-dataset (when (not omage.config:*is-production*)
					     (let ((dataset (subseq rates
								    (- dataset-size
								       omage.config:*max-testing-dataset-size*))))
					       (-loop-log-testing-dataset dataset)
					       dataset)))
			  (agents-count (get-agents-count instrument timeframe omage.config:*type-groups*)))
		      ;; Optimization.
		      (loop for types in omage.config:*type-groups*
			    do (-loop-optimize instrument timeframe types full-creation-dataset full-training-dataset agents-count))
		      ;; Signal creation. Development.
		      (when (not omage.config:*is-production*)
			(-loop-test instrument timeframe omage.config:*type-groups* testing-dataset))))
		  (-loop-validate))
		(refresh-memory)
		(sync-datasets-to-database)))
	omage.config:*instruments*)
  (unless omage.config:*is-production*
    (wipe-agents)))

(defun loop-optimize-test ()
  (handler-bind ((error (lambda (c)
			  (log-stack c))))
    (when (is-market-close)
      (format t "~%===============================================~%MARKET IS CLOSED~%SWITCH TO DEVELOPMENT MODE IF YOU WANT TO RUN EXPERIMENTS.~%==============================================="))
    (clear-logs)
    (refresh-memory)
    (sync-datasets-from-database)
    (loop (unless (is-market-close))
	  (-loop-optimize-test-validate))))

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
	 (rrs (remove nil (loop for result in results collect (if (= (assoccess result :tp) 0)
								  nil
								  (* (/ (assoccess result :sl)
									(assoccess result :tp))
								     (if (plusp (assoccess result :tp))
									 -1
									 1))))))
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

(defun con (y x0 x1)
  (let* ((m (/ 1
	       (- x1 x0)))
	 (b (+ (* m (- x0)) 0)))
    (/ (- y b) m)))
;; (con 0.0 -5 0)
;; (con 0.0 0 -5)

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
	(len (length inputs))
	(winner-gm 0)
	(winner-idx 0)
	(num-rules (length (aref input-antecedents 0))))
    ;; Calculating most activated antecedents.
    (loop
      for idx from 0 below num-rules
      do (let ((gm 0))
	   (loop for antecedents across input-antecedents
		 for input in inputs
		 do (let* ((antecedent (aref antecedents idx))
			   (act (ant input (aref antecedent 0) (aref antecedent 1))))
		      (when (and (<= act 1)
				 (>= act 0))
			(incf gm act))))
	   (when (>= gm winner-gm)
	     (setf winner-idx idx)
	     (setf winner-gm gm))))
    ;; Calculating outputs (TP & SL).
    (let ((activation (/ winner-gm len)))
      (loop
	for consequents across input-consequents
	do (progn
	     (incf tp (con activation
			   (aref (aref (aref consequents winner-idx) 0) 0)
			   (aref (aref (aref consequents winner-idx) 0) 1)))
	     (incf sl (con activation
			   (aref (aref (aref consequents winner-idx) 1) 0)
			   (aref (aref (aref consequents winner-idx) 1) 1)))))
      (values
       (/ tp len)
       (/ sl len)
       activation))))

;; Tipping problem.
;; Food Q, Service Q
;; (loop for f from 0 upto 10
;;       do (progn
;; 	   (loop for s from 0 upto 10
;; 		 do (let ((inputs `(,f ,s))
;; 			  (input-antecedents (vector (vector #(0 4)
;; 							     #(3 7)
;; 							     #(6 10))
;; 						     (vector #(0 4)
;; 							     #(3 7)
;; 							     #(6 10))
;; 						     ))
;; 			  (input-consequents (vector (vector (vector #(0 5) #(0 1))
;; 							     (vector #(5 10) #(0 1))
;; 							     (vector #(10 15) #(0 1)))
;; 						     (vector (vector #(0 0.01) #(0 1))
;; 							     (vector #(5 10) #(0 1))
;; 							     (vector #(10 15) #(0 1))))))
;; 		      (format t "~2$, " (float (eval-ifis inputs input-antecedents input-consequents)))))
;; 	   (format t "~%")))

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
						     (vector (* omage.config:*n-times-sl-for-max-sl* sl) sl))
					     ;; We need to repeat the consequent for the "other side" of the triangle.
					     (vector (vector 0 tp)
						     (vector (* omage.config:*n-times-sl-for-max-sl* sl) sl))
					     )))))
	    (one-set-outputs-v (make-array (length one-set-outputs) :initial-contents one-set-outputs))
	    (v (loop repeat (length (first chosen-inputs))
		     collect one-set-outputs-v)))
       (make-array (length v) :initial-contents v)))))

;; (defparameter *rates* (get-random-rates-count-big :AUD_USD omage.config:*train-tf* 1000))
;; (make-ifis (gen-agent 3 :EUR_USD *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 3 :EUR_USD *rates*)
;; (time
;;  (evaluate-agent (let ((beliefs (gen-random-beliefs omage.config:*number-of-agent-inputs*)))
;; 		   (gen-agent omage.config:*number-of-agent-rules*
;; 			      :AUD_USD
;; 			      *rates*
;; 			      (assoccess beliefs :perception-fns)
;; 			      (assoccess beliefs :lookahead-count)
;; 			      (assoccess beliefs :lookbehind-count)))
;; 		 *rates* :return-fitnesses-p t))

;; (slot-value (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 10) 'perception-fns)
;; (insert-agent (gen-agent 2 *rates* (assoccess (gen-random-beliefs 2) :perception-fns) 10 16) :EUR_USD omage.config:*train-tf* '(:BULLISH))
