(defpackage overmind-agents.api
  (:use #:cl #:postmodern #:alexandria)
  (:import-from #:ominp.db
		#:conn)
  (:import-from #:omcom.utils
		#:assoccess
		#:comment
		#:format-table)
  (:import-from #:ominp.rates
		#:to-pips
		#:get-rates-range-big)
  (:import-from #:omage.trading
		#:get-agents
		#:get-agents-count)
  (:import-from #:omage.log
		#:push-to-log)
  (:import-from #:omage.utils
	        #:prepare-agents-properties
		#:format-rr)
  (:export #:get-all-agents
	   #:get-trades
	   #:get-nested-trades
	   #:describe-trades
	   #:describe-agents
	   #:get-trade-result
	   #:get-global-revenue
	   #:validate-trades
	   #:re-validate-trades
	   #:delete-trades)
  (:nicknames #:omage.api))
(in-package :overmind-agents.api)

(defun get-all-agents ()
  (let ((markets (make-hash-table)))
    (loop for instrument in omcom.omage:*forex*
	  do (let ((agents (make-hash-table)))
	       (loop for types in '(:bullish :bearish :stagnated)
		     do (let ((values (let* ((agents-props (prepare-agents-properties (get-agents instrument omcom.omage:*train-tf* (list types))))
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

(defun get-trades (&optional limit)
  (if limit
      (conn (query (:select 
		    '*
		    :from
		    (:as (:select '*
				  (:as (:over (:row-number)
					      (:partition-by 'instrument 'timeframe
							     :order-by
							     (:desc 'creation-time)
							     (:desc 'activation)))
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
			     (:in 'creation-time (:select (:max 'trades.creation-time)
						  :from 'trades
						  :inner-join 'patterns-trades
						  :on (:= 'trades.id 'patterns-trades.trade-id)
						  :inner-join 'patterns
						  :on (:= 'patterns-trades.pattern-id 'patterns.id)
						  :group-by 'patterns.instrument))))
	       nested-limit
	       :alists)))

;; (length (-get-nested-trades 20))

(defun get-nested-trades (nested-limit)
  (let ((trades (-get-nested-trades nested-limit))
	(result))
    (loop for instrument in omcom.omage:*instruments*
	  do (let ((trades (remove-if-not (lambda (elt)
					    (string= elt (format nil "~a" instrument)))
					  trades
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
	       (let ((rbullish (sort bullish #'> :key (lambda (elt) (assoccess elt omcom.omage:*trades-sort-by*))))
		     (rbearish (sort bearish #'> :key (lambda (elt) (assoccess elt omcom.omage:*trades-sort-by*)))))
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
	 ;; (trades-won (loop for trade in trades
	 ;; 		   summing (assoccess trade :test-trades-won)))
	 ;; (trades-lost (loop for trade in trades
	 ;; 		    summing (assoccess trade :test-trades-lost)))
	 ;; (total-return (loop for trade in trades
	 ;; 		     summing (assoccess trade :test-total-return)))
	 )
    (when trades
      ;; (format t "Total trades won: ~a. Total trades lost: ~a. Total trades: ~a. ~%Total return: ~a. Avg return: ~a.~%~%"
      ;; 	      trades-won
      ;; 	      trades-lost
      ;; 	      (+ trades-won trades-lost)
      ;; 	      total-return
      ;; 	      (/ total-return (+ trades-won trades-lost)))
      (values
       (/ (loop for trade in trades
		;; when (and (not (eq (assoccess trade :result) :null))
		;; 		 ;; (not (string= (assoccess trade :instrument) "USD_CNH"))
		;; 		 )
		;; summing (to-pips
		;; 	  (assoccess trade :instrument)
		;; 	  (assoccess trade :result))
		summing (to-pips
			 (assoccess trade :instrument)
			 (assoccess trade :test-avg-revenue))
		)
	  (length trades))
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
;; (describe-trades 1000 (lambda (trade) (> (assoccess trade :activation) 0.0)))
;; (describe-trades nil (lambda (trade) t))

(defun alist-keys (alist)
  (loop for item in alist collect (car item)))
;; (alist-keys (car (prepare-agents-properties (get-agents :AUD_USD omage.config:*train-tf* '(:bullish)))))

(defun alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-values (car (prepare-agents-properties (get-agents :EUR_USD omage.config:*train-tf* '(:bullish)))))

(defun describe-agents ()
  (with-open-stream (s (make-string-output-stream))
    (format s "<h3>AGENTS POOL.</h3><hr/>")
    (loop for instrument in omcom.omage:*forex*
	  do (loop for types in '((:bullish) (:bearish) (:stagnated))
		   do (let* ((agents-props (prepare-agents-properties (get-agents instrument omcom.omage:*train-tf* types)))
			     (agents-count (get-agents-count instrument omcom.omage:*train-tf* types))
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

(comment
 (let ((step 0.1))
   (loop for instrument in omcom.omage:*instruments*
	 do (progn
	      (format t "~a~%" instrument)
	      (loop for act from 0 to 1 by step
		    do (multiple-value-bind (avg len)
			   (describe-trades 1000 (lambda (trade) (and (not (eq (assoccess trade :activation) :null))
								      (string= (assoccess trade :instrument)
									       (format nil "~a" instrument))
								      ;; (> (abs (/ (assoccess trade :tp)
								      ;; 		 (assoccess trade :sl)))
								      ;; 	 (+ 1 act))
								      ;; (< (abs (/ (assoccess trade :tp)
								      ;; 		 (assoccess trade :sl)))
								      ;; 	 (+ 1 act step))
								      (> (assoccess trade :test-avg-activation) act)
								      (< (assoccess trade :test-avg-activation) (+ act step))
								      )))
			 (format t "~$: ~a: ~a~%"
				 act
				 len
				 avg)))
	      (format t "~%")
	      )))
 )

(comment
 (loop for instrument in omcom.omage:*instruments*
       do (progn
	    (format t "~a: " instrument)
	    (loop for type in '((:bullish) (:bearish) (:stagnated))
		  do (format t "~a, " (length (get-agents instrument omcom.omage:*train-tf* type))))
	    (format t "~%"))
       finally (describe-trades 300 (lambda (trade) (and (not (eq (assoccess trade :activation) :null))
							 (> (assoccess trade :activation) 0.8)))))
 )

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
;; (loop for i from 0 below 10 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) (1+ i) :day)))))

(defun -get-trade-time (trade)
  "Used in `-validate-trades`."
  (let ((entry-time (assoccess trade :entry-time))
	(creation-time (assoccess trade :creation-time)))
    (ceiling (* (if omcom.all:*is-production*
		    ;; The exact time when the trade got created, e.g 4:33 PM.
		    creation-time
		    (if (not (equal entry-time :null))
			;; The time of the last traded candle in the testing dataset.
			;; This time will be a rounded hour (if using hours), e.g. 4:00 PM.
			entry-time
			creation-time))
		1000000))))

(defun -validate-trades (instrument trades older-than)
  "We use `older-than` to determine what trades to ignore due to possible lack of prices for validation."
  (when (> (length trades) 0)
    (push-to-log (format nil "Trying to validate ~a trades." (length trades)))
    (let* ((oldest (first (sort (copy-sequence 'list trades) #'< :key #'-get-trade-time)))
	   ;; (newest (first (sort (copy-sequence 'list trades) #'> :key #'-get-trade-time)))
	   (rates (get-rates-range-big instrument omcom.omage:*validation-timeframe*
				       (-get-trade-time oldest)
				       ;; (-get-trade-time newest)
				       (* (local-time:timestamp-to-unix (local-time:now)) 1000000)
				       )))
      (loop for trade in trades
	    do (let* ((idx (position (-get-trade-time trade) rates :test #'<= :key (lambda (rate) (read-from-string (assoccess rate :time))))))
		 (when idx
		   (let ((sub-rates (subseq rates idx))
			 (from-timestamp (local-time:unix-to-timestamp (ceiling (/ (-get-trade-time trade) 1000000)))))
		     (when (or (not omcom.all:*is-production*)
			       (local-time:timestamp< from-timestamp
						      (local-time:timestamp- (local-time:now) older-than :day)))
		       (push-to-log (format nil "Using minute rates from ~a to ~a to validate trade."
					    from-timestamp
					    (local-time:timestamp+ from-timestamp 3 :day)))
		       (let* ((result (get-trade-result (assoccess trade :entry-price)
							(assoccess trade :tp)
							(assoccess trade :sl)
							sub-rates)))
			 (push-to-log (format nil "Result obtained for trade: ~a." result))
			 (conn
			  (let ((dao (get-dao 'trade (assoccess trade :id))))
			    (setf (slot-value dao 'result) (if result result :NULL))
			    (update-dao dao)))
			 ))))))
      (sleep 1))))

(defun re-validate-trades (&optional (older-than 0) (last-n-days 30))
  (loop for instrument in omcom.omage:*instruments*
	do (let ((trades (conn (query (:order-by (:select '*
						  :from (:as (:order-by (:select 'trades.* 'patterns.*
										 :distinct-on 'trades.id
										 :from 'trades
										 :inner-join 'patterns-trades
										 :on (:= 'trades.id 'patterns-trades.trade-id)
										 :inner-join 'patterns
										 :on (:= 'patterns.id 'patterns-trades.pattern-id)
										 :where (:and
											 (:= 'patterns.instrument (format nil "~a" instrument))
											 (:> :creation-time (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) last-n-days :day)))))
									'trades.id)
							     'results))
						 (:desc 'creation-time))
				      :alists))))
	     (-validate-trades instrument trades older-than))))
;; (time (re-validate-trades 0 5))

(defun validate-trades (&optional (older-than 1))
  (loop for instrument in omcom.omage:*instruments*
	do (let ((trades (conn (query (:order-by (:select '*
						  :from (:as (:order-by (:select 'trades.* 'patterns.*
										 :distinct-on 'trades.id
										 :from 'trades
										 :inner-join 'patterns-trades
										 :on (:= 'trades.id 'patterns-trades.trade-id)
										 :inner-join 'patterns
										 :on (:= 'patterns.id 'patterns-trades.pattern-id)
										 ;; :where (:not (:is-null 'trades.result))
										 :where (:and
											 (:= 'patterns.instrument (format nil "~a" instrument))
											 (:is-null 'trades.result)))
									'trades.id)
							     'results))
						 (:desc 'creation-time))
				      :alists))))
	     (when trades
	       (-validate-trades instrument trades older-than)))))
;; (validate-trades)

(defun delete-trades (from to)
  (conn
   (with-transaction ()
     ;; Deleting patterns-trades.
     (execute (:delete-from 'patterns-trades
	       :where (:in 'pattern-id
			   (:select 'id :from 'trades :where (:and (:>= 'creation-time from)
								   (:<= 'creation-time to))))))
     ;; Deleting the actual trades.
     (execute (:delete-from 'trades
	       :where (:and (:>= 'creation-time from)
			    (:<= 'creation-time to)))))))

;; (delete-trades (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 5 :hour))
;; 	       (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 4 :hour)))

;; (conn (query (:select (:count 'id) :from 'trades)))

(defun get-trade-result (entry-price tp sl rates)
  (let ((low-type (if (plusp tp) :low-bid :low-ask))
	(high-type (if (plusp tp) :high-bid :high-ask)))
    (loop for rate in rates do
      (progn
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
