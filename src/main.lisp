;; (ql:quickload :overmind-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
;; (loop-optimize-test 100 :instruments-keys '(:forex) :timeframes-keys '(:all) :print-log? t)
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
        :sxql
        :datafly
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
	)
  (:export :query-test-instruments
	   :query-test-timeframes
	   :loop-optimize-test)
  (:nicknames :omage))
(in-package :overmind-agents)

(defparameter *instrument* :EUR_USD)
(defparameter *timeframe* :H1)
(defparameter *population* nil)
(defparameter *testing-ratio* 0.0)
(defparameter *fitness-fn* nil)
(defparameter *generations* nil)
(defparameter *rules-config* nil)
(defparameter *rates* nil)
(defparameter *all-rates* nil)
(defparameter *min-dataset-size* 200)
(defparameter *activation-level* (floor (/ (1- omper:*data-count*) 4)))

;; (progn
;;   (defparameter *instrument* :SUNSPOT
;;     "The financial instrument used for querying the data used to train or test.")
;;   (defparameter *timeframe* :Y
;;     "The timeframe used for querying the data used to train or test.")
  
;;   (defparameter *all-rates* (mapcar (lambda (time point)
;; 				      `((:time . ,(format nil "~a" time))
;; 					(:close-bid . ,(* 1 point))))
;; 				    (iota (length *sunspot*))
;; 				    *sunspot*)
;;     "All the rates. Subsets are used during training, validation and testing stages."))

;; (progn
;;   (defparameter *instrument* ;; :EUR_USD ;; :SPX500_USD ;; :BCO_USD
;;     :NATGAS_USD
;;     "The financial instrument used for querying the data used to train or test.")
;;   (defparameter *timeframe* :H1
;;     "The timeframe used for querying the data used to train or test.")
;;   (defparameter *all-rates* (get-rates *instrument* 2 *timeframe*)
;;     "All the rates. Subsets are used during training, validation and testing stages."))

;; (length *all-rates*)

;; (progn
;;   (defparameter *instrument* :SKY
;;     "The financial instrument used for querying the data used to train or test.")
;;   (defparameter *timeframe* :D
;;     "The timeframe used for querying the data used to train or test.")
;;   (defparameter *all-rates* (reverse
;; 			     (mapcar (lambda (tuple)
;; 				      `((:time . ,(format nil "~a" (local-time:timestamp-to-unix (local-time:parse-timestring (nth 0 tuple)))))
;; 					(:close-bid . ,(read-from-string (nth 4 tuple))))
;; 				      )
;; 				    (cl-csv:read-csv #P"~/skycoin.csv" :separator #\Comma)))
;;     "All the rates. Subsets are used during training, validation and testing stages."))

(defun mean-test-error (&optional (timeframe :H1))
  (let ((numerator 0)
	(denominator 0)
	(markets 0))
    (dolist (test (query-test-instruments timeframe))
      (when (/= (aref (access test :accuracy) 1) 0)
	(incf numerator (aref (access test :accuracy) 0))
	(incf denominator (aref (access test :accuracy) 1))
	(incf markets)))
    (format t "Mean: ~$%, Corrects: ~a, Trades: ~a, Markets: ~a~%"
	    (if (= denominator 0) 0 (float (* 100 (/ numerator denominator)))) numerator denominator markets)))
;; (query-test-instruments :H1)
;; (mean-test-error :H1)
;; (mean-test-error :D)

(defmacro try-until-successful (body)
  "Repeatedly runs `BODY` until it doesn't throw any errors."
  `(let (result)
     (loop while (null result)
	do (ignore-errors
	     (setf result (progn ,body))
	     ))
     result))

(defun compress-population (population)
  "Compresses a `population`."
  ;; (compress-population *population*)
  (salza2:compress-data (flexi-streams:string-to-octets
			 (format nil "~s"
				 (marshal:marshal (mapcar #'extract-agents-from-pool population))))
			'salza2:zlib-compressor))

(defun compress-object (object)
  "Compresses `object`, which can be any lisp data structure."
  ;; (compress-object *rules-config*)
  (salza2:compress-data (flexi-streams:string-to-octets
			 (format nil "~s" (marshal:marshal object)))
			'salza2:zlib-compressor))

(defun decompress-object (compressed-object)
  "Decompresses an object represented by `compressed-object`."
  ;; (decompress-object (compress-object *rules-config*))
  (ms:unmarshal
   (read-from-string
    (flexi-streams:octets-to-string
     (zlib:uncompress compressed-object)))))

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (with-postgres-connection
      (execute (create-table (:populations :if-not-exists t)
                   ((id :type '(:varchar 36)
                        :primary-key t)
                    (label :type '(:varchar 128))
                    (parent-id :type '(:varchar 36)
                               :not-null t)
                    (population :type 'bytea
                                :not-null t)
                    (best-index :type 'integer
                                :not-null t)
                    (instrument :type '(:varchar 128)
                                :not-null t)
                    (timeframe :type '(:varchar 128)
                               :not-null t)
                    (generations :type 'integer
                                 :not-null t)
                    (fitness-fn :type '(:varchar 128)
                                :not-null t)
                    (rules-config :type 'bytea
                                  :not-null t)
                    (begin :type 'bigint
                           :not-null t)
                    (end :type 'bigint
                         :not-null t)
                    (creation-time :type 'bigint
                                   :not-null t)
                    (mape :type '(:numeric)
                          :not-null t)
		    (mase :type '(:numeric)
                          :not-null t)
                    (pmape :type '(:numeric)
                           :not-null t)
                    (mae :type '(:numeric)
                         :not-null t)
                    (mse :type '(:numeric)
                         :not-null t)
                    (rmse :type '(:numeric)
                          :not-null t)
		    (recall :type '(:numeric)
			    :not-null t)
		    (precision :type '(:numeric)
			       :not-null t)
		    (f1-score :type '(:numeric)
			      :not-null t)
                    (accuracy :type '(:numeric[2])
                              :not-null t)
                    (revenue :type '(:numeric)
                             :not-null t)
                    )))
    (execute (create-table (:populations-closure :if-not-exists t)
                 ((parent-id :type '(:varchar 36))
                  (child-id :type '(:varchar 36))
                  (depth :type 'integer
                         :not-null t))
               (primary-key '(:parent-id :child-id))))
    (execute (create-table (:tests :if-not-exists t)
                 ((population-id :type '(:varchar 36)
                                 :not-null t)
                  (instrument :type '(:varchar 128)
                              :not-null t)
                  (timeframe :type '(:varchar 128)
                             :not-null t)
                  (begin :type 'bigint
                         :not-null t)
                  (end :type 'bigint
                       :not-null t)
		  (creation-time :type 'bigint
				 :not-null t)
                  (mape :type '(:numeric)
                        :not-null t)
		  (mase :type '(:numeric)
                        :not-null t)
                  (pmape :type '(:numeric)
                         :not-null t)
                  (mae :type '(:numeric)
                       :not-null t)
                  (mse :type '(:numeric)
                       :not-null t)
                  (rmse :type '(:numeric)
                        :not-null t)
		  (recall :type '(:numeric)
			  :not-null t)
		  (precision :type '(:numeric)
			     :not-null t)
		  (f1-score :type '(:numeric)
			    :not-null t)
                  (accuracy :type '(:numeric[2])
                            :not-null t)
                  (revenue :type '(:numeric)
                           :not-null t)
                  (decision :type '(varchar 128)
                            :not-null t)
                  (delta :type '(:numeric)
                         :not-null t)
                  )))))
;; (init-database)

;; (local-time:timestamp-to-unix (local-time:now))
;; (local-time:unix-to-timestamp (local-time:timestamp-to-unix (local-time:now)))

(defun get-population (population-id)
  (with-postgres-connection
      (retrieve-one (select :*
		      (from :populations)
		      (where (:= :id population-id))))))

(defun init-from-database (population-id)
  "Retrieves a population from the Overmind Agents database and initializes the
agents parameters according to it."
  ;; (init-from-database "EC553212-95D0-4549-A57E-2C3D2AE3CC31")
  (let* ((retrieved-pop (get-population population-id))
	 (pop (decompress-object
	       (access:access retrieved-pop :population)))
	 unique-agents)
    ;; Setting `*rates*` and its parameters.
    ;; (setf *begin* (position (access retrieved-pop :begin)
    ;; 			    *all-rates* :key (lambda (rate)
    ;; 					       (read-from-string (access rate :time)))))
    ;; (setf *end* (1+ (position (access retrieved-pop :end)
    ;; 			      *all-rates* :key (lambda (rate)
    ;; 						 (read-from-string (access rate :time))))))
    ;; (setf *rates* (subseq *all-rates* *begin* *end*))
    ;; Setting `*timeframe*` and `*instrument*`.
    ;; (setf *timeframe* (read-from-string (access retrieved-pop :timeframe)))
    ;; (setf *instrument* (read-from-string (access retrieved-pop :instrument)))
    ;; Resetting `*cached-agents*`, as these most likely are useless now.
    ;; (setf *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t))
    (setf *generations* (access:access retrieved-pop :generations))
    ;; (let ((fit-fn (read-from-string (access:access retrieved-pop :fitness-fn))))
    ;;   (cond ((eq fit-fn :mse)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :mse))))
    ;;         ((eq fit-fn :mae)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :mae))))
    ;;         ((eq fit-fn :mape)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :mape))))
    ;; 	    ((eq fit-fn :pmape)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :pmape))))
    ;;         ((eq fit-fn :rmse)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :rmse))))
    ;;         ((eq fit-fn :accuracy)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :accuracy))))
    ;;         ((eq fit-fn :revenue)
    ;;          (setf *fitnesses* (list (access:access retrieved-pop :revenue))))
    ;;         (t ;; default
    ;;          (setf *fitnesses* nil))))
    ;; Finding out what are the unique agents, as agents can be
    ;; repeated in a population's communities.
    (mapcar (lambda (elt)
	      (pushnew elt unique-agents :test #'equal))
	    (flatten pop))
    ;; Setting parameters according to what is set in the stored population.
    (setf *community-size* (length (first pop)))
    (setf *population-size* (length pop))
    (setf *num-inputs* (/ (length (first (slot-value (first (first pop)) 'rules))) 2))
    (setf *num-rules* (length (rules (first (first pop)))))
    ;; Checking if we need to set `*agents-pool*` with filling or not.
    (if (> (length unique-agents) *num-pool-agents*)
    	(progn
    	  (setf *num-pool-agents* (length unique-agents))
    	  (setf *agents-pool* unique-agents))
    	(setf *agents-pool* (append unique-agents
    				    (gen-agents (- *num-pool-agents*
    						   (length unique-agents))))))
    ;; `*population*` needs to be a list of lists of indexes.
    (setf *population*
	  (mapcar (lambda (community)
		    (mapcar (lambda (agent)
			      (position agent unique-agents :test #'equal))
			    community))
		  pop))))

(defun get-descendants (population-id)
  (with-postgres-connection
      (retrieve-all (select :p.*
                      (from (:as :populations :p))
                      (join (:as :populations-closure :c) :on (:= :p.id :c.child-id))
                      (where (:= :c.parent-id population-id))
                      (where (:> :c.depth 0))
                      ))))

(defun get-ancestors (population-id)
  ;; (get-ancestors 3)
  (with-postgres-connection
      (retrieve-all (select :p.*
                      (from (:as :populations :p))
                      (join (:as :populations-closure :c) :on (:= :p.id :c.parent-id))
                      (where (:= :c.child-id population-id))
                      (where (:> :c.depth 0))
                      ))))

(defun insert-initial-closure (population-id)
  (with-postgres-connection
      (execute (insert-into :populations-closure
                 (set= :parent-id population-id
                       :child-id population-id
                       :depth 0)))))
;; (insert-initial-closure 4)

(defun insert-closure (parent-id child-id)
  (with-postgres-connection
      (execute (insert-into :populations-closure
                 (:parent-id :child-id :depth)
                 (select (:p.parent-id :c.child-id (:+ :p.depth :c.depth 1))
                   (from (:as :populations-closure :p)
                         (:as :populations-closure :c))
                   (where (:= :p.child-id parent-id))
                   (where (:= :c.parent-id child-id)))))))
;; (insert-closure 3 4)

;; check sanitization

;; (marshal:marshal (extract-agents-from-pool (agents-best (agents-distribution *population*))))

(defun pmape (sim real &optional (zero-metric-constraint nil))
  "Penalized MAPE. If a simulation has a different direction than that of the real price, it gets penalized with a weight."
  (let ((real (subseq real *delta-gap*))
	(trades (remove nil
			(mapcar (lambda (s r)
				  (when (/= s 0)
				    (if (>= (* r s) 0)
					(abs (/ (- r s) (1+ (abs r))))
					(* 3
					   (abs (/ (- r s) (1+ (abs r))))))))
				sim
				real))))
    (if trades
	(/ (reduce #'+ trades)
	   (length real))
	cl:most-positive-fixnum)))

(defun mase (sim real &optional (zero-metric-constraint nil))
  "Mean absolute scaled error between a simulated time series `sim` and a real time series `real`."
  (let* ((real (subseq real *delta-gap*))
	 (numerators (mapcar (lambda (s r)
			       (if (/= s 0)
				   (abs (- r s))))
			     sim
			     real))
	 (denominators (remove nil
			       (mapcar (lambda (next current num)
					 (if num
					     (abs (- next current))))
				       (rest real)
				       real
				       numerators))))
    (if denominators
	(/ (mean (remove nil numerators))
	   (mean (remove nil denominators)))
	cl:most-positive-fixnum)
    ))

(defun mape (sim real &optional (zero-metric-constraint nil))
  "Mean absolute percentage error between a simulated time series `sim` and a real time series `real`.
This version scales the simulation."
  (let ((real (subseq real *delta-gap*))
	(trades (remove nil
			(mapcar (lambda (s r)
				  (if (/= s 0)
				      (/ (abs (- r s)) (1+ (abs r)))))
				sim
				real
				))))
    (if trades
	(/ (reduce #'+ trades)
	   (length trades))
	cl:most-positive-fixnum)))

(defun mse (sim real)
  "Mean squared error between a simulated time series `sim` and a real time series `real`."
  (let ((real (subseq real *delta-gap*))
	(trades (mapcar (lambda (elt) (expt elt 2))
			(remove nil
				(mapcar (lambda (s r)
					  (if (/= s 0)
					      (- s r)))
					sim real)))))
    (if trades
	(/ (reduce #'+ trades)
	   (length trades))
	cl:most-positive-fixnum)))

(defun mae (sim real)
  "Mean absolute error between a simulated time series `sim` and a real time
series `real`."
  (let ((real (subseq real *delta-gap*))
	(trades (remove nil
			(mapcar (lambda (s r)
				  (if (/= s 0)
				      (abs (- s r))))
				sim
				real))))
    (if trades
	(/ (reduce #'+ trades)
	   (length real))
	cl:most-positive-fixnum)))

(defun rmse (sim real)
  "Root mean square error between a simulated time series `sim` and a real time
series `real`."
  (let ((real (subseq real *delta-gap*)))
    (sqrt (mse sim real))))

(defun revenue (sim real)
  "Average revenue per trade."
  (let ((real (subseq real *delta-gap*)))
    (let* ((trades-count 0)
	   (trades-sum (apply #'+
			      (remove nil
				      (mapcar (lambda (s r)
						(when (/= s 0)
						  (incf trades-count)
						  (if (> (* s r) 0)
						      (abs r)
						      (- (abs r)))
						  ))
					      sim real)))))
      (if (> trades-count 0)
	  (/ trades-sum trades-count)
	  0))))

(defun accuracy (sim real &optional (mape-constraint nil))
  "How many trades were correct."
  ;; (accuracy (agents-indexes-simulation (agents-best (agents-distribution *population*))) (get-closes))
  (let* ((real (subseq real *delta-gap*))
         (trades-count 0)
	 (trades-sum (apply #'+
			    (mapcar (lambda (s r)
				      (when (/= s 0)
					(incf trades-count))
				      (if (> (* s r) 0)
					  1
					  0))
				    sim real))))
    (if (> trades-count 0)
	(make-array 2 :initial-contents `(,trades-sum ,trades-count))
    	#(0 0))
    ))

(defun recall (sim real)
  (let* ((real (subseq real *delta-gap*))
	 (true-positives 0)
	 (false-negatives 0))
    (map nil (lambda (s r)
	       (when (/= s 0)
		 ;; False negatives.
		 (when (and (< s 0) (> r 0))
		   (incf false-negatives))
		 ;; True positives.
		 (when (and (> s 0) (> r 0))
		   (incf true-positives))))
	 sim real)
    (if (or (/= true-positives 0) (/= false-negatives 0))
	(/ true-positives (+ true-positives false-negatives))
	0)))

(defun precision (sim real)
  (let* ((real (subseq real *delta-gap*))
	 (true-positives 0)
	 (false-positives 0))
    (map nil (lambda (s r)
	       (when (/= s 0)
		 ;; False positives.
		 (when (and (> s 0) (< r 0))
		   (incf false-positives))
		 ;; True positives.
		 (when (and (> s 0) (> r 0))
		   (incf true-positives))))
	 sim real)
    (if (or (/= true-positives 0) (/= false-positives 0))
	(/ true-positives (+ true-positives false-positives))
	0)))

(defun f1-score (sim real)
  (let ((rec (recall sim real))
	(prec (precision sim real)))
    (if (or (/= rec 0) (/= prec 0))
	(/ (* 2 rec prec)
	   (+ rec prec))
	0)))

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
		(mapcar (lambda (outer-val)
			  (cons outer-val
				inner-val))
			(car lists)))
	      (apply #'combinations (cdr lists)))))

(defun round-to (number precision &optional (what #'round))
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))

(defun gen-communities (size count)
  "How many communities (`count`) of agents of size `size`"
  ;; (gen-communities 3 5)
  (group size (alexandria:iota (* size count)))
  ;; (mapcar (lambda (_)
  ;; 	    (mapcar (lambda (_)
  ;; 		      (random-int *rand-gen* 0 (1- *num-pool-agents*)))
  ;; 		    (cl21:iota size)))
  ;; 	  (cl21:iota count))
  )

(defun group (n list)
  "Split LIST into a list of lists of length N."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (labels ((rec (src acc)
             (let ((rest (cl21:nthcdr src n)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list nil))))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun agents-best (distribution &optional (sort-fn #'<))
  (cadar (sort (copy-tree distribution) sort-fn :key (lambda (elt) (first elt)))))

(defun agents-worst (distribution &optional (sort-fn #'<))
  (cadar (reverse (sort (copy-tree distribution) sort-fn :key (lambda (elt) (first elt))))))

(defun agents-without-worst (distribution &optional (sort-fn-for-best #'<))
  ;; (agents-without-worst (agents-distribution *population*))
  (mapcar #'cadr (cdr (reverse (sort (copy-tree distribution) sort-fn-for-best :key (lambda (elt) (first elt)))))))

(defun all-positions (needle haystack)
  (loop
     for element in haystack
     and position from 0
     when (eql element needle)
     collect position))

(defun all-positions-not (needle haystack)
  (loop
     for element in haystack
     and position from 0
     when (not (eql element needle))
     collect position))

;; (length (agent-perception nil))

;; Summarizing data.
;; (maphash (lambda (k v)
;; 	   (print k)
;; 	   (print v))
;; 	 (first (get-data *instrument* *rates*)))

;; (defparameter *coco* (agent-perception (first *agents-pool*)))
;; (access (access (first *coco*) :heat) :z)
;; (agent-perception (first *agents-pool*))
;; (length
;;  (remove-duplicates
;;   (mapcar (lambda (perc)
;; 		    (subseq perc 0 5))
;; 		  *coco*)
;;   :test #'equal))

(defun add-rules-deltas (rules rules-deltas)
  (let ((idx 0))
    (mapcar (lambda (rule)
	      (mapcar (lambda (clause)
			(prog1
			    (list (+ (first clause) (nth idx rules-deltas))
				      (+ (second clause) (nth idx rules-deltas))
				      (third clause))
			  (incf idx)))
		      rule))
	    rules)))

;; (defparameter *coco* (gen-rules 2))
;; (add-rules-deltas *coco* (iota 300 :start 2 :step 0))

(defun agent-trades (agent)
  "Creates a simulation of a single agent's trades."
  ;; (time (length (agent-trades (make-instance 'agent))))
  (let ((sig (reduce #'+
		     (append (flatten (slot-value agent 'beliefs))
			     (flatten (slot-value agent 'rules))
			     (flatten (slot-value agent 'activations))
			     (get-deltas 10)
                             ))))
    (if (gethash sig *cached-agents*)
	(gethash sig *cached-agents*)
	(let* ((data (funcall *perception-fn* agent)))
	  (let ((sim (ifis-agents data
                                  (slot-value agent 'rules)
				  (slot-value agent 'activations)
				  (slot-value agent 'activation-threshold)
                                  ;; (slot-value agent 'input-min)
				  ;; (slot-value agent 'input-max)
				  ;; (slot-value agent 'output-min)
				  ;; (slot-value agent 'output-max)
				  )))
	    (setf (gethash sig *cached-agents*) sim)
	    sim
	    )))))

(defun agents-fitness (agents-indexes &optional (fitness-fn #'mase))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    ;; (mse sim (get-closes 3))
    (funcall fitness-fn sim (get-deltas (length sim)))))

(defun eval-rules (inp mx-inp agent-ifss rules-group)
  (if-coa
   (reduce #'ifintersection
	   (mapcar (lambda (i)
		     (let ((ifs (nth (+ (* *num-inputs* rules-group) i) agent-ifss)))
		       (reduce #'ifunion
			       (mapcar (lambda (j)
					 (rule
					  (* 100 (/ (nth i inp) (+ 1 mx-inp)))
					  (nth (* j 2) ifs)
					  (nth (1+ (* j 2)) ifs)))
				       (iota (floor (/ (length ifs) 2)))))))
		   (iota (length (butlast inp)))))))

;; (defparameter *acts* nil)
;; (mean *acts*)
;; (standard-deviation *acts*)

(defun ifis-agents (inputs rules activations activation-threshold)
  "This one considers an activation function."
  (let* ((agent-ifss (mapcar (lambda (params)
			       (mapcar (lambda (par)
					 (agents-ifs par))
				       params))
			     rules))
	 (activations
	  (mapcar (lambda (inp)
		    (flatten
		     (mapcar (lambda (j)
			       (mapcar (lambda (i)
                                         (if-membership (nth i inp)
							(agents-ifs (nth i (nth j activations))))
                                         )
				       (iota *num-inputs*)))
			     (iota *num-rules*)
			     )))
		  inputs)))
    (mapcar (lambda (j inp)
    	      (let ((activation-weight (mean
    					(mapcar (lambda (act thresh)
    						  (if (>= act thresh)
    						      act
    						      (- act)))
    						(nth j activations)
    						activation-threshold))))
    		(* activation-weight
    		   (mean (mapcar (lambda (i)
    				   (let ((ifs (nth i agent-ifss)))
    				     (if-coa
    				      (reduce #'ifunion
    					      (mapcar (lambda (j)
    							(rule
    							 (nth j inp)
    							 (nth (* j 2) ifs)
    							 (nth (1+ (* j 2)) ifs)))
    						      (iota (floor (/ (length ifs) 2)))
    						      )))))
    				 (iota (length rules))
    				 )))))
    	    (iota (length inputs))
    	    inputs)
    
    ;; (mapcar (lambda (j inp)
    ;; 	      (if ;; (>= (nth j reductions) activation-threshold)
    ;; 	       t
    ;; 	       ;; (every (lambda (elt) (not (null elt)))
    ;; 	       ;; 	      (mapcar (lambda (act thresh)
    ;; 	       ;; 			(>= act
    ;; 	       ;; 			    thresh
    ;; 	       ;; 			    ))
    ;; 	       ;; 		      (nth j activations)
    ;; 	       ;; 		      activation-threshold
    ;; 	       ;; 		      ))
    ;; 	       ;; This one doesn't use *num-rules* == 1.
    ;; 	       ;; (if-coa
    ;; 	       ;; 	(reduce #'ifunion
    ;; 	       ;; 		(mapcar (lambda (i)
    ;; 	       ;; 			  (let ((ifs (nth i agent-ifss)))
    ;; 	       ;; 			    (reduce #'ifunion
    ;; 	       ;; 				    (mapcar (lambda (j)
    ;; 	       ;; 					      (rule
    ;; 	       ;; 					       (nth j inp)
    ;; 	       ;; 					       (nth (* j 2) ifs)
    ;; 	       ;; 					       (nth (1+ (* j 2)) ifs)))
    ;; 	       ;; 					    (iota (floor (/ (length ifs) 2)))
    ;; 	       ;; 					    ))))
    ;; 	       ;; 			(iota (length rules))
    ;; 	       ;; 			)))
    ;; 	       (mean (mapcar (lambda (i)
    ;; 			       (let ((ifs (nth i agent-ifss)))
    ;; 				 (if-coa
    ;; 				  (reduce #'ifunion
    ;; 					  (mapcar (lambda (j)
    ;; 						    (rule
    ;; 						     (nth j inp)
    ;; 						     (nth (* j 2) ifs)
    ;; 						     (nth (1+ (* j 2)) ifs)))
    ;; 						  (iota (floor (/ (length ifs) 2)))
    ;; 						  )))))
    ;; 			     (iota (length rules))
    ;; 			     ))
    ;; 	       0))
    ;; 	    (iota (length inputs))
    ;; 	    inputs)
    ))
;; (gen-rules 2)

(defun agents-distribution (population &optional (fitness-fn #'mase) (sort-fn #'<))
  ;; (agents-distribution *population*)
  (let* ((fitnesses (pmapcar (lambda (agents)
                               (agents-fitness agents fitness-fn))
			     population
                             ;; (mapcar (lambda (indexes)
                             ;;           (mapcar (lambda (index)
                             ;;                     (nth index *agents-pool*))
                             ;;                   indexes))
                             ;;         population)
			     ))
	 (sum (apply #'+ fitnesses)))
    (values
     (mapcar (lambda (fitness x)
	       (cons (/ fitness sum) (list x))
	       )
	     fitnesses
	     population)
     (first (sort (copy-seq fitnesses) sort-fn)))))

(defun agents-reproduce (&optional (fitness-fn #'mase) (sort-fn #'<))
  (agents-mutate)
  ;; Finding appropriate leverages.
  ;; (dolist (community *population*)
  ;;   (woof community))
  ;; (when (> *generations* 300)
  ;;   (dolist (community *population*)
  ;;     (woof community)))
  ;; Reset all agents' leverages to 1.
  ;; (dolist (agent *agents-pool*)
  ;;     (setf (slot-value agent 'leverage)
  ;; 	    (iota *num-inputs* :start 1 :step 0)))

  (let ((x (agents-selectone (agents-distribution *population* fitness-fn sort-fn)))
	(y (agents-selectone (agents-distribution *population* fitness-fn sort-fn))))
    ;; If we selected the same individual, we don't want to use them for crossover.
    (unless (equal x y)
      (multiple-value-bind (newx newy)
	  (agents-crossover x y)
	;; (setf *population* (append (list newx) (agents-without-worst (agents-distribution *population* fitness-fn sort-fn) sort-fn)))

	(pushnew newx *population* :test #'equal)
	(when (/= (length *population*) *population-size*)
	  (setf *population* (agents-without-worst (agents-distribution *population* fitness-fn sort-fn))))
	(pushnew newy *population* :test #'equal)
	(when (/= (length *population*) *population-size*)
	  (setf *population* (agents-without-worst (agents-distribution *population* fitness-fn sort-fn))))
          
	;; (setf *population* (append (list newy) (agents-without-worst (agents-distribution *population* fitness-fn sort-fn))))
	)))

  ;; Finding appropriate leverages.
  ;; (dolist (community *population*)
  ;;     (woof community))
  ;; (when (> *generations* 300)
  ;;   (dolist (community *population*)
  ;;     (woof community)))

  (multiple-value-bind (_ f) (agents-distribution *population* fitness-fn sort-fn)
    f))

(defun avg (lst)
  (/ (reduce #'+ lst) (length lst)))

(defun write-to-file (filename content)
  (with-open-file (str filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str content)))

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'combinations (cdr lists)))))

;; (float (accesses (agents-test
;;                   (mapcar (lambda (idx)
;;                             (nth idx *test-agents*))
;;                           '(42 99))
;;                   (get-rates-range *instrument* *timeframe*
;;                                    (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 512 :day))
;;                                    (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 188 :day))))
;;                  :performance-metrics :accuracy))

;; (defparameter *test-agents* (gen-agents 200))
;; (setf *agents-pool* *test-agents*)
;; (setf *population* (list (first (first (agents-err-adjust *test-agents* (- omper:*data-count* 3))))))
;; (agents-err-adjust *test-agents* 60)
;; (let ((sim (agents-simulation (mapcar (lambda (idx)
;;                                         (nth idx *test-agents*))
;;                                       '(67 74)))))
;;   (float (accuracy sim (get-closes (length sim)))))

;; (map nil (lambda (n)
;;            (print (second (first (agents-err-adjust *test-agents* n)))))
;;      (alexandria:iota 10))

;; (agents-mf-adjust (extract-agents-from-pool (first *population*)) 10000)
;; *cached-agents*

(defun largest-number-indexes (list)
  (mapcar #'second
          (stable-sort
	   (loop
              for index from 0
              for element in list
              collect (list element index))
	   #'>
	   :key #'first)))

(defun largest-number-index (list)
  (first (largest-number-indexes list)))

(defun gen-beliefs ()
  ;; (gen-beliefs)
  (let (;; (options '(0.236 0.382 0.5 0.618 1 1.618))
	(options (iota 10)))
    (let ((nums (mapcar (lambda (fibo)
			  (round-to (float (/ (random-int *rand-gen* 0 200) 100)) 3)
			  )
			(cl21:take (1+ (random-int *rand-gen* 1 (1- (length options))))
				   (shuffle options)))))
      (remove nil
	      (shuffle (concatenate 'list (make-list (- (length options) (length nums))) nums))))))

(defun gen-activations (max-num-rules)
  ;; (gen-rules 1)
  (let* ((percs (flatten (mapcar #'butlast (funcall *perception-fn* nil))))
	 (reals (get-deltas omper:*data-count*))
	 (mn-perc (apply #'min percs))
	 (mx-perc (apply #'max percs))
	 (mn-real (apply #'min reals))
	 (mx-real (apply #'max reals))
	 (inp-sd (/ (/ (- mx-perc mn-perc) max-num-rules) (* 2 2.355)))
	 (out-sd (/ (/ (- mx-real mn-real) max-num-rules) (* 2 2.355))))
    (mapcar (lambda (_)
	      (apply #'nconc
		     (mapcar (lambda (i) `((,(random-float *rand-gen* mn-perc mx-perc)
					     ,(random-float *rand-gen* mn-perc mx-perc)
					     ,(random-float *rand-gen* 0 1)
					     ,inp-sd
					     ,mn-perc
					     ,mx-perc
					     )
					   (,(random-float *rand-gen* mn-real mx-real)
					     ,(random-float *rand-gen* mn-real mx-real)
					     ,(random-float *rand-gen* 0 1)
					     ,out-sd
					     ,mn-real
					     ,mx-real
					     )
					   ))
			     (cl21:iota max-num-rules))))
	    (cl21:iota *num-inputs*))))

(defun get-idxs-same-direction (nums count delta-gap)
  (let ((pred (if (> (random-float *rand-gen* 0 1) 0.5) #'plusp #'minusp))
	(idxs nil))
    (loop
       for idx from 0 to (length nums)
       for num in nums
       do (when (and (funcall pred num) (>= idx delta-gap)) (push idx idxs)))
    (subseq (shuffle idxs) 0 count)
    ))
;; (get-idxs-same-direction '(1 -1 3 -4 -5 10 10 10) 2 2)

(defun gen-rules (num-rules)
  "This one uses *num-rules* == N and each MF uses an input as its mean."
  ;; (gen-rules 3)
  (let* ((inputs (mapcar #'butlast (funcall *perception-fn* nil)))
	 (outputs (get-deltas omper:*data-count*))
	 ;; (chosen-idxs (loop repeat num-rules
	 ;; 		 collect (random-int *rand-gen* 0 (- (length outputs) *delta-gap*))))
	 (chosen-idxs (get-idxs-same-direction outputs num-rules *delta-gap*))
	 ;; (chosen-idxs (try-until-successful
	 ;; 	       (let* ((idxs (loop repeat num-rules
	 ;; 			       collect (random-int *rand-gen* 0 (- (length outputs) *delta-gap*))))
	 ;; 		      (outs (loop for i in idxs collect (nth (+ i *delta-gap*) outputs)))
	 ;; 		      (wanted-direction (first outs)))
	 ;; 		 (when (not (every (lambda (elt) (> (* elt wanted-direction) 0)) outs))
	 ;; 		   (error "not all outputs point to same direction"))
	 ;; 		 idxs)))
	 ;; (chosen-inputs (loop for i in chosen-idxs collect (nth i inputs)))
	 ;; (chosen-outputs (loop for i in chosen-idxs collect (nth (+ i *delta-gap*) outputs)))
	 (chosen-inputs (loop for i in chosen-idxs collect (nth (- i *delta-gap*) inputs)))
	 (chosen-outputs (loop for i in chosen-idxs collect (nth i outputs)))
	 (inp-sd (mapcar (lambda (inp) (/ (standard-deviation inp) 1)) (apply #'mapcar #'list chosen-inputs)))
	 (out-sd (/ (standard-deviation chosen-outputs) 1))
	 (mn-inp (- (apply #'min (flatten chosen-inputs)) (apply #'max inp-sd)))
	 (mx-inp (+ (apply #'max (flatten chosen-inputs)) (apply #'max inp-sd)))
	 ;; (mn-out (- (apply #'min chosen-outputs) out-sd))
	 ;; (mx-out (+ (apply #'max chosen-outputs) out-sd))
	 (mn-out (let ((min-chosen (apply #'min chosen-outputs))) (if (and (> min-chosen 0) (< (- min-chosen out-sd) 0)) 0.0 (- min-chosen out-sd))))
	 (mx-out (let ((max-chosen (apply #'max chosen-outputs))) (if (and (< max-chosen 0) (> (+ max-chosen out-sd) 0)) 0.0 (+ max-chosen out-sd))))
	 )
    (mapcar (lambda (j)
	      (apply #'nconc
		     (mapcar (lambda (i)
			       (let ((input (nth j chosen-inputs))
				     (output (nth j chosen-outputs)))
				 (list
				  (list (nth i input)
					(nth i input)
					;; (random-float *rand-gen* 0 0)
                                        1
					(nth i inp-sd)
					mn-inp
					mx-inp
					)
				  (list output
					output
					;; (random-float *rand-gen* 0 0)
                                        1
					out-sd
					mn-out
					mx-out
					))))
			     (iota *num-inputs*)
			     ;; (iota (length chosen-outputs))
			     )))
	    (iota (length chosen-outputs))
	    )))
;; (loop repeat 10 do (gen-rules 3))

(defun n-into-m-parts (m n)
  (mapcar #'+
	  (print (let ((lst (sort (append (mapcar (lambda (_)
					     (random-float *rand-gen* 0 (* 10 m)))
					   (iota (1- n)))
				   `(0 ,(* 10 m)))
			   #'<)))
	    (mapcar #'-
		    (rest lst) lst)))

	  (print (let ((lst (sort (append (mapcar (lambda (_)
					     (random-float *rand-gen* (- (* 9 m)) 0))
					   (iota (1- n)))
				   `(,(- (* 9 m)) 0))
			   #'<)))
	    (mapcar (lambda (n1 n2) (- (- n1 n2)))
		    (rest lst) lst)))))

(defun n-into-m-parts (n m)
  (let* ((rand-nums (mapcar (lambda (_)
			      (random-float *rand-gen* -1 1))
			    (iota m)))
	 (sum (reduce #'+ rand-nums)))
    (mapcar (lambda (num)
	      (* (/ num sum) n))
	    rand-nums)))

(defun gen-agents (num)
  (mapcar (lambda (_)
	    (let ((agent (make-instance 'agent)))
	      (let* ((activations (mapcar (lambda (inp)
					    (mapcar (lambda (i)
						      (second
						       (if-membership (nth i inp)
								   (agents-ifs (nth i (slot-value agent 'activations))))))
						    (iota *num-inputs*)))
					  (funcall *perception-fn* nil)))
		     (reductions (mapcar (lambda (act) (reduce #'+ act)) activations))
		     (best-activation (nth (largest-number-index reductions) activations))
		     (activation-threshold (nth 5 (sort (mapcar (lambda (act) (mase act best-activation)) activations) #'<)))
		     )
		(setf (slot-value agent 'activation-threshold)
		      activation-threshold)
		(setf (slot-value agent 'activation-best)
		      best-activation))
	      agent))
	  (iota num)))

(defun gen-agents (num)
  "This one uses the agent input MFs as its activations."
  (mapcar (lambda (_)
	    (let ((agent (make-instance 'agent)))
	      ;; (setf (activations agent) (mapcar #'first (rules agent)))
	      (setf (activations agent) (mapcar (lambda (rule)
						  (remove nil
							  (mapcar (lambda (i r)
								    (when (evenp i)
								      r))
								  (iota (length rule))
								  rule)))
						(rules agent)))
	      (let* ((reductions (mapcar (lambda (inp)
					    (reduce #'+
						    (mapcar (lambda (j)
							      (reduce #'+
								      (mapcar (lambda (i)
										(second
										 (if-membership (nth i inp)
											     (agents-ifs (nth i (nth j (activations agent)))))))
									      (iota *num-inputs*))))
							    ;; (funcall *perception-fn* nil)
							    (iota *num-rules*)
							    )))
					  ;; (iota *num-rules*)
					  (funcall *perception-fn* nil)))
		     ;; (reductions (mapcar (lambda (act) (reduce #'+ act)) (mapcar #'flatten (print activations))))
		     ;; (activation-threshold (nth (random-int *rand-gen* 0 *activation-level*) (sort reductions #'>)))
		     (sorted-reductions (sort reductions #'>))
		     ;; (activation-threshold (mean `(,(nth 1 sorted-reductions) ,(nth 1 sorted-reductions))))
		     (activation-threshold (nth *activation-level* sorted-reductions))
		     ;; (activation-threshold (nth 3 sorted-reductions))
		     )
		(setf (activation-threshold agent) activation-threshold)
		agent)))
	  (iota num)))

;; (length (funcall *perception-fn* nil))
;; (get-deltas omper:*data-count*)

(defun should-add-activation? (wanted-direction direction all-directions)
  (let ((all-directions (append (list direction) all-directions)))
    (if (null all-directions)
	t
	(let ((right-count (count t
				  (mapcar (lambda (dir)
					    (> (* wanted-direction dir) 0))
					  all-directions))))
	  (if (>= right-count (* (/ (length all-directions) 3) 2))
	      t)))))

(defun gen-agents (num)
  "This one uses a list of activations."
  (loop repeat num
     collect (try-until-successful
	      (let ((agent (make-instance 'agent)))
		;; (setf (activations agent) (mapcar #'first (rules agent)))
		(setf (activations agent) (mapcar (lambda (rule)
						    (remove nil
							    (mapcar (lambda (i r)
								      (when (evenp i)
									r))
								    (iota (length rule))
								    rule)))
						  (rules agent)))
		(let* ((activations (mapcar (lambda (inp)
					      (flatten
					       (mapcar (lambda (j)
							 (mapcar (lambda (i)
								   (if-membership (nth i inp)
										  (agents-ifs (nth i (nth j (activations agent)))))
								   )
								 (iota *num-inputs*)))
						       ;; (funcall *perception-fn* nil)
						       (iota *num-rules*)
						       )))
					    ;; (iota *num-rules*)
					    (subseq (funcall *perception-fn* nil) 0 (- omper:*data-count* *delta-gap*))))
		       (reductions (mapcar (lambda (act) (reduce #'+ act)) activations))
		       (sorted-reductions-idxs (largest-number-indexes reductions))
		       (outputs (get-deltas omper:*data-count*))
		       (wanted-direction (nth (+ (first sorted-reductions-idxs) *delta-gap*) outputs))
		       ;; (activation-threshold (apply #'mapcar (lambda (&rest acts)
		       ;; 					     (apply #'min acts))
		       ;; 				  (mapcar (lambda (idx)
		       ;; 					    (nth idx activations))
		       ;; 					  (subseq sorted-reductions-idxs 0 *activation-level*))))
		       (activation-threshold (apply #'mapcar (lambda (&rest acts)
		       					       (apply #'min acts))
		       				    (let ((result)
							  (directions))
		       				      (dolist (idx sorted-reductions-idxs)
		       					(let ((dir (nth (+ idx *delta-gap*) outputs)))
							  (if (should-add-activation? wanted-direction dir directions)
							   ;; (> (* dir wanted-direction) 0)
							      (progn
								(push dir directions)
								(push (nth idx activations) result))
							      (return))))
		       				      (when (< (length result) *required-activations*)
		       				      	(error "not enough activations"))
						      (setf (consecutive-activations agent) (length result))
		       				      result)))
		       )
		  (setf (activation-threshold agent) activation-threshold)
		  agent)))))

;; (time (gen-agents 1))

(defun calc-trade-scale ()
  (/ (mean (mapcar (lambda (next curr)
                     (abs (- (access next :close-bid)
                             (access curr :close-bid))))
                   (rest *rates*)
                   *rates*))
     (* *community-size* 100)))

(defun calc-leverage-mean ()
  (/ (mean (mapcar (lambda (next curr)
                     (abs (- (access next :close-bid)
                             (access curr :close-bid))))
                   (rest *rates*)
                   *rates*))
     (* *community-size* 100)))

(defun calc-leverage-stdev ()
  (/ (standard-deviation (mapcar (lambda (next curr)
                     (abs (- (access next :close-bid)
                             (access curr :close-bid))))
                   (rest *rates*)
                   *rates*))
     (* *community-size* 100)))
;; (calc-leverage-mean)
;; (calc-leverage-stdev)

(defun calc-stdev-deltas ()
  (standard-deviation (get-deltas omper:*data-count*)))
;; (calc-stdev-deltas)

(defun median-elt (sample)
  (nth (floor (/ (length sample) 2))
       (sort (copy-sequence 'list sample) #'<)))

(defun median-elt-idx (sample)
  (position (median-elt sample) sample))

;; (let ((seq '(4 5 1 2 3 6 7 8)))
;;   (median-elt-idx seq))

(defun csv-real-sim ()
  (map nil (lambda (real sim)
             (format t "~a, ~a~%" real sim))
       (get-deltas omper:*data-count*)
       (mapcar (lambda (rate)
                 (format nil "~f" rate))
               (agents-indexes-simulation (agents-best (agents-distribution *population*
                                                                            #'mase
                                                                            #'<)
                                                       #'<)))))

;; (csv-real-sim)

(defun cluster-trades-deltas (trades deltas agents-count)
  (let* (final-trades final-deltas)
      (let ((delta-clusters (mapcar #'flatten (km (mapcar #'list deltas) 2)))
	    (trade-clusters (make-hash-table :size 2)))

	(let* ((min-delta-clusters-size (min (length (nth 0 delta-clusters))
					     (length (nth 1 delta-clusters))))
	       (0-cluster-size (length (nth 0 delta-clusters)))
	       (1-cluster-size (length (nth 1 delta-clusters)))
	       (first-half-size (if (< min-delta-clusters-size (/ agents-count 2))
				    (if (< 0-cluster-size (/ agents-count 2))
					0-cluster-size
					(- agents-count min-delta-clusters-size))
				    (floor agents-count 2)))
	       (second-half-size (if (< min-delta-clusters-size (/ agents-count 2))
				     (if (< 1-cluster-size (/ agents-count 2))
					 1-cluster-size
					 (- agents-count first-half-size))
				     (+ first-half-size (rem agents-count 2)))))

	  ;;(format t "~a,~a~%" (length (nth 0 delta-clusters)) (length (nth 1 delta-clusters)))

	  ;; Clustering deltas and trades.
	  (dotimes (i (length deltas))
	    ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
	    (let ((num-cluster (position (nth i deltas) delta-clusters
					 :test (lambda (elt seq)
						 (find elt seq :test #'equal)))))
	      ;; We save the slice of trades (different trades by different agents)
	      ;; in its corresponding cluster.
	      (push (mapcar (lambda (trade)
			      (nth i trade))
			    trades)
		    (gethash num-cluster trade-clusters))))

	  ;; First half.
	  (let* ((r-trade-cluster (reverse (gethash 0 trade-clusters)))
		 (inner-trade-clusters (km r-trade-cluster first-half-size))
		 (inner-delta-clusters (make-hash-table :size first-half-size)))
	    ;; Clustering trades and deltas.
	    (dotimes (i (length r-trade-cluster))
	      ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
	      (let ((num-cluster (position (nth i r-trade-cluster) inner-trade-clusters
					   :test (lambda (elt seq)
						   (find elt seq :test #'equal)))))
		;; We save the slice of trades (different trades by different agents)
		;; in its corresponding cluster.
		(push (nth i (nth 0 delta-clusters))
		      (gethash num-cluster inner-delta-clusters))))
	    ;; Saving results.
	    ;; (format t "First half: ~a~%" inner-trade-clusters)
	    (push inner-trade-clusters final-trades)
	    (push (mapcar #'reverse (mapcar #'cdr (sort (copy-sequence 'list (hash-table-alist inner-delta-clusters)) #'< :key #'first)))
		  final-deltas)
	    )

	  ;; Second half.
	  (let* ((r-trade-cluster (reverse (gethash 1 trade-clusters)))
		 (inner-trade-clusters (km r-trade-cluster second-half-size))
		 (inner-delta-clusters (make-hash-table :size second-half-size)))
	    ;; Clustering trades and deltas.
	    (dotimes (i (length r-trade-cluster))
	      ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
	      (let ((num-cluster (position (nth i r-trade-cluster) inner-trade-clusters
					   :test (lambda (elt seq)
						   (find elt seq :test #'equal)))))
		;; We save the slice of trades (different trades by different agents)
		;; in its corresponding cluster.
		(push (nth i (nth 1 delta-clusters))
		      (gethash num-cluster inner-delta-clusters))))
	    ;; (format t "Second half: ~a~%" inner-trade-clusters)
	    (push inner-trade-clusters final-trades)
	    (push (mapcar #'reverse (mapcar #'cdr (sort (copy-sequence 'list (hash-table-alist inner-delta-clusters)) #'< :key #'first)))
		  final-deltas))

	  ;; (format t "~a~%" final-trades)
	  ;; (defparameter *coco* final-trades)

	  ;; (values (mapcar (lambda (cluster)
	  ;; 		    (apply #'mapcar (lambda (&rest trades)
	  ;; 				      (mean trades))
	  ;; 			   cluster))
	  ;; 		  (apply #'concatenate 'list (reverse final-trades)))
	  ;; 	  (mapcar #'mean (apply #'concatenate 'list (reverse final-deltas))))

	  ;; (format t "delta-clusters: ~a~%" delta-clusters)
	  ;; (format t "trade-clusers: ~a~%" (hash-table-values trade-clusters))
	  ;; (format t "r-final-deltas: ~a~%" (reverse final-deltas))
	  ;; (format t "r-final-trades: ~a~%" (reverse final-trades))
	  ;; (break "")
	  
	  (let* ((r-final-deltas (reverse final-deltas))
		 (r-final-trades (reverse final-trades))
		 ;; (idxs (mapcar #'median-elt-idx r-final-deltas))
		 )
	    (values (mapcar (lambda (cluster)
			      (apply #'mapcar (lambda (&rest trades)
						(mean trades)
						;; (nth idx trades)
						)
				     cluster))
			    ;; idxs
			    (apply #'concatenate 'list r-final-trades))
		    (mapcar #'mean (apply #'concatenate 'list r-final-deltas)))))
	)))
;; (cluster-trades-deltas *trades* *deltas* 10)

;; (dotimes (i (length *population*))
;;   (multiple-value-bind (res err)
;;       (woof
;;        (nth i *population*))
;;     (print err))
;;   )

;; (mapcar #'float (mapcar #'agents-pmape *population*))

;; All communities leverages.
;; (dolist (community *population*)
;;   (dolist (agent-idx community)
;;     (format t " ~a~%" (slot-value (extract-agent-from-pool agent-idx) 'leverage)))
;;   (terpri))

;; (dolist (community *population*)
;;   (woof (extract-agents-from-pool community))
;;   )

(defun f (v)
  (reduce '+ (map 'vector (lambda (i) (expt i 2)) v)))

(defun norm (x)
  (sqrt (reduce '+ (map 'vector (lambda (i) (expt i 2)) x))))

(defun nabla (f x v &optional (epsilon 1.0d-4))
  (let* ((dir (map 'vector (lambda (u) (* epsilon u)) v))
         (ter (map 'vector '+ x dir))
         (ini (map 'vector '- x dir)))
    (/ (- (funcall f ter) 
          (funcall f ini))
       (* 2 (norm dir)))))

(defun basis (i n)
  (map 'vector (lambda (j) (if (equal i j) 1 0)) (range n)))

(defun descent (community fun x &key (error 1.0d-5) 
                        (rate 1.0d-2) 
                        (max-steps 10000))
  (let ((len (length x))
	(best nil)
	(best-error 100))
    (do* ((y (copy-sequence 'vector x) (map 'vector '- y step))
          (step (map 'vector 
                     (lambda (i) (* rate (nabla fun y (basis i len)))) 
                     (iota len))
                (map 'vector 
                     (lambda (i) (* rate (nabla fun y (basis i len)))) 
                     (iota len)))
          (n 0 (1+ n)))
         ((or (< (agents-mase community nil) error) 
              (>= n max-steps))
	  ;; Changing leverages
	  ;; (let ((leverages (coerce (group *num-inputs* (coerce best 'list)) 'vector)))
	  ;;   (dotimes (i (length leverages))
	  ;;     (setf (slot-value (extract-agent-from-pool (nth i community)) 'leverage)
	  ;; 	    (aref leverages i))))
	  (dotimes (i (length best))
	    (setf (slot-value (extract-agent-from-pool (nth i community)) 'leverage)
		  (aref best i)))

          ;; (if (< n max-steps) y)
          ;; y
          )
      (let ((mase (agents-mase community nil)))
	(when (< mase best-error)
	  (setf best-error mase)
	  (setf best y))
	;; (format t "~a, ~a, ~a~%" mase (float (agents-accuracy *best* nil)) (validate-test *best* 0.1 :accuracy))
	;; (print mase)
	)
      )))

;; Reset leverages.
;; (dolist (agent *agents-pool*)
;;   (setf (slot-value agent 'leverage)
;; 	(iota *num-inputs* :start (calc-trade-scale) :step 0)
;; 	;; (iota *num-inputs* :start 1 :step 0)
;; 	))

;; Reset rules-deltas.
;; (dolist (agent *agents-pool*)
;;   (setf (slot-value agent 'rules-deltas)
;; 	(make-list (* 2 *num-rules* *num-inputs*) :initial-element 0)))

;; (defparameter *best* (agents-best (agents-distribution *population* #'accuracy #'>) #'>))
;; (defparameter *best* (agents-best (agents-distribution *population* #'mase)))
(defun descent-leverages (community error rate max-steps)
  (descent community
	   (lambda (leverages)
	     (fit community leverages))
	   (flatten
	    (mapcar (lambda (agent)
		      (slot-value agent 'leverage))
		    (extract-agents-from-pool community)))
	   :error error
	   ;; :rate (calc-trade-scale)
	   :rate rate
	   :max-steps max-steps))
;; (dotimes (i 20) (descent-leverages *best* 6.0d-50 1d-5 5))
;; (float (agents-accuracy *best* nil))
;; (validate-test *best* 1.0 :accuracy)

;; 0.905592098739329d0, 0.6025, (0.575 0.5)
;; 0.910618936775545d0, 0.5675, (0.525 0.4)

;; Init using last entry (generations)
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :mase)))))) :id))

;; Init using agents with most accuracy
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :accuracy)))))) :id))

;; Print metrics.
;; (csv-real-sim)

;; Print current leverages
;; (dolist (agent *best*) (print (slot-value (extract-agent-from-pool agent) 'leverage)))

(defun validate-test (community ratio metric-kw)
  (let ((omper:*data-count* (ceiling (* omper:*data-count* ratio))))
    (list
     (float
      (accesses
       (agents-test (extract-agents-from-pool
		     community)
		    (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
       :performance-metrics metric-kw))
     (float
      (accesses
       (agents-test (extract-agents-from-pool
		     community)
		    (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*))))
       :performance-metrics metric-kw)))))
;; (validate-test )

;; (float 68/125)

(defun fit (agents-indexes leverages)
  (let* ((agents (extract-agents-from-pool
                  agents-indexes))
	 ;; (leverages (coerce (group *num-inputs* (coerce leverages 'list)) 'vector))
	 )
    ;; Changing leverages
    (dotimes (i (length leverages))
      (setf (slot-value (nth i agents) 'leverage)
            (aref leverages i)))
    (let ((mase (agents-mase agents-indexes t)))
      mase)))

(defun report-all-best-database ()
  (remove nil
	  (mapcar (lambda (id)
		    (let ((db-pop (get-best-descendant (access id :id))))
		      (init-from-database (access db-pop :id))

		      (let* ((omper:*data-count* (ceiling (* omper:*data-count* *testing-ratio*))))

			(let* ((validation (market-report db-pop
							  *instrument* *timeframe*
							  (subseq *all-rates* *begin* (+ *end* omper:*data-count*))))
			       (test (market-report db-pop
						    *instrument* *timeframe*
						    (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*)))))
			       (result `((:train . ((:mase . ,(float (accesses test :training :performance-metrics :mase)))
						    (:accuracy . ,(accesses test :training :performance-metrics :accuracy))))
					 (:validation . ((:mase . ,(float (accesses validation :testing :performance-metrics :mase)))
							 (:accuracy . ,(accesses validation :testing :performance-metrics :accuracy))))
					 (:test . ((:mase . ,(float (accesses test :testing :performance-metrics :mase)))
						   (:accuracy . ,(accesses test :testing :performance-metrics :accuracy)))
						))))
			  result)
	      
			)))
		  (get-root-populations-ids))))

(defun get-root-populations-ids ()
  (with-postgres-connection
      (retrieve-all (select (:id)
		      (from :populations)
		      (where (:= :parent-id ""))
		      ))))

;; (defun get-best-descendant (id &optional (fitness-kw :mase))
;;   (let* ((desc (get-descendants id))
;; 	 (idx (last-elt (largest-number-indexes (mapcar (lambda (entry)
;; 							(access entry fitness-kw))
;; 						      desc)))))
;;     (nth idx desc)))

(defun get-best-descendant (population-id)
  (with-postgres-connection
      (retrieve-one (select :p.*
                      (from (:as :populations :p))
                      (join (:as :populations-closure :c) :on (:= :p.id :c.child-id))
                      (where (:= :c.parent-id population-id))
                      (where (:> :c.depth 0))
		      (order-by (:asc :mase))
                      ))))

(defun get-best-descendant-id (id &optional (fitness-kw :mase))
  (let* ((desc (get-descendants id))
	 (idx (last-elt (largest-number-indexes (mapcar (lambda (entry)
							(access entry fitness-kw))
						      desc)))))
    (access (nth idx desc) :id)))

;; (defmacro param-singleton (sym value)
;;   (print (find-symbol (symbol-name sym)))
;;   (unless (find-symbol (symbol-name sym) :overmind-agents)
;;     `(defparameter ,sym ,value)))

;; (agents-accuracy (agents-best (agents-distribution *population* #'accuracy #'>) #'>))
;; (dolist (pop *population*)
;;   (print (float (agents-accuracy pop))))

(defun get-data-sample (validation-ratio testing-ratio)
  (let* ((*end* (ceiling (+ *end* (* omper:*data-count* (+ validation-ratio testing-ratio)))))
	 (omper:*data-count* (ceiling (* omper:*data-count* (+ 1 validation-ratio testing-ratio)))))
    (mapcar #'butlast (funcall *perception-fn* (extract-agent-from-pool 1)))))

(defun remove-nth (idx list)
  (append (subseq list 0 idx)
	  (subseq list (1+ idx))))

(defun remove-nths (idxs list)
  (let (res)
    (dotimes (i (length list))
      (unless (find i idxs)
	(push (nth i list) res)))
    (nreverse res)))

(defun scale-sim (lst from to)
  ;; (let ((mx (apply #'max lst))
  ;; 	(mn (apply #'min lst)))
  ;;   (mapcar (lambda (x)
  ;; 	      (+ (* from (- 1 (/ (- x mn) (- mx mn))))
  ;; 		 (* to (/ (- x mn) (- mx mn)))))
  ;; 	    lst))
  lst
  )

(defun km-positions (observations k &key (key #'identity) (test #'equal))
  (let* ((keyed (mapcar key observations))
	 (clusters (km keyed k)))
    (mapcar (lambda (cluster)
	      (mapcar (lambda (elt)
			(position elt observations :test test :key key))
		      cluster))
	    clusters)))

;; (km-positions '((1 2 3 "one")
;; 		(10 20 30 "ten")
;; 		(10 20 31 "eleven")
;; 		(2 3 4 "two")
;; 		(11 21 31 "twenty"))
;; 	      2
;; 	      :key #'butlast)

(defun format-accuracy (accuracy)
  (format nil "~a/~a (~$%)"
	  (aref accuracy 0)
	  (aref accuracy 1)
	  (if (= (aref accuracy 1) 0)
	      0.0
	      (* 100
		 (/ (aref accuracy 0)
		    (aref accuracy 1))))))


;; (ql:quickload :cl-mathstats)
;; (cl-mathstats:correlation '(1 2 3 2 1) '(1 2 3 4 4))
;; (let ((reports (get-reports 2 *testing-ratio*)))
;;   (float (mase
;; 	  (mapcar (lambda (elt)
;; 		    (let ((val (accesses elt :validation :accuracy)))
;; 		      (if (= (aref val 1) 0)
;; 			  0
;; 			  (/ (aref val 0)
;; 			     (aref val 1)))))
;; 		  reports)
;; 	  (mapcar (lambda (elt)
;; 		    (let ((test (accesses elt :test :accuracy)))
;; 		      (if (= (aref test 1) 0)
;; 			  0
;; 			  (/ (aref test 0)
;; 			     (aref test 1)))))
;; 		  reports))))

(defun drop-populations ()
  (with-postgres-connection (execute (delete-from :populations)))
  (with-postgres-connection (execute (delete-from :populations-closure))))
;; (drop-populations)

(defun drop-tests ()
  (with-postgres-connection (execute (delete-from :tests))))
;; (drop-tests)

(defun get-n-best (n candidate)
  "Used in `DRAW-OPTIMIZATION`."
  (let* ((n (if (> n (length candidate)) (length candidate) n))
	 (idxs (subseq (largest-number-indexes
			(loop
			   for agent in (extract-agents-from-pool candidate)
			   collect (consecutive-activations agent)))
		       0 n)))
    (loop
       for idx in idxs
       collect (nth idx candidate))
    ))


(defun get-median-best (candidate)
  "Used in `DRAW-OPTIMIZATION`."
  (let* ((consecutives (loop
			  for agent in (extract-agents-from-pool candidate)
			  collect (consecutive-activations agent)))
	 (med (median (remove-duplicates consecutives)))
	 (idxs (loop
		  for cons in consecutives
		  for idx from 0 to (length consecutives)
		  when (>= cons med)
		  collect idx)))
    (loop
       for idx in idxs
       collect (nth idx candidate))
    ))

(defun draw-optimization (iterations &key (label "") (reset-db nil) (starting-population "") (print-log? t) (agents-fitness-fn #'agents-mase) (fitness-fn #'mase) (sort-fn #'<))
  ;; TODO: We're running `INIT` in multiple functions because some variables
  ;; depend on different points of the process. Fix this.
  ;; (init *instrument* *timeframe*)
  (when print-log?
    (format t "~%~%~A ~A. RUNNING FROM ~a TO ~a~%"
	    *instrument* *timeframe*
	    (local-time:unix-to-timestamp (/ (read-from-string (access (nth *begin* *all-rates*) :time)) 1000000))
	    (local-time:unix-to-timestamp (/ (read-from-string (access (nth *end* *all-rates*) :time)) 1000000))))
  ;; (init *instrument* *timeframe*)
  (when reset-db
    (drop-populations))
  (when (not (string= starting-population ""))
    (init-from-database starting-population))
  (let ((parent-id starting-population)
	(val+test '(0 0)))
    (when print-log?
      ;; (let ((accuracy (agents-accuracy (first *population*))))
      ;; 	(format t "~%~6a ~4$ ~ttrain: ~17a"
      ;; 		*generations*
      ;; 		(float (funcall agents-fitness-fn (first *population*)))
      ;; 		(format-accuracy accuracy)))
      (format t "~%GENERATIONS, #AGENTS, MAPE(train), MAPE(val), MAPE(test), MASE(train), MASE(val), MASE(test), PMAPE(train), PMAPE(val), PMAPE(test), MAE(train), MAE(val), MAE(test), MSE(train), MSE(val), MSE(test), RMSE(train), RMSE(val), RMSE(test), RECALL(train), RECALL(val), RECALL(test), PRECISION(train), PRECISION(val), PRECISION(test), F1-SCORE(train), F1-SCORE(val), F1-SCORE(test), ACCURACY(train), ACCURACY(val), ACCURACY(test), REVENUE(train), REVENUE(val), REVENUE(test)"))
    (dotimes (i iterations)
      (incf *generations*)
      (let ((candidate (let* ((option (random-float *rand-gen* 0 1))
			      (cand (cond
				      ;; Remove an agent.
				      ((and (< option 0.5) (> (length (first *population*)) *community-size*))
				       (remove-nth (random-int *rand-gen* 0 (1- (length (first *population*)))) (first *population*)))
				      ;; Replace agent from solution using random agent.
				      ;; ((and (< option 0.66) (> (length (first *population*)) *community-size*))
				      ;;  (append
				      ;;   (remove-nth (random-int *rand-gen* 0 (1- (length (first *population*)))) (first *population*))
				      ;;   (list (random-int *rand-gen* 0 (1- (length *agents-pool*))))
				      ;;   ))
				      ;; Create new agent and push to pool.
				      ((< option 1.00)
				       (setf *agents-pool* (append *agents-pool* (gen-agents 1)))
				       (append
					(first *population*)
					(list (1- (length *agents-pool*)))))
				      ;; Append random agent from pool.
				      ;; ((<= option 1.0)
				      ;;  (append
				      ;;   (first *population*)
				      ;;   (list (random-int *rand-gen* 0 (1- (length *agents-pool*))))
				      ;;   ))
				      )))
			 ;; (get-n-best *only-best* cand)
			 cand
			 )))
	(let ((cand-error (funcall agents-fitness-fn (get-median-best candidate)))
	      (best-error (funcall agents-fitness-fn (get-median-best (first *population*)))))
	  (when (funcall sort-fn cand-error best-error)
	    
	    (setf (first *population*) candidate))

	  (when (or (> i (- iterations 2))
		    (and print-log?
			 (funcall sort-fn cand-error best-error)))
	    (setf parent-id (insert-best-agents parent-id label fitness-fn sort-fn))

	    (let* ((report (first (get-reports 1 *testing-ratio*)))
		   (median-best (get-median-best candidate))

		   (train-mape (agents-mape median-best))
		   (val-mape (accesses report :validation :performance-metrics :mape))
		   (test-mape (accesses report :testing :performance-metrics :mape))

		   (train-mase (agents-mase median-best))
		   (val-mase (accesses report :validation :performance-metrics :mase))
		   (test-mase (accesses report :testing :performance-metrics :mase))

		   (train-pmape (agents-pmape median-best))
		   (val-pmape (accesses report :validation :performance-metrics :pmape))
		   (test-pmape (accesses report :testing :performance-metrics :pmape))

		   (train-mae (agents-mae median-best))
		   (val-mae (accesses report :validation :performance-metrics :mae))
		   (test-mae (accesses report :testing :performance-metrics :mae))

		   (train-mse (agents-mse median-best))
		   (val-mse (accesses report :validation :performance-metrics :mse))
		   (test-mse (accesses report :testing :performance-metrics :mse))

		   (train-rmse (agents-rmse median-best))
		   (val-rmse (accesses report :validation :performance-metrics :rmse))
		   (test-rmse (accesses report :testing :performance-metrics :rmse))

		   (train-recall (agents-recall median-best))
		   (val-recall (accesses report :validation :performance-metrics :recall))
		   (test-recall (accesses report :testing :performance-metrics :recall))

		   (train-precision (agents-precision median-best))
		   (val-precision (accesses report :validation :performance-metrics :precision))
		   (test-precision (accesses report :testing :performance-metrics :precision))

		   (train-f1-score (agents-f1-score median-best))
		   (val-f1-score (accesses report :validation :performance-metrics :f1-score))
		   (test-f1-score (accesses report :testing :performance-metrics :f1-score))
		     
		   (train-accuracy (agents-accuracy median-best))
		   (val-accuracy (accesses report :validation :performance-metrics :accuracy))
		   (test-accuracy (accesses report :testing :performance-metrics :accuracy))
		     
		   (train-revenue (agents-revenue median-best))
		   (val-revenue (accesses report :validation :performance-metrics :revenue))
		   (test-revenue (accesses report :testing :performance-metrics :revenue)))
	      (when print-log?
		(format t "~%~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a"
		        *generations*
			(length (first *population*))
			train-mape
			val-mape
			test-mape

			train-mase
			val-mase
			test-mase

			train-pmape
			val-pmape
			test-pmape

			train-mae
			val-mae
			test-mae

			train-mse
			val-mse
			test-mse

			train-rmse
			val-rmse
			test-rmse

			train-recall
			val-recall
			test-recall

			train-precision
			val-precision
			test-precision

			train-f1-score
			val-f1-score
			test-f1-score

			(format-accuracy train-accuracy)
			(format-accuracy val-accuracy)
			(format-accuracy test-accuracy)
			  
			train-revenue
			val-revenue
			test-revenue))
	      (setf val+test (list
			      (+ (aref val-accuracy 0) (aref test-accuracy 0))
			      (+ (aref val-accuracy 1) (aref test-accuracy 1))))
	      ))
	  )))
    val+test))

(defun get-starting-population (is-cold-start instrument timeframe)
  "Used by `OPTIMIZE-ALL` and `OPTIMIZE-ONE`."
  (if is-cold-start
      ""
      (let ((pop (get-most-relevant-population instrument timeframe)))
	(if (null pop)
	    ""
	    (if (and (eq (read-from-string (access pop :instrument)) instrument)
		     (eq (read-from-string (access pop :timeframe)) timeframe))
		(access pop :ID)
		""))
	)))

(defun optimize-one (instrument timeframe iterations &key (is-cold-start t) (print-log? t) (agents-fitness-fn #'agents-mase) (fitness-fn #'mase) (sort-fn #'<))
  ;; We need to run init in here and in `DRAW-OPTIMIZATION`, because using
  ;; random range needs to set `*MIN-DATASET-SIZE*` to a bigger size.
  ;; We need to change that one first and then load `*ALL-RATES*`.
  ;; (init instrument timeframe)
  (let ((*instrument* instrument)
	(*timeframe* timeframe)
	;; (*all-rates* (get-rates-count instrument timeframe *min-dataset-size*))
	(starting-population (get-starting-population is-cold-start instrument timeframe)))
    (draw-optimization iterations
		       :label "" :reset-db nil :starting-population starting-population :print-log? print-log?
		       :agents-fitness-fn agents-fitness-fn :fitness-fn fitness-fn :sort-fn sort-fn)))

(defun optimize-all (timeframe iterations
		     &key (instruments ominp:*instruments*)
		       (is-cold-start t))
  (dolist (instrument instruments)
    (format t "~%~%Optimizing ~a ~%" instrument)
    (let ((*instrument* instrument)
	  (*timeframe* timeframe)
          (*all-rates* (get-rates-range instrument timeframe
                                        (local-time:timestamp-to-unix
                                         (local-time:timestamp- (local-time:now)
                                                                (ceiling
                                                                 (* 5 omper:*data-count*))
                                                                (timeframe-for-local-time timeframe)))
                                        (local-time:timestamp-to-unix (local-time:now))))
	  (starting-population (get-starting-population is-cold-start instrument timeframe)))
      (draw-optimization iterations #'agents-mase #'mase #'<
			 :label "" :reset-db nil :starting-population starting-population))))

(defparameter *popular-instruments* '(:AUD_USD :EUR_GBP :EUR_JPY :EUR_USD :GBP_USD :USD_CAD :USD_CHF :USD_JPY))
(defparameter *popular-timeframes* '(:H1 :D))
;; (optimize-all :D 25 :instruments (subseq *popular-instruments* 0) :is-cold-start nil)
;; (test-all-markets :D *popular-instruments*)
;; (test-market :USD_JPY :H1)

(defun loop-optimize-test (iterations &key (instruments-keys '(:all)) (timeframes-keys '(:all)) (print-log? t))
  "Infinitely optimize and test markets represented by the bag of
instruments `INSTRUMENTS-KEYS` for `ITERATIONS`."
  (loop
     (dolist (instruments-key instruments-keys)
       (let ((instruments (cond ((eq instruments-key :all) ominp:*instruments*)
				((eq instruments-key :forex) ominp:*forex*)
				((eq instruments-key :indices) ominp:*indices*)
				((eq instruments-key :commodities) ominp:*commodities*)
				((eq instruments-key :bonds) ominp:*bonds*)
				((eq instruments-key :metals) ominp:*metals*))))
	 (dolist (timeframes-key timeframes-keys)
	   (let ((timeframes (cond ((eq timeframes-key :all) ominp:*timeframes*)
				   ((eq timeframes-key :shortterm) ominp:*shortterm*)
				   ((eq timeframes-key :longterm) ominp:*longterm*)
				   )))
	     (dolist (instrument instruments)
	       (dolist (timeframe timeframes)
		 (let ((*instrument* instrument)
		       (*timeframe* timeframe))
		   (when print-log? (format t "~%~%~a, ~a~%" instrument timeframe))
		   (init instrument timeframe)
		   (optimize-one instrument timeframe iterations :is-cold-start nil :print-log? print-log?)
		   (test-market instrument timeframe))))))))
     (prune-populations)))

(defun tweaking (&key (instrument :EUR_USD) (timeframe :D) (iterations 100) (experiments-count 10) (agents-fitness-fn #'agents-mase) (fitness-fn #'mase) (sort-fn #'<))
  (let ((*instrument* instrument)
	(*timeframe* timeframe)
	(nums nil)
	(denoms nil))
    (dotimes (i experiments-count)
      (init instrument timeframe)
      (let ((run (optimize-one instrument timeframe iterations :is-cold-start t :agents-fitness-fn agents-fitness-fn :fitness-fn fitness-fn :sort-fn sort-fn)))
	 (push (first run) nums)
	 (push (second run) denoms)
	 ;; (format t "~%~%#~a - ACCUMULATION: (~a ~a) = ~a"
	 ;; 	 (1+ i)
	 ;; 	 (reduce #'+ nums)
	 ;; 	 (reduce #'+ denoms)
	 ;; 	 (float (* 100 (if (/= (reduce #'+ denoms) 0) (/ (reduce #'+ nums) (reduce #'+ denoms)) 0))))
	 ))
    ;; (format t "~%~%RESULTS: ~a~%" (list nums denoms))
    ))

;; (dolist (instrument '(:EUR_GBP :EUR_JPY :EUR_USD :GBP_USD :USD_CHF :USD_CAD :USD_CNH :USD_HKD))
;;   (tweaking :instrument instrument :timeframe :H1 :iterations 300 :experiments-count 60))

;; (tweaking :instrument :AUD_USD :timeframe :D :iterations 100 :experiments-count 9 :agents-fitness-fn #'agents-mase :fitness-fn #'mase :sort-fn #'<)
;; (tweaking :instrument :EUR_USD :timeframe :H1 :iterations 300 :experiments-count 30)
;; *population*
;; *generations*

;; (get-n-best 2 (first *population*))
;; (get-median-best (first *population*))
;; (loop for agent in (extract-agents-from-pool (get-n-best *only-best* (first *population*))) collect (print (consecutive-activations agent)))
;; (loop for agent in (extract-agents-from-pool (get-median-best (first *population*))) collect (print (consecutive-activations agent)))

;; (float 
;;  (/ (+ )
;;     (+ )))

;; (loop-optimize-test 25 :instruments-keys '(:all))
;; (test-all-markets :D ominp:*instruments*)
;; (optimize-all :H1 1000)
;; (optimize-all :H1 100 :instruments ominp:*instruments* :is-cold-start t)
;; (optimize-one :AUD_USD :D 50 :is-cold-start t)
;; (optimize-one :EUR_USD :H1 10000 :is-cold-start t)
;; (init *instrument* *timeframe*)
;; (mean-test-error :H1) ;; 0.42777777, 30 *data-count*
;; (mean-test-error :D)
;; (mean-test-error :H1)
;; (length (agent-perception nil))
;; (drop-populations)
;; (drop-tests)
;; (test-market :JP225_USD :D)
;; (test-market :US30_USD :D)
;; (json:encode-json-to-string (test-market :AUD_HKD :D))
;; (draw-optimization 1000 #'agents-mase #'mase #'< :label "" :reset-db t)
;; (dotimes (_ 30) (ignore-errors (draw-optimization 100 #'agents-mase #'mase #'< :label "" :reset-db nil)))
;; (get-reports 1 *testing-ratio*)
;; (print-simulation (first *population*) *testing-ratio* :rmse)
;; (print-simulation)
;; (time (report-all-best-accuracy))

;; (get-best-descendant (access (first (get-root-populations-ids)) :id))

;; 100    0.534  train: 134/136 (98.53%)   val: 2/2 (100.00%)      test: 5/5 (100.00%)
;; 100    0.489  train: 133/134 (99.25%)   val: 22/22 (100.00%)    test: 5/5 (100.00%)
;; 87     0.554  train: 51/61 (83.61%)     val: 14/16 (87.50%)     test: 0/0 (0.00%)
;; 97     0.511  train: 157/166 (94.58%)   val: 1/1 (100.00%)      test: 4/7 (57.14%)
;; 97     0.481  train: 138/141 (97.87%)   val: 6/6 (100.00%)      test: 12/13 (92.31%)
;; 96     0.510  train: 144/151 (95.36%)   val: 0/1 (0.00%)        test: 1/1 (100.00%)
;; 100    0.534  train: 144/148 (97.30%)   val: 0/0 (0.00%)        test: 13/17 (76.47%)
;; 99     0.598  train: 98/99 (98.99%)     val: 3/3 (100.00%)      test: 41/41 (100.00%)
;; 100    0.479  train: 133/136 (97.79%)   val: 9/9 (100.00%)      test: 0/0 (0.00%)
;; 96     0.483  train: 148/152 (97.37%)   val: 0/0 (0.00%)        test: 13/13 (100.00%)
;; 98     0.494  train: 143/149 (95.97%)   val: 0/0 (0.00%)        test: 13/14 (92.86%)
;; 100    0.575  train: 116/128 (90.62%)   val: 0/3 (0.00%)        test: 7/15 (46.67%)
;; 99     0.544  train: 101/107 (94.39%)   val: 8/8 (100.00%)      test: 8/9 (88.89%)
;; 94     0.537  train: 113/116 (97.41%)   val: 5/5 (100.00%)      test: 1/1 (100.00%)
;; 97     0.495  train: 147/150 (98.00%)   val: 0/0 (0.00%)        test: 1/2 (50.00%)
;; 96     0.496  train: 130/131 (99.24%)   val: 4/4 (100.00%)      test: 10/10 (100.00%)
;; 99     0.460  train: 151/153 (98.69%)   val: 7/7 (100.00%)      test: 6/7 (85.71%)
;; 99     0.524  train: 146/146 (100.00%)  val: 11/11 (100.00%)    test: 12/12 (100.00%)
;; 94     0.555  train: 117/133 (87.97%)   val: 10/14 (71.43%)     test: 8/13 (61.54%)
;; 100    0.499  train: 138/149 (92.62%)   val: 11/11 (100.00%)    test: 8/8 (100.00%)
;; 94     0.547  train: 132/141 (93.62%)   val: 0/0 (0.00%)        test: 4/7 (57.14%)
;; 99     0.517  train: 132/137 (96.35%)   val: 12/12 (100.00%)    test: 2/6 (33.33%)
;; 99     0.520  train: 115/123 (93.50%)   val: 5/11 (45.45%)      test: 0/0 (0.00%)

;; (mean '(98.53 99.25 83.61 94.58 97.87 95.36 97.30 98.99 97.79 97.37 95.97 90.62 94.39 97.41 98 99.24 98.69 100 87.97 92.62 93.62 96.35 93.50))
;; (mean '(100 100 87.50 100 100 0 100 100 0 100 100 100 100 100 71.43 100 100 45.45))
;; (mean '(100 100 57.14 92.31 100 76.47 100 100 92.86 46.67 88.89 100 50 100 85.71 100 61.54 100 57.14 33.33))

(defun report-all-best-accuracy ()
  (map nil (lambda (leaf)
	     (format t "~a, ~a, ~a~%"
		     (format-accuracy (accesses leaf :train :accuracy))
		     (format-accuracy (accesses leaf :validation :accuracy))
		     (format-accuracy (accesses leaf :testing :accuracy))
		     ))
       (report-all-best-database)))

;; (rmse (mapcar #'+ '(
;; 		    16.28 22.34 36.82 -14.2 -19.1 -18.0 -29.3 -4.16 4.469
;; 		    )
;; 	      '(4.8 24.9 80.8 84.5 94.0 113.3 69.8 39.8 21.7))
;;       '(24.9 80.8 84.5 94.0 113.3 69.8 39.8 21.7 7))

;; (let ((*rates* *all-rates*)
;;       (omper:*data-count* (- (length *all-rates*) *num-inputs*)))
;;   (agents-simulation (extract-agents-from-pool (first *population*))))

;; (print-simulation)
;; (format-accuracy (accesses (first (get-reports 1 1.0)) :validation :accuracy))
;; (length (first *population*))
;; (dotimes (i (length (first *population*))) (print (length (remove-if #'zerop (agent-trades (nth i (extract-agents-from-pool (first *population*))))))))
;; (length *agents-pool*)
;; (dotimes (i (length (first *population*))) (print (extract-training-clustered-trades (agent-trades (nth i (extract-agents-from-pool (first *population*)))))))

(defun init (instrument timeframe)
  (let* ((config-dir (merge-pathnames #P"config/" *application-root*))
	 (default-config (merge-pathnames #P"default.lisp" config-dir))
	 (general-config (merge-pathnames #P"general.lisp" config-dir))
	 (config (merge-pathnames (pathname (format nil "~a-~a.lisp" instrument timeframe)) config-dir)))
    ;; We always load default.
    (load default-config)
    ;; Then we override parameters according to custom config files.
    (when (probe-file config)
      (load config))
    (load general-config)))

;; (init *instrument* *timeframe*)

;; (time (train 100000 100 :fitness-fn #'accuracy :sort-fn #'> :save-every 10 :epsilon 1.8))
;; (time (train 100000 100 :fitness-fn #'mase :sort-fn #'< :save-every 1 :epsilon 0.00 :gd-epsilon 0.000))
;; *cached-agents*
;; *population*
;; *generations*

;; (filter-reports *results*)
;; (defparameter *results* (get-reports 1 0.1))

;; Init using last entry (generations)
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :mase)))))) :id))

;; Communities' sizes.
;; (mapcar #'length *population*)

;; Get accuracy of *results*.
;; (dolist (res (reverse *results*))
;;   (format t "~a,~a,~a~%" (accesses res :train :accuracy)
;; 	  (accesses res :validation :accuracy)
;; 	  (accesses res :test :accuracy)))

;; Report
;; (map nil (lambda (lst) (format t "gen: ~a ~t mase:~a ~t accuracy: ~a ~t revenue: ~a~%" (access:access lst :generations) (float (access:access lst :mase)) (* (float (access:access lst :accuracy)) 100) (float (access:access lst :revenue)))) (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :creation-time))))))

;; Setting *begin* and *end* according to last entry in database.
;; (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :creation-time)))))
;; *begin*
;; (position 1386046800000000 *all-rates* :key (lambda (elt) (read-from-string (access elt :time))))
;; *end*
;; (1+ (position 1538539200000000 *all-rates* :key (lambda (elt) (read-from-string (access elt :time)))))

;; (setf *begin* 1780)
;; (setf *end* 2382)

;; Print metrics.
;; (csv-real-sim)

;; Get metrics.
;; (float (mase (agents-indexes-simulation (agents-best (agents-distribution *population* #'mase
;; 									  #'<)
;; 						     #'<))
;; 	     (get-deltas omper:*data-count*)))

;; (mapcar (lambda (pop)
;; 	  (float (accuracy (agents-indexes-simulation pop)
;; 			   (get-closes omper:*data-count*))))
;; 	*population*)

;; Check if there are duplicates.
;; (mapcar #'length (mapcar #'remove-duplicates *population*))

;; (setq *last-id* (train 50000 :starting-population *last-id* :fitness-fn #'pmape :sort-fn #'<))
;; (setq *last-id* (train 50000 :starting-population "6DF6BB62-B9AB-4A6B-B6A8-9EF44EFD86FB" :fitness-fn #'pmape :sort-fn #'<))

;; (agent-trades (extract-agent-from-pool (alexandria:random-elt (first *population*))))
;; (slot-value (extract-agent-from-pool (first (first *population*))) 'rules)
;; *generations*

;; continue from database
;; (setq *last-id* (train 50000 :starting-population (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :generations)))))) :id) :fitness-fn #'mase :sort-fn #'<))

;; Init using agents with most accuracy
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :accuracy)))))) :id))

;; Check most accuracy from database.
;; (float (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :accuracy)))))) :accuracy))

;; Woofing individually.
;; (woof (agents-best (agents-distribution *population*)))
;; (agents-accuracy (agents-best (agents-distribution *population*)))
;; (let ((omper:*data-count* (ceiling (* omper:*data-count* 0.1))))
;;   (agents-test (extract-agents-from-pool
;;                 (agents-best (agents-distribution *population*)))
;;                (subseq *all-rates* *begin* (+ *end* omper:*data-count*))))

;; (agents-accuracy (agents-best (agents-distribution *population*)))

;; (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; (get-deltas omper:*data-count*)
;; (agents-accuracy (agents-best (agents-distribution *population*)))
;; (mapcar (lambda (agent)
;;           (slot-value agent 'leverage))
;;         (extract-agents-from-pool (agents-best (agents-distribution *population*))))

;; Checking simulation.
;; (agents-accuracy (nth 8 *population*))
;; (agents-indexes-simulation (nth 8 *population*))
;; (agents-test (extract-agents-from-pool (nth 8 *population*)) (subseq *all-rates* *begin* (+ *end* (* 11 omper:*data-count*))))
;; (get-closes 20)

;; Get agent trades.
;; (apply #'mapcar (lambda (&rest nums)
;; 		  (apply #'+ nums))
;;        (mapcar (lambda (agent)
;; 	  (agent-trades agent))
;; 	       (extract-agents-from-pool (nth 8 *population*))))
;; Single agent trades.
;; (agent-trades (fourth (extract-agents-from-pool (agents-best (agents-distribution *population*)))))
;; (agents-simulation (extract-agents-from-pool (agents-best (agents-distribution *population*))))
;; (agents-indexes-simulation (nth 8 *population*))
;; (get-closes 20)
;; (agents-accuracy (nth 8 *population*))

;; (get-ancestors *last-id*)
;; (access:access (get-population *last-id*) :generations)

;; (length (with-postgres-connection (retrieve-all (select :* (from :populations-closure)))))

;; (access (first (with-postgres-connection (retrieve-all (select (:id :pmape) (from :populations) (order-by (:asc :pmape)))))) :id)

;; (defparameter *results* (report-all-best-database))

;; (get-best-descendant-id "8BD62D44-D989-4FB4-8A0D-F45665602A4A")

(defun train (generations gd-iterations
	      &key (fitness-fn #'mase)
		(sort-fn #'<)
		(starting-population "")
		(save-every 100)
		(epsilon 0.01)
		(gd-epsilon 0.001))
  "Starts the evolutionary process, using the parameters in
`src/config.lisp`. `starting-population` is a v4-uuid that is used to retrieve a
population from Overmind Agents database and is used as a seed to start the
evolutionary process."
  ;; (train 10)
  ;; Checking if it's a fresh start or if we'll create a branch
  ;; from a population stored in the database.
  (if (string= starting-population "")
      ;; Resetting some globals like the population, agents, the cache table, etc.
      (progn
	;; (setf *agents-pool* (gen-agents *num-pool-agents*))
	;; (setf *population* (gen-communities *community-size* *population-size*))
	;; (setf *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t))
	;; (setf *generations* 0)
	;; (setf *fitnesses* nil)
	;; Reset all agents' leverages to 1.
	;; (dolist (agent *agents-pool*)
	;;   (setf (slot-value agent 'leverage) 1))
	)
      (init-from-database starting-population))
  (format t "~%start.~%")
  (let ((parent-id starting-population)
	(can-save? t))
    (dotimes (gen generations) 
      ;; (when (and (access *rules-config* :mf-mean-adjusting)
      ;; 		 (< (random-float *rand-gen* 0 1) 0.20))
      ;; 	(agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 10))
      
      (let* ((fitness-training (agents-reproduce fitness-fn sort-fn))
	     )
        (when (or (null *fitnesses*)
                  ;; (> fitness 0.6)
                  (funcall sort-fn fitness-training (first *fitnesses*))
		  ;; (and (funcall sort-fn fitness-training (first *fitnesses*))
		  ;;      (funcall sort-fn fitness-validation (first *fitnesses-validation*)))
		  )
          (when (and can-save?)
	    (setf parent-id (insert-best-agents parent-id "" fitness-fn sort-fn))
	    (setf can-save? nil)
	    )
	  
          (push fitness-training *fitnesses*)
	  ;; (push fitness-validation *fitnesses-validation*)
          ;; (format t "~%~a: ~a, ~a" *generations* (float fitness-training) (float fitness-validation)))
	  (format t "~%~a: ~a" *generations* (float fitness-training)))
	;; Checking if error threshold is met.
	(when (or (funcall sort-fn fitness-training epsilon)
		  (= gen (1- generations)))
	  (descent-leverages (agents-best (agents-distribution *population* fitness-fn sort-fn) sort-fn)
			     gd-epsilon 1000 gd-iterations)
	  (insert-best-agents parent-id "gd" fitness-fn sort-fn)
	  (return))
	)
      
      ;; This check needs to be before incrementing `*generations*`
      ;; so we always save the root population and we save any
      ;; improvements obtained in the first generations
      ;; after continuing evolving a population.
      (when (= (mod *generations* save-every) 0)
        (setf can-save? t))
      ;; Incrementing global generations counter.
      (incf *generations*))
    (print "done.")
    (format nil "~a" parent-id)))

(defun print-simulation (community ratio metric-kw)
  (let ((omper:*data-count* (ceiling (* omper:*data-count* ratio))))
    (print "validation:")
    (let* ((real (mapcar (lambda (rate)
			   (access rate :close-bid))
			 (subseq *all-rates* *end* (ceiling (+ *end* omper:*data-count*)))))
	   (sim (mapcar #'+
			real
			(accesses (agents-test (extract-agents-from-pool
						community)
					       (subseq *all-rates* *begin* (ceiling (+ *end* omper:*data-count*)))
					       )
				  :simulation))))
      (map nil (lambda (s r)
		 (format t "~%~10f, ~10f" s r))
	   sim
	   real)
      (format t "~%rmse: ~a~%" (rmse sim real)))
    (print "test:")
    (let* ((real (mapcar (lambda (rate)
			  (access rate :close-bid))
			(subseq *all-rates* (+ *end* omper:*data-count*) (ceiling (+ *end* (* 2 omper:*data-count*))))))
	  (sim (mapcar #'+
		       real
		       (accesses (agents-test (extract-agents-from-pool
					       community)
					      (subseq *all-rates* *begin* (ceiling (+ *end* (* 2 omper:*data-count*))))
					      )
				 :simulation))))
      (map nil (lambda (s r)
		 (format t "~%~10f, ~10f" s r))
	   sim
	   real													      )
      (format t "~%rmse: ~a~%" (rmse sim real))
      )))

(defun print-simulation ()
  ;; (print-simulation)
  (map nil (lambda (s r)
	     (format t "~5$, ~5$~%" s r))
       (agents-indexes-simulation (agents-best (agents-distribution *population*)))
       (get-deltas omper:*data-count*)))

;; (print-simulation (first *population*) *testing-ratio* :rmse)
;; (print-simulation)
;; (get-deltas (+ omper:*data-count* 10))

(defun insert-best-agents (parent-id label fitness-fn sort-fn)
  (let* ((child-id (insert-population parent-id label fitness-fn sort-fn)))
    (insert-initial-closure child-id)
    (insert-closure parent-id child-id)
    child-id
    ))

(defclass agent ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (try-until-successful(gen-rules *num-rules*)) :accessor rules)
   (activations :initarg :activations :initform nil :accessor activations)
   (activation-threshold :initarg :activation-threshold :initform 0 :accessor activation-threshold)
   (consecutive-activations :initarg :consecutive-activations :initform 0 :accessor consecutive-activations)
   (leverage :initarg :leverage :initform 1)
   (input-min :initarg :input-min :initform 0)
   (input-max :initarg :input-max :initform 100)
   (output-min :initarg :output-min :initform 0)
   (output-max :initarg :output-max :initform 100)
   ))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules activations activation-threshold consecutive-activations leverage input-min input-max output-min output-max))

(defun get-most-relevant-population (instrument timeframe)
  "The most relevant population is the one that matches `instrument`,
`timeframe` and whose creation date is most recent. However, this function has
some fallback cases that increase the chance of getting a population, even if it
is not ideal."
  (with-postgres-connection
      ;; Trying to retrieve results where both instrument and timeframe match.
      (alexandria:if-let ((both-match (retrieve-one (select :*
						      (from :populations)
						      (where (:= :instrument
								 (format nil "~s" instrument)))
						      (where (:= :timeframe
								 (format nil "~s" timeframe)))
						      (order-by (:desc :end) (:desc :accuracy))
                                                      ))))
	both-match
	;; Couldn't find any. Now trying to retrieve results where instrument matches.
	(alexandria:if-let ((inst-match (retrieve-one (select :*
							(from :populations)
							(where (:= :instrument
								   (format nil "~s" instrument)))
							(order-by (:desc :end) (:desc :accuracy))))))
	  inst-match
	  ;; Couldn't find any. Now trying to retrieve results where timeframe matches.
	  (alexandria:when-let ((time-match (retrieve-one (select :*
							    (from :populations)
							    (where (:= :timeframe
								       (format nil "~s" timeframe)))
							    (order-by (:desc :end) (:desc :accuracy))))))
	    time-match
	    )))))

;; (test-market :SKY :D)
;; (optimize-one :SKY :D 1000 :is-cold-start nil)

(defun getSecondsToExpire (timeframe)
  (cond ((string= timeframe "H1") (* 60 60))
	((string= timeframe "D") (* 24 60 60))))

(let ((cached-tests '()))
  (setf cached-tests '())
  (defun test-market (instrument timeframe)
    (let* ((*instrument* instrument)
	   (*timeframe* timeframe)
	   (*cached-agents* (make-hash-table))
	   (db-pop (get-most-relevant-population instrument timeframe))
	   (pop (decompress-object (access db-pop :population)))
	   (*rates* (subseq *all-rates* *begin* *end*))
	   (report (append `((:population-id . ,(access db-pop :id)))
			   (get-report db-pop instrument timeframe *rates*
				       *begin*
				       *end*)))
	   (pop-id (accesses report :population-id))
	   (str-instrument (format nil "~a" (accesses report :testing :instrument)))
	   (str-timeframe (format nil "~a" (accesses report :testing :timeframe)))
	   ;; The testing period is from validation :begin to testing :end.
	   (begin (accesses report :validation :begin))
	   (end (accesses report :testing :end))
	   (creation-time (local-time:timestamp-to-unix (local-time:now)))
	   (test-mape (/ (+ (accesses report :validation :performance-metrics :mape)
			    (accesses report :testing :performance-metrics :mape))
			 2))
	   (test-mase (/ (+ (accesses report :validation :performance-metrics :mase)
			    (accesses report :testing :performance-metrics :mase))
			 2))
	   (test-pmape (/ (+ (accesses report :validation :performance-metrics :pmape)
			     (accesses report :testing :performance-metrics :pmape))
			  2))
	   (test-mae (/ (+ (accesses report :validation :performance-metrics :mae)
			   (accesses report :testing :performance-metrics :mae))
			2))
	   (test-mse (/ (+ (accesses report :validation :performance-metrics :mse)
			   (accesses report :testing :performance-metrics :mse))
			2))
	   (test-rmse (/ (+ (accesses report :validation :performance-metrics :rmse)
			    (accesses report :testing :performance-metrics :rmse))
			 2))
	   (test-recall (/ (+ (accesses report :validation :performance-metrics :recall)
			    (accesses report :testing :performance-metrics :recall))
			   2))
	   (test-precision (/ (+ (accesses report :validation :performance-metrics :precision)
				 (accesses report :testing :performance-metrics :precision))
			      2))
	   (test-f1-score (/ (+ (accesses report :validation :performance-metrics :f1-score)
				 (accesses report :testing :performance-metrics :f1-score))
			      2))
	   (test-accuracy (let ((val-corr (accesses report :validation :performance-metrics :accuracy))
				(test-corr (accesses report :testing :performance-metrics :accuracy)))
			    (vector (+ (aref val-corr 0) (aref test-corr 0))
				    (+ (aref val-corr 1) (aref test-corr 1)))))
	   (test-revenue (+ (accesses report :validation :performance-metrics :revenue)
			    (accesses report :testing :performance-metrics :revenue)))
	   (decision (format nil "~a" (accesses report :testing :forecast :decision)))
	   (delta (accesses report :testing :forecast :delta)))
      ;; Caching report into `CACHED-TESTS`.
      ;; We'll only cache the result if the decision is different to "HOLD"
      ;; or if there's no test at all for that market/timeframe.
      ;; (when (or (not (string= decision "HOLD"))
      ;; 	        (= (length (remove-if-not (lambda (test)
      ;; 					    (and (string= (access test :timeframe) str-timeframe)
      ;; 						 (string= (access test :instrument) str-instrument)))
      ;; 					  cached-tests))
      ;; 		   0))
      ;; 	)

      ;; Check if we need to remove tests with a "HOLD" decision,
      ;; considering that we're now adding one with an actual decision.
      (setf cached-tests
	    (remove-if (lambda (test)
			 (and (string= (access test :decision) "HOLD")
			      (string= (access test :timeframe) str-timeframe)
			      (string= (access test :instrument) str-instrument)))
		       cached-tests))
      
      ;; Storing the test.
      (push `((:population-id . ,pop-id)
	      (:instrument . ,str-instrument)
	      (:timeframe . ,str-timeframe)
	      (:begin . ,begin)
	      (:end . ,end)
	      (:creation-time . ,creation-time)
	      (:mape . ,test-mape)
	      (:mase . ,test-mase)
	      (:pmape . ,test-pmape)
	      (:mae . ,test-mae)
	      (:mse . ,test-mse)
	      (:rmse . ,test-rmse)
	      (:recall . ,test-recall)
	      (:precision . ,test-precision)
	      (:f1-score . ,test-f1-score)
	      (:accuracy . ,test-accuracy)
	      (:revenue . ,test-revenue)
	      (:decision . ,decision)
	      (:delta . ,delta))
	    cached-tests)

      ;; Removing old cached tests.
      (setf cached-tests
	    (remove-if (lambda (test)
			 (let ((seconds (getSecondsToExpire (access test :timeframe))))
			   ;; If creation time + `seconds` already happened, then it's an old prediction.
			   (< (+ (access test :creation-time)
				 seconds)
			      (local-time:timestamp-to-unix (local-time:now)))))
		       cached-tests))

      (with-postgres-connection
	  (execute (insert-into :tests
		     (set= :population-id pop-id
			   :instrument str-instrument
			   :timeframe str-timeframe
			   :begin begin
			   :end end
			   :creation-time creation-time
			   :mape test-mape
			   :mase test-mase
			   :pmape test-pmape
			   :mae test-mae
			   :mse test-mse
			   :rmse test-rmse
			   :recall test-recall
			   :precision test-precision
			   :f1-score test-f1-score
			   :accuracy test-accuracy
			   :revenue test-revenue
			   :decision decision
			   :delta delta
			   ))))
      report))

  (defun query-db-tests (instrument)
    (let (results)
      (dolist (timeframe ominp:*timeframes*)
	(let ((str-instrument (format nil "~a" instrument))
	      (str-timeframe (format nil "~a" timeframe)))
	  ;; Latest "HOLD".
	  (with-postgres-connection
	      (let ((res (retrieve-one (select :*
					 (from :tests)
					 (where (:= :instrument str-instrument))
					 (where (:= :timeframe  str-timeframe))
					 (where (:= :decision "HOLD"))
					 (where (:> :creation-time (- (local-time:timestamp-to-unix (local-time:now))
								      (getSecondsToExpire str-timeframe))))
					 (order-by (:desc :creation-time))
					 )
				       :as 'trivial-types:association-list)))
		(when res
		  (push res results)))
	    (dolist (test (retrieve-all (select :*
					  (from :tests)
					  (where (:= :instrument str-instrument))
					  (where (:= :timeframe  str-timeframe))
					  (where (:!= :decision "HOLD"))
					  (where (:> :creation-time (- (local-time:timestamp-to-unix (local-time:now))
								       (getSecondsToExpire str-timeframe))))
					  (order-by (:desc :creation-time))
					  )
					:as 'trivial-types:association-list))
	      (setf results
		    (remove-if (lambda (test)
				 (and (string= (access test :decision) "HOLD")
				      (string= (access test :timeframe) str-timeframe)
				      (string= (access test :instrument) str-instrument)))
			       results))
	      (when test
		(push test results)))
	    )))
      (nreverse (remove nil results))))
  ;; (query-db-tests :USD_JPY)

  (defun query-test-instruments (timeframe)
    (remove-if-not (lambda (test)
		     (string= (access test :timeframe)
			      timeframe))
		   cached-tests))

  (defun query-test-timeframes (instrument)
    (remove-if-not (lambda (test)
		     (string= (access test :instrument)
			      instrument))
		   cached-tests))

  (defun check-cached-tests ()
    "Function used to check if `CACHED-TESTS` is empty. If it is
empty, `CHECK-CACHED-TESTS` will fill `CACHED-TESTS` with the latest
tests performed that are stored on database."
    (when (= (length cached-tests) 0)
      (map nil (lambda (instrument)
		 (map nil (lambda (entry)
			    ;; Timeframe entries will be strings, not keywords.
			    (push entry cached-tests)
			    ;; (map nil (lambda (test)
			    ;; 	       (push test cached-tests))
			    ;; 	 entry)
			    ;; (setf (gethash (read-from-string (format nil ":~a" (access entry :timeframe))) entries) entry)
			    )
		      (query-db-tests instrument))
		 
		 ;; (let ((entries (make-hash-table)))
		 ;;   (map nil (lambda (entry)
		 ;; 	      ;; Timeframe entries will be strings, not keywords.
		 ;; 	      (setf (gethash (read-from-string (format nil ":~a" (access entry :timeframe))) entries) entry))
		 ;; 	(query-db-tests instrument))
		 ;;   (setf (gethash instrument cached-tests) entries))
		 )
	   ominp:*instruments*))
    cached-tests)
  ;; Run check.
  (check-cached-tests))

;; (print (query-test-instruments :H1))
;; (query-test-timeframes :AUD_USD)

;; (test-market :EUR_USD :D)
;; (test-market :BCO_USD :D)
;; (test-market :FR40_EUR :H1)
;; (test-market :SPX500_USD :D)

(defun test-all-markets (timeframe instruments)
  (dolist (instrument instruments)
    (format t "~%Testing ~a ~%" instrument)
    (test-market instrument timeframe)))
;; (test-all-markets :H1 ominp:*instruments*)

;; (time (loop repeat 10000 do (query-test-timeframes :EUR_USD)))

;; (with-postgres-connection
;;     (retrieve-one (select :*
;; 		    (from :populations)
;; 		    (where (:= :instrument
;; 			       (format nil "~s" :AUD_USD)))
;; 		    (where (:= :timeframe
;; 			       (format nil "~s" :H1)))
;; 		    (order-by (:desc :end) (:desc :accuracy))
;; 		    )))

;; querying latest market reports (query `tests`)
;; testing a market (periodically run tests with latest populations, save to `tests`)
;; optimizing for a market (`draw-optimization`)

(defun timeframe-for-local-time (timeframe)
  (cond ((eq timeframe :H1) :HOUR)
        ((eq timeframe :D) :DAY)))

;; (get-deltas omper:*data-count*)

;; (access (get-most-relevant-population *instrument* *timeframe*) :best-index)

(defun market-report (db-pop instrument timeframe rates)
  "Searches for a relevant population in the database that has a good fitness
for the environment defined by `instrument`, `timeframe`, `begin` and `end`, where
`begin` and `end` define a period of time for the market `instrument` at
granularity `timeframe`."
  ;; Resetting agents cache so we don't get wrong results.
  ;; (setf *cached-agents* (make-hash-table :test #'equal))
  (let* ((*instrument* instrument)
	 (*timeframe* timeframe)
	 (*rates* rates)
	 ;; (db-pop (get-most-relevant-population instrument timeframe))
	 (pop (decompress-object (access db-pop :population)))
	 (best (nth (access db-pop :best-index) pop))
	 (sim (agents-simulation best))
	 (next-sim-price (alexandria:last-elt sim)))
    `((:training (:begin . ,(access db-pop :begin))
		 (:end . ,(access db-pop :end))
                 (:instrument . ,(read-from-string (access db-pop :instrument)))
                 (:timeframe . ,(read-from-string (access db-pop :timeframe)))
                 (:generations . ,(access db-pop :generations))
                 (:fitness-fn . ,(read-from-string (access db-pop :fitness-fn)))
                 (:creation-time . ,(access db-pop :creation-time))
                 (:performance-metrics . ((:mae . ,(access db-pop :mae))
                                          (:mse . ,(access db-pop :mse))
                                          (:rmse . ,(access db-pop :rmse))
					  (:recall . ,(access db-pop :recall))
					  (:precision . ,(access db-pop :precision))
					  (:f1-score . ,(access db-pop :f1-score))
                                          (:mape . ,(access db-pop :mape))
					  (:mase . ,(access db-pop :mase))
                                          (:pmape . ,(access db-pop :pmape))
                                          (:accuracy . ,(access db-pop :accuracy))
                                          (:revenue . ,(access db-pop :revenue)))))
      (:testing (:begin . ,(read-from-string (access (first rates) :time)))
		(:end . ,(read-from-string (access (alexandria:last-elt rates) :time)))
                (:instrument . ,instrument)
                (:timeframe . ,timeframe)
                ,@(agents-test best rates)
                ;; (:rates . ,rates)
                ))
    ))

(defun make-history-function (periods evaluator)
  "Returns a function that takes a series of data points, one at a
time, and returns the results of calling the evaluator with a sequence
storing PERIODS data points. The function returns NIL before PERIODS
data points have been evaluated. If the data point is NIL, the
function returns NIL and does not store the data point in the
history."
  (let ((history '())
	(count 0))
    (lambda (element)
      (if element
	  (progn
	    (push element history)
	    (incf count)
	    (if (>= count periods)
		(progn
		  (setf history (subseq history 0 periods))
		  (funcall evaluator history))
		nil))
	  nil))))

(defun moving-average (periods &key (key #'identity))
  "Creates a function that computes a moving average over PERIODS data points."
  (make-history-function periods (lambda (history)
				   (ta-average history :key key))))

;; Agent inputs "getters".

(defun subseq-input (seq)
  (subseq seq 0 omper:*data-count*))

(defun get-deltas (count)
  "This one predicts N periods in future as a big single delta."
  (let ((reals (get-closes (+ count *delta-gap*))))
    (remove nil
	    (maplist (lambda (real)
		       (unless (<= (length real) *delta-gap*)
			 (- (nth *delta-gap* real)
			    (nth 0 real)
			    )))
		     reals))))

(defun get-closes (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 count)))

(defun get-volumes (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :volume rate))) *rates*)) 0 count)))

(defun get-highs (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :high-bid rate))) *rates*)) 0 count)))

(defun get-lows (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :low-bid rate))) *rates*)) 0 count)))

(defun io-closes (&key (offset 0))
  (subseq-input (get-closes (+ offset omper:*data-count*))))

(defun io-volumes (&key (offset 0))
  (subseq-input (get-volumes (+ offset omper:*data-count*))))

(defun io-highs (&key (offset 0))
  (subseq-input (get-highs (+ offset omper:*data-count*))))

(defun io-lows (&key (offset 0))
  (subseq-input (get-lows (+ offset omper:*data-count*))))

(defun io-high-heights (&key (offset 0))
  (subseq-input
   (loop for candle in (get-full-real-data (+ offset omper:*data-count*))
      collect (if (> (access candle :open-bid)
		     (access candle :close-bid))
		  (- (access candle :high-bid)
		     (access candle :open-bid))
		  (- (access candle :high-bid)
		     (access candle :close-bid))))))

(defun io-low-heights (&key (offset 0))
  (subseq-input
   (loop for candle in (get-full-real-data (+ offset omper:*data-count*))
      collect (if (> (access candle :open-bid)
		     (access candle :close-bid))
		  (- (access candle :close-bid)
		     (access candle :low-bid))
		  (- (access candle :open-bid)
		     (access candle :low-bid))))))

(defun io-candle-heights (&key (offset 0))
  (subseq-input
   (loop for candle in (get-full-real-data (+ offset omper:*data-count*))
      collect (abs (- (access candle :close-bid)
		      (access candle :open-bid))))))

(defun io-deltas (delta-gap &key (offset 0))
  "This one predicts N periods in future as a big single delta."
  (let ((reals (get-closes (+ offset omper:*data-count* delta-gap))))
    (remove nil
	    (maplist (lambda (real)
		       (unless (<= (length real) delta-gap)
			 (- (nth delta-gap real)
			    (nth 0 real)
			    )))
		     reals))))

(defun io-full-heights (&key (offset 0))
  (subseq-input (mapcar #'- (get-highs (+ offset omper:*data-count*))
			(get-lows (+ offset omper:*data-count*)))))

;; (io-deltas 10 :offset 1)

(defun io-fibos (z-count &key (offset 0))
  "Generates the fibos of a market."
  (subseq-input
   (last (mapcar (lambda (heat)
		   (let ((heat (cl21:gethash heat :heat))
			 (close (cl21:gethash heat :close)))
		     (let* ((z (cl21:gethash heat :z))
			    (index (+ (if-let
					  ((res (search `(,close) (cl21:gethash heat :y)
							:key (lambda (elt)
							       (when (> close elt) t)))))
					res
					(1- (length (cl21:gethash heat :y)))
					)
				      (floor (/ z-count 2))))
			    (res (append (mapcar (lambda (i)
						   (nth (if (< (- index (- z-count i 1)) 0)
							    i
							    (if (>= (- index (- z-count i 1))
								    (length z))
								(1- (length z))
								(- index (- z-count i 1))))
							z))
						 (iota (1- z-count)))
					 (list (if-let ((nth-z (nth index z)))
						 nth-z 0.0)))))

		       ;; (append res (list close))
		       res
		       )))
		 ;; TODO: generalize (get-data) so it doesn't need an `instrument`.
		 ;; (get-data *instrument* *rates* :levels (slot-value agent 'beliefs))
		 ;; (get-data *instrument* *rates*)
		 (get-data *instrument* *rates*))
	 (+ offset omper:*data-count*))))

;; (length (time (io-fibos 6 :offset 0)))

(defun io-stochastic-oscillator (%k-periods %d-periods &key (offset 0))
  (subseq-input
   (last (remove nil
		 (mapcar (stochastic
			  (lambda (rate)
			    (access rate :high-bid))
			  (lambda (rate)
			    (access rate :low-bid))
			  (lambda (rate)
			    (access rate :close-bid))
			  :%k-periods %k-periods
			  :%d-periods %d-periods)
			 (get-full-real-data (+ omper:*data-count*
						offset
						(+ %d-periods %k-periods)
						))))
	 (+ offset omper:*data-count*))))

(defun io-sma (periods &key (offset 0))
  (subseq-input
   (sma periods (get-closes
		 (+ omper:*data-count*
		    (1- periods))))))

(defun io-awma (period &key (offset 0))
  (subseq-input
   (awma period (get-closes
		 (+ omper:*data-count*
		    (1- period))))))

(defun io-ema (period &key (offset 0))
  (subseq-input
   (ema period (get-closes
		(+ omper:*data-count*
		   offset
		   (1- period))))))

;; (length (io-ema 10 :offset 2))
;; (time (dotimes (x 10) (io-ema 10 :offset 0)))

(defun io-macd (period-a period-b &key (offset 0))
  (subseq-input (macd period-a period-b
		      (get-closes (+ offset (1- (max period-a period-b)) omper:*data-count*)))))

;; (time (dotimes (x 10) (io-macd 10 20 :offset 0)))

(defun io-macd-signal (smoothing-period period-a period-b &key (offset 0))
  (subseq-input (macd-signal smoothing-period
			     period-a
			     period-b
			     (get-closes (- (+ offset smoothing-period (max period-a period-b) omper:*data-count*) 2)))))

(defun ta-average (sequence &key (key #'identity))
  (let ((len 1))
    (/ (reduce #'(lambda (x y) (incf len) (+ x y)) sequence :key key)
       len)))

(defun bollinger-bands (periods &key (key #'identity))
  "Creates a function that computes Bollinger bands over PERIODS data
points. The function returns a list with the average, upper, and lower
Bollinger band points."
  (make-history-function periods
			 (lambda (history)
			   (let* ((avg (average history :key key))
				  (std (std-dev history :key key))
				  (2std (* 2 std))
				  (upper (+ avg 2std))
				  (lower (- avg 2std)))
			     (list avg upper lower)))))

(defun max-price (history &key key)
  (apply #'max (mapcar key history)))
(defun min-price (history &key key)
  (apply #'max (mapcar key history)))

(defun stochastic (key-high key-low key-close
		   &key (%k-periods 5) (%d-periods 5))
  "Creates a function that computes the stochastic indicator over the
specified number of periods for %K and $D. This function requires
access to high, low, and closing prices for all data points and so
accessor functions must be passed to the function to retrieve the data
from each sample."
  (let ((%d-history (moving-average %d-periods)))
    (make-history-function %k-periods
			   (lambda (history)
			     (let* ((high (max-price history :key key-high))
				    (low (min-price history :key key-low))
				    (close (funcall key-close (elt history 0)))
				    (%k (* 100 (/ (- close low)
						  (- high low))))
				    (%d (funcall %d-history %k)))
			       ;; (list %k %d)
                               %d)))))

(defun get-report (db-pop instrument timeframe rates
                   &optional (begin *begin*) (end *end*) (ratio *testing-ratio*))
  ;; (format t "~%dbg.get-report: ~a, ~a, ~a, ~a, ~a, ~a" begin end (length *all-rates*) (length rates) ratio omper:*data-count*)
  (let* ((omper:*data-count* (ceiling (* omper:*data-count* ratio)))
         (*instrument* instrument)
         (*timeframe* timeframe)
         (*rates* rates)
         (validation (market-report db-pop
				    *instrument* *timeframe*
				    (subseq *all-rates* begin (+ end omper:*data-count*))))
	 (test (market-report db-pop
			      *instrument* *timeframe*
			      (subseq *all-rates* begin (+ end (* 2 omper:*data-count*)))))
	 (result `((:train . ,(accesses test :training))
		   (:validation . ,(accesses validation :testing))
		   (:testing . ,(accesses test :testing)
			     ))))
    result))

(defun get-reports (count &optional (ratio *testing-ratio*))
  (mapcar (lambda (db-pop)
	    (get-report db-pop *instrument* *timeframe*
                        *rates* *begin* *end* *testing-ratio*))
	  (with-postgres-connection
	      (retrieve-all (select (:*)
			      (from :populations)
			      (order-by (:desc :creation-time))
			      (limit count))))))
;; (get-reports 1)

;; (filter-reports *reports*)
;; (defparameter *reports* (get-reports 1))
;; *cached-agents*
;; *generations*

(defun filter-reports (reports)
  (let ((train-mase 0)
	(train-accuracy 0)
	(val-mase 0)
	(val-accuracy 0)
	(test-mase 0)
	(test-accuracy 0)
	(len 0))
    (dolist (report reports)
      (when (and ;; (> (accesses report :train :accuracy) 0.5)
             (< (accesses report :train :mase) 0.01)
             ;; (> (accesses report :validation :accuracy) 0.6)
             ;; (< (accesses report :validation :mase) 0.02)
             )
	(incf train-mase (accesses report :train :mase))
	(incf train-accuracy (accesses report :train :accuracy))
	(incf val-mase (accesses report :validation :mase))
	(incf val-accuracy (accesses report :validation :accuracy))
	(incf test-mase (accesses report :testing :mase))
	(incf test-accuracy (accesses report :testing :accuracy))
	(incf len)))
    (when (> len 0)
      `((:train-mase . ,(float (/ train-mase len)))
	(:train-accuracy . ,(float (/ train-accuracy len)))
	(:validation-mase . ,(float (/ val-mase len)))
	(:validation-accuracy . ,(float (/ val-accuracy len)))
	(:test-mase . ,(float (/ test-mase len)))
	(:test-accuracy . ,(float (/ test-accuracy len)))
        (:sample-size . ,len)))))

;; (accesses *market-report* :training :performance-metrics :accuracy)
;; (accesses *market-report* :testing :performance-metrics :accuracy)
;; (time (market-report :EUR_USD :H1 *begin* *end*))

;; (accesses (agents-indexes-test (first *population*)
;;                                (get-rates-range :GBP_USD *timeframe*
;;                                                 
;;                                                 (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 500 :hour))))
;;           :performance-metrics)

(defun agents-test (agents rates)
  "Runs a simulation of a market defined by `rates` using `agents`. Returns multiple performance metrics."
  (let* ((*rates* rates)
         (sim (agents-simulation agents))
         (real (get-deltas (length sim)))
         (mae (mae sim real))
	 (mse (mse sim real))
	 (rmse (rmse sim real))
	 (recall (recall sim real))
	 (precision (precision sim real))
	 (f1-score (f1-score sim real))
	 (mape (mape sim real nil))
	 (mase (mase sim real nil))
	 (pmape (pmape sim real))
	 (accuracy (accuracy sim real nil))
	 (revenue (revenue sim real)))
    `((:performance-metrics . ((:mae . ,mae)
                               (:mse . ,mse)
                               (:rmse . ,rmse)
			       (:recall . ,recall)
			       (:precision . ,precision)
			       (:f1-score . ,f1-score)
                               (:mape . ,mape)
			       (:mase . ,mase)
                               (:pmape . ,pmape)
                               (:accuracy . ,accuracy)
                               (:revenue . ,revenue)))
      ;; (:simulation . ,sim)
      (:forecast (:delta . ,(last-elt sim))
		 (:decision . ,(if (> (last-elt sim) 0)
				   :BUY
				   (if (= (last-elt sim) 0)
				       :HOLD
				       :SELL))))
      )))

(defun agents-indexes-test (agents-indexes rates)
  "Runs a simulation of a market defined by `rates` using `agents-indexes`. `agents-indexes` is used to extract agents from `*agents-pool*`. Returns multiple performance metrics."
  (agents-test (extract-agents-from-pool agents-indexes) rates))

;; (dolist (pop *population*)
;;   (dolist (beliefs (slot-value pop 'beliefs))
;;     (print beliefs)))

;; ;; (dolist (real (get-closes))
;; ;;   (print real))

;; ;; ;; make a test-all method to test a model against multiple markets, multiple testing sets
;; ;; ;; the objective is to prove that our architecture is generalized enough to work anywhere
;; ;; (let ((*rates* (subseq (ms:unmarshal (read-from-string (file-get-contents "/home/amherag/quicklisp/local-projects/neuropredictions/data/aud_usd.dat"))) 500 1000)))
;; ;;   (accuracy (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; ;; 	  (get-closes)))

;; (defparameter *best-agent* (agents-best (agents-distribution *population*)))

(defun agents-ifs (params)
  (let ((stdev (if (= (nth 3 params) 0.0)
		   1.0
		   (nth 3 params))))
    (ifs (gaussian-mf (nth 0 params) stdev (nth 4 params) (nth 5 params) 0 1)
	 (gaussian-nmf (nth 1 params) stdev (nth 4 params) (nth 5 params) 0 1))))

(defun purge-non-believers (population)
  "Searches in `pop` if an agent is not believing in anything, i.e. a vector of
`NIL`s. Replaces this sequence with a random belief vector."
  ;; (purge-non-believers *population*)
  (dotimes (i (length population))
    (dotimes (j (length (slot-value (nth i population) 'beliefs)))
      (if (every #'null (nth j (slot-value (nth i population) 'beliefs)))
	  (setf (nth j (slot-value (nth i population) 'beliefs))
	  	(first (gen-beliefs 1)))))))

(defun process-agent-output (out leverage)
  (* (- (* out 2) 100) leverage))
;; (float (process-agent-output 0 1))

(defun agents-selectone (distribution)
  "Done"
  (let ((random (random-float *rand-gen* 0 1.0)) 
	(prob 0) 
	genotype)
    (some (lambda (pair) 
	    (incf prob (car pair))
	    (if (> random prob)
		nil
		(setf genotype (car (cdr pair))))) 
	  distribution)
    genotype))

(defun agents-descent (agents-indexes)
  (descent (lambda (leverages)
	     (fit agents-indexes leverages))
	   (flatten
	    (mapcar (lambda (agent)
		      (slot-value agent 'leverage))
		    (extract-agents-from-pool agents-indexes)))
	   :error 0.0d0
	   :rate (* (calc-trade-scale) *community-size*)
	   :max-steps 1))

(defun agents-crossover (x y &optional (chance 0.6))
  (if (<= (random-float *rand-gen* 0 1.0) chance)
      (let* ((lenx1 (* 2 (ceiling (/ (random-int *rand-gen* 1 (1- (length x))) 2))))
             (lenx2 (* 2 (ceiling (/ (random-int *rand-gen* 1 (1- (length x))) 2))))
             (leny1 (* 2 (ceiling (/ (random-int *rand-gen* 1 (1- (length y))) 2))))
             (leny2 (* 2 (ceiling (/ (random-int *rand-gen* 1 (1- (length y))) 2))))
             (x1 (subseq (shuffle x) 0 lenx1))
	     (x2 (subseq (shuffle x) 0 lenx2))
	     (y1 (subseq (shuffle y) 0 leny1))
	     (y2 (subseq (shuffle y) 0 leny2)))
	(let ((newx (append x1 y2))
	      (newy (append y1 x2)))
	  (values newx
		  newy)))
      ;; no change
      (values x y)))

(defun agents-crossover (x y &optional (chance 0.6))
  (if (<= (random-float *rand-gen* 0 1.0) chance)
      (let* ((x (shuffle x))
             (y (shuffle y))
             (site (random-int *rand-gen* 1 (1- *community-size*)))
	     (x1 (subseq x 0 site))
	     (x2 (nthcdr site x))
	     (y1 (subseq y 0 site))
	     (y2 (nthcdr site y)))
	(let ((newx (append x1 y2))
	      (newy (append y1 x2)))
	  ;; Performing gradient descent to improve agents' leverages.
	  ;; (agents-descent newx)
	  ;; (agents-descent newy)
	  ;; (descent-leverages newx 0.0 1d-5 5)
	  ;; (descent-leverages newy 0.0 1d-5 5)
	  (values newx
		  newy)))
      ;; no change
      (values x y)))

(defun extract-agent-from-pool (agent-index)
  "Extracts the agent represented by `agent-index` from *agents-pool*."
  (nth agent-index *agents-pool*))

(defun extract-agents-from-pool (agents-indexes)
  "Uses the indexes in `agents-indexes` to compile a list of the agents
represented by those indexes in `*agents-pool*`."
  ;; (extract-agents-from-pool '(1 2 3))
  (mapcar #'extract-agent-from-pool agents-indexes))

(defun agents-indexes-simulation (agents-indexes)
  "Returns a simulation of multiple agents trading a dataset. The agents are
extracted from `*agents-pool*` using the indexes stored in `agents-indexes`."
  ;; (agents-indexes-simulation (first *population*))
  ;; (woof agents-indexes)
  (agents-simulation (extract-agents-from-pool agents-indexes)))

(defun agents-simulation (agents)
  "Returns a simulation of multiple agents trading a dataset."
  (let ((sim (apply #'mapcar (lambda (&rest trades)
			       ;; (apply #'+ trades)
			       
			       ;; (let* ( ;; (threshold (if (< (length (first *population*)) 1)
			       ;; 	      ;; 		     (length (first *population*))
			       ;; 	      ;; 		     1))
			       ;; 	      (no-zeros (remove-if #'zerop trades))
			       ;; 	      (pos (remove-if #'minusp no-zeros))
			       ;; 	      (neg (remove-if #'plusp no-zeros))
			       ;; 	      ;; (lzer (length no-zeros))
			       ;; 	      (lpos (length pos))
			       ;; 	      (lneg (length neg)))
			       ;; 	 (cond ;; ((< lzer threshold) 0)
			       ;; 	   ((> lpos lneg) (mean pos))
			       ;; 	   ((> lneg lpos) (mean neg))
			       ;; 	   (t ;;(mean no-zeros)
			       ;; 	    0
			       ;; 	    )))

			       ;; (let* ((no-zeros (remove-if #'zerop trades)))
			       ;; 	 (if (and (> (length no-zeros) 1)
			       ;; 		  (or (every #'plusp no-zeros)
			       ;; 		      (every #'minusp no-zeros)))
			       ;; 	     (mean no-zeros)
			       ;; 	     0))


			       (let* ((no-zeros (remove-if #'zerop trades)))
			       	 (if (= (length no-zeros) 0)
			       	     0
			       	     (mean no-zeros)))
			       
			       ;; (mean trades)
			       ;; (if (/= (length (remove-if #'zerop trades)) 1)
			       ;; 	   0
			       ;; 	   (mean (remove-if #'zerop trades)))

			       )

		    (mapcar (lambda (agent)
			      (agent-trades agent))
			    agents)
		    )))
    sim
    ))

(defun agent-simulation (agent)
  "Returns a simulation of a single agent trading a dataset."
  (let ((deltas (agent-trades agent)))
    deltas
    ;; (mapcar #'+
    ;; 	    deltas
    ;; 	    (get-closes (length deltas)))
    ))

;; (length (agent-simulation (first *agents-pool*)))

(defun agents-mutate ()
  "Mutates the pool of agents if a according to `chance`."
  ;; (agents-mutate 0.5)

  ;;   (let* ((leverages (mapcar (lambda (i)
  ;; 				(abs (magicl:ref sol-matrix i 0)))
  ;; 			      (alexandria:iota (length agents))))
  ;; 	   (leverage-idx (if (< (random-float *rand-gen* 0 1) 0.5)
  ;; 			     (largest-number-index leverages)
  ;; 			     (alexandria:last-elt (largest-number-indexes leverages))
  ;; 			     )))
  ;;     (setf (nth leverage-idx agents)
  ;; 	    (random-int *rand-gen* 0 (1- *num-pool-agents*)))))

  
  ;; (when (> chance (random-float *rand-gen* 0 1.0))
  ;;   (let ((community (nth (random-int *rand-gen* 0 (1- *population-size*)) *population*))
  ;; 	  (mutation (random-int *rand-gen* 0 (1- *num-pool-agents*))))
  ;;     (unless (find mutation community)
  ;; 	(setf (nth (random-int *rand-gen* 0 (1- *community-size*))
  ;; 		   community)
  ;; 	      mutation))))


  ;; Replace whole community.
  ;; (when (> chance (random-float *rand-gen* 0 1.0))
  ;;   (let ((agent (extract-agent-from-pool (random-elt (random-elt *population*)))))
  ;;     (setf (slot-value agent 'rules)
  ;; 	    (mapcar (lambda (inp-rules)
  ;; 		      (mapcar (lambda (rule)
  ;; 				(list (+ (nth 0 rule)
  ;; 					 (random-float *rand-gen* -1 1))
  ;; 				      (+ (nth 1 rule)
  ;; 					 (random-float *rand-gen* -1 1))
  ;; 				      (+ (nth 2 rule)
  ;; 					 (random-float *rand-gen* -0.1 0.1))))
  ;; 			      inp-rules))
  ;; 		    (slot-value agent 'rules)))))

  ;; Replace single agent, maintaining position.
  ;; (when (> chance (random-float *rand-gen* 0 1.0))
  ;;   (let ((community-idx (position (agents-worst (agents-distribution *population*))
  ;; 				   *population* :test #'equal)))
  ;;     (setf (nth community-idx *population*)
  ;; 	    (mapcar (lambda (_)
  ;; 		      (random-int *rand-gen* 0 *num-pool-agents*))
  ;; 		    ;; (iota (floor (* 1.5 (apply #'max (mapcar #'length *population*)))))
  ;; 		    (iota *community-size*)
  ;; 		    )))

    
  ;; Replace random agent from random community.
  (when (> *mutation-chance* (random-float *rand-gen* 0 1.0))
    (let ((community-idx (position (agents-worst (agents-distribution *population*))
  				   *population* :test #'equal)))
      (setf (nth (random-int *rand-gen* 0 (1- (length (nth community-idx *population*))))
		 (nth community-idx *population*))
	    (random-int *rand-gen* 0 (1- *num-pool-agents*))))))

(defun agents-profit (agents-indexes)
  "Uses the market simulation created by the agents represented by `agents-indexes` to generate a list of profits generated by comparing the simulation against the real market data."
  ;; (agents-profit '(1 2 4))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (revenue sim (get-deltas (length sim)))))

(defun agents-describe (agents-indexes)
  (let* ((fibos (mapcar (lambda (agent-index)
			  (agent-perception (extract-agent-from-pool agent-index)))
			agents-indexes))
         (sim (agents-indexes-simulation agents-indexes))
	 (agent-rules (mapcar (lambda (agent)
				(slot-value agent 'rules))
			      (extract-agents-from-pool agents-indexes)))
         (agents-profits (mapcar (lambda (index)
                                   (agents-profit (list index)))
                                 agents-indexes))
	 (agents-perception
          (let ((simp-fibos (mapcar (lambda (agent-data)
                                      (let* ((data-no-pi (mapcar (lambda (elt) (subseq elt 0 3)) agent-data))
                                             (mx (apply #'max (alexandria:flatten data-no-pi)))
                                             )
                                        (mapcar (lambda (datum)
                                                  (mapcar (lambda (d)
                                                            (let ((nd (/ d mx)))
                                                              (cond ((< nd (/ 1 3)) 0)
                                                                    ((< nd (/ 2 3)) 1)
                                                                    (t 2))))
                                                          datum))
                                                data-no-pi)
                                        ))
                                    fibos)))
            (mapcar (lambda (agent-fibos)
                      (let ((above-high 0) (level-high 0) (below-high 0)
                            (above-med 0) (level-med 0) (below-med 0)
                            (above-low 0) (level-low 0) (below-low 0)
                            (lfib (length agent-fibos)))
                        (mapcar (lambda (fibos)
                                  (cond ((= (first fibos) 0) (incf above-low))
                                        ((= (first fibos) 1) (incf above-med))
                                        ((= (first fibos) 2) (incf above-high)))
			    
                                  (cond ((= (second fibos) 0) (incf level-low))
                                        ((= (second fibos) 1) (incf level-med))
                                        ((= (second fibos) 2) (incf level-high)))
			    
                                  (cond ((= (third fibos) 0) (incf below-low))
                                        ((= (third fibos) 1) (incf below-med))
                                        ((= (third fibos) 2) (incf below-high)))
                                  )
                                agent-fibos)
                        (list (list (/ above-low lfib) (/ above-med lfib) (/ above-high lfib))
                              (list (/ level-low lfib) (/ level-med lfib) (/ level-high lfib))
                              (list (/ below-low lfib) (/ below-med lfib) (/ below-high lfib)))))
                    simp-fibos)
            )))
    (let ((interprets (make-hash-table :test #'equal)))
      (mapcar (lambda (agent-number agent-rules)
        	(let ((dups (make-hash-table :test #'equal)))
        	  (mapcar (lambda (agent-rule)
        		    (let ((simp-rule (mapcar (lambda (clause)
        					       (let ((b (apply #'beta clause))
        						     (action (cond ((< (first clause) 33) 0) ;; sell
        								   ((< (first clause) 66) 1) ;; buy or sell a little
        								   (t 2) ;; buy
        								   )))

        						 (list action b)
        						 ))
        					     agent-rule)))
        		      (mapcar (lambda (fibo-type clause)
        				;; buy; above; low fib
        				(let ((key (list `(:action ,(first (alexandria:last-elt simp-rule)))
        						 `(:fibo-type ,fibo-type)
        						 `(:fibo-strength ,(first clause))))
        				      (num-agents )
        				      (pi-fibo (second clause))
        				      (pi-action (second (alexandria:last-elt simp-rule)))
        				      (profit (cl21:nth agents-profits agent-number))
        				      (perception (cl21:nth (cl21:nth (cl21:nth agents-perception agent-number)
        								      fibo-type)
        							    (first clause))))
        				  (when (gethash key interprets) ;; already exists, I need to average
        				    (setq pi-fibo (/ (+ (first (access (gethash key interprets) :pi-fibo))
        							pi-fibo)
        						     2)
        					  pi-action (/ (+ (first (access (gethash key interprets) :pi-action))
        							  pi-action)
        						       2)
        					  profit (/ (+ (first (access (gethash key interprets) :profit))
        						       profit)
        						    2)
        					  perception (/ (+ (first (access (gethash key interprets) :perception))
        							   perception)
        							2)))
				 
        				  (if (gethash key dups)
        				      (setf (gethash key interprets)
        					    (list `(:num-agents ,(first (access (gethash key interprets) :num-agents)))
        						  `(:pi-fibo ,pi-fibo)
        						  `(:pi-action ,pi-action)
        						  `(:perception ,(float perception))
        						  `(:profit ,profit)))
        				      (progn
        					(setf (gethash key dups) t)
        					(setf (gethash key interprets)
        					      (list `(:num-agents ,(if (gethash key interprets)
        								       (1+ (first (access (gethash key interprets) :num-agents)))
        								       1))
        						    `(:pi-fibo ,pi-fibo)
        						    `(:pi-action ,pi-action)
        						    `(:perception ,(float perception))
        						    `(:profit ,profit)))))))
        			      (cl21:iota 3)
        			      (reverse (rest (reverse simp-rule))))))
        		  agent-rules)))
              (cl21:iota *population-size*)
              ;; (slot-value agents 'rules)
	      agent-rules
	      )
      (format nil "~{~a~^~%~}"
              (mapcar (lambda (elt)
        		(let* ((key (car elt))
        		       (body (cdr elt))
        		       (num-agents (first (access body :num-agents)))
        		       (profit (round (* (first (access body :profit)) 10000)))
        		       (perception (round (* (first (access body :perception)) 100)))
        		       (fibo-strength (first (access key :fibo-strength)))
        		       (fibo-type (first (access key :fibo-type)))
        		       (pi-fibo (round-to (first (access body :pi-fibo)) 3))
        		       (action (first (access key :action)))
        		       (pi-action (round-to (first (access body :pi-action)) 3))
        		       )
        		  (format nil "* ~a agents — with an average profit of ~a units — perceived in ~a% of the market that a ~a resistance ~a — with a hesitancy of ~a — is a signal to ~a — with a hesitancy of ~a."
        			  num-agents
        			  profit
        			  perception
        			  (cond ((= fibo-strength 0) "weak")
        				((= fibo-strength 1) "moderate")
        				((= fibo-strength 2) "strong"))
        			  (cond ((= fibo-type 0) "above current price")
        				((= fibo-type 1) "nearby current price")
        				((= fibo-type 2) "below current price"))
        			  pi-fibo
        			  (cond ((= action 0) "sell")
        				((= action 1) "hold the current position")
        				((= action 2) "buy"))
        			  pi-action
        			  ))
        		)
        	      (sort (copy-seq (alexandria:hash-table-alist interprets))
        		    (lambda (elt1 elt2)
        		      (if (= (first (access elt1 :num-agents))
        			     (first (access elt2 :num-agents)))
        			  (> (first (access elt1 :profit))
        			     (first (access elt2 :profit)))
        			  (> (first (access elt1 :num-agents))
        			     (first (access elt2 :num-agents)))))))))))
;; (agents-describe (agents-best (agents-distribution *population*)))

(defun agents-table (agents-indexes)
  (let* ((fibos (mapcar (lambda (agent-index)
			  (agent-perception (extract-agent-from-pool agent-index)))
			agents-indexes))
         (sim (agents-indexes-simulation agents-indexes))
	 (agent-rules (mapcar (lambda (agent)
				(slot-value agent 'rules))
			      (extract-agents-from-pool agents-indexes)))
         (agents-profits (mapcar (lambda (index)
                                   (agents-profit (list index)))
                                 agents-indexes))
	 (agents-perception
          (let ((simp-fibos (mapcar (lambda (agent-data)
                                      (let* ((data-no-pi (mapcar (lambda (elt) (subseq elt 0 3)) agent-data))
                                             (mx (apply #'max (alexandria:flatten data-no-pi)))
                                             )
                                        (mapcar (lambda (datum)
                                                  (mapcar (lambda (d)
                                                            (let ((nd (/ d mx)))
                                                              (cond ((< nd (/ 1 3)) 0)
                                                                    ((< nd (/ 2 3)) 1)
                                                                    (t 2))))
                                                          datum))
                                                data-no-pi)
                                        ))
                                    fibos)))
            (mapcar (lambda (agent-fibos)
                      (let ((above-high 0) (level-high 0) (below-high 0)
                            (above-med 0) (level-med 0) (below-med 0)
                            (above-low 0) (level-low 0) (below-low 0)
                            (lfib (length agent-fibos)))
                        (mapcar (lambda (fibos)
                                  (cond ((= (first fibos) 0) (incf above-low))
                                        ((= (first fibos) 1) (incf above-med))
                                        ((= (first fibos) 2) (incf above-high)))
			    
                                  (cond ((= (second fibos) 0) (incf level-low))
                                        ((= (second fibos) 1) (incf level-med))
                                        ((= (second fibos) 2) (incf level-high)))
			    
                                  (cond ((= (third fibos) 0) (incf below-low))
                                        ((= (third fibos) 1) (incf below-med))
                                        ((= (third fibos) 2) (incf below-high)))
                                  )
                                agent-fibos)
                        (list (list (/ above-low lfib) (/ above-med lfib) (/ above-high lfib))
                              (list (/ level-low lfib) (/ level-med lfib) (/ level-high lfib))
                              (list (/ below-low lfib) (/ below-med lfib) (/ below-high lfib)))))
                    simp-fibos)
            )))
    (let ((interprets (make-hash-table :test #'equal)))
      (mapcar (lambda (agent-number agent-rules)
        	(let ((dups (make-hash-table :test #'equal)))
        	  (mapcar (lambda (agent-rule)
        		    (let ((simp-rule (mapcar (lambda (clause)
        					       (let ((b (apply #'beta clause))
        						     (action (cond ((< (first clause) 33) 0) ;; sell
        								   ((< (first clause) 66) 1) ;; buy or sell a little
        								   (t 2) ;; buy
        								   )))

        						 (list action b)
        						 ))
        					     agent-rule)))
        		      (mapcar (lambda (fibo-type clause)
        				;; buy; above; low fib
        				(let ((key (list `(:action ,(first (alexandria:last-elt simp-rule)))
        						 `(:fibo-type ,fibo-type)
        						 `(:fibo-strength ,(first clause))))
        				      (num-agents )
        				      (pi-fibo (second clause))
        				      (pi-action (second (alexandria:last-elt simp-rule)))
        				      (profit (cl21:nth agents-profits agent-number))
        				      (perception (cl21:nth (cl21:nth (cl21:nth agents-perception agent-number)
        								      fibo-type)
        							    (first clause))))
        				  (when (gethash key interprets) ;; already exists, I need to average
        				    (setq pi-fibo (/ (+ (first (access (gethash key interprets) :pi-fibo))
        							pi-fibo)
        						     2)
        					  pi-action (/ (+ (first (access (gethash key interprets) :pi-action))
        							  pi-action)
        						       2)
        					  profit (/ (+ (first (access (gethash key interprets) :profit))
        						       profit)
        						    2)
        					  perception (/ (+ (first (access (gethash key interprets) :perception))
        							   perception)
        							2)))
				 
        				  (if (gethash key dups)
        				      (setf (gethash key interprets)
        					    (list `(:num-agents ,(first (access (gethash key interprets) :num-agents)))
        						  `(:pi-fibo ,pi-fibo)
        						  `(:pi-action ,pi-action)
        						  `(:perception ,(float perception))
        						  `(:profit ,profit)))
        				      (progn
        					(setf (gethash key dups) t)
        					(setf (gethash key interprets)
        					      (list `(:num-agents ,(if (gethash key interprets)
        								       (1+ (first (access (gethash key interprets) :num-agents)))
        								       1))
        						    `(:pi-fibo ,pi-fibo)
        						    `(:pi-action ,pi-action)
        						    `(:perception ,(float perception))
        						    `(:profit ,profit)))))))
        			      (cl21:iota 3)
        			      (reverse (rest (reverse simp-rule))))))
        		  agent-rules)))
              (cl21:iota *population-size*)
              ;; (slot-value agents 'rules)
	      agent-rules
	      )
      (format nil "~{~a~^~%~}"
              (mapcar (lambda (elt)
        		(let* ((key (car elt))
        		       (body (cdr elt))
        		       (num-agents (first (access body :num-agents)))
        		       (profit (round (* (first (access body :profit)) 10000)))
        		       (perception (round (* (first (access body :perception)) 100)))
        		       (fibo-strength (first (access key :fibo-strength)))
        		       (fibo-type (first (access key :fibo-type)))
        		       (pi-fibo (round-to (first (access body :pi-fibo)) 3))
        		       (action (first (access key :action)))
        		       (pi-action (round-to (first (access body :pi-action)) 3))
        		       )
        		  (format nil "* ~a agents — with an average profit of ~a units — perceived in ~a% of the market that a ~a resistance ~a — with a hesitancy of ~a — is a signal to ~a — with a hesitancy of ~a."
        			  num-agents
        			  profit
        			  perception
        			  (cond ((= fibo-strength 0) "weak")
        				((= fibo-strength 1) "moderate")
        				((= fibo-strength 2) "strong"))
        			  (cond ((= fibo-type 0) "above current price")
        				((= fibo-type 1) "nearby current price")
        				((= fibo-type 2) "below current price"))
        			  pi-fibo
        			  (cond ((= action 0) "sell")
        				((= action 1) "hold the current position")
        				((= action 2) "buy"))
        			  pi-action
        			  ))
        		)
        	      (sort (copy-seq (alexandria:hash-table-alist interprets))
        		    (lambda (elt1 elt2)
        		      (if (= (first (access elt1 :num-agents))
        			     (first (access elt2 :num-agents)))
        			  (> (first (access elt1 :profit))
        			     (first (access elt2 :profit)))
        			  (> (first (access elt1 :num-agents))
        			     (first (access elt2 :num-agents)))))))))))

(defun agents-perceptions (agents-indexes)
  "Compiles a list of all the perceptions of the agents in `agents-indexes`."
  ;; (agents-perceptions (first *population*))
  (mapcar (lambda (agent-index)
	    (agent-perception (extract-agent-from-pool agent-index)))
	  agents-indexes))

(defun agents-revenue (agents-indexes)
  "Compiles a list of the revenues made by the agents in `agents-indexes` at
each point in the real prices."
  ;; (agents-revenue (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (revenue sim (get-deltas (length sim)))))

(defun agents-accuracy (agents-indexes &optional (mape-constraint nil))
  "Returns the number of correct direction predictions made by the agents in
`agents-indexes` for the real prices."
  ;; (agents-accuracy (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (accuracy sim (get-deltas (length sim)) mape-constraint)))

(defun agents-recall (agents-indexes)
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (recall sim (get-deltas (length sim)))))

(defun agents-precision (agents-indexes)
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (precision sim (get-deltas (length sim)))))

(defun agents-f1-score (agents-indexes)
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (f1-score sim (get-deltas (length sim)))))

(defun agents-pmape (agents-indexes &optional (zero-metric-constraint t))
  "Returns the penalized mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (pmape sim (get-deltas (length sim)) zero-metric-constraint)))

(defun agents-mape (agents-indexes &optional (zero-metric-constraint nil))
  "Returns the mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mape sim (get-deltas (length sim)) zero-metric-constraint)))

(defun agents-mase (agents-indexes &optional (zero-metric-constraint nil))
  "Returns the mean absolute scaled error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mase sim (get-deltas (length sim)) zero-metric-constraint)))

;; (agents-mape (second *population*))
;; (get-deltas 10)
;; (agents-indexes-simulation (first *population*))

(defun agents-rmse (agents-indexes)
  "Returns the root mean square error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (rmse sim (get-deltas (length sim)))))

(defun agents-mae (agents-indexes)
  "Returns the mean absolute error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mae sim (get-deltas (length sim)))))

(defun agents-mse (agents-indexes)
  "Returns the mean squared error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mse sim (get-deltas (length sim)))))

(defun plot-mfs ()
  "Returns an `alist` with the plots (y-values) of all the membership"
  ;; TODO: Maybe we should move this to OIntuition.
  ;; TODO: Or maybe move it to MLForecasting as a function that is calling stuff
  ;; TODO: from OIntuition.
  )

(defun beta (mu1 mu2 _pi)
  ;; (beta 99 90 0.5)
  (/ _pi (sqrt (1+ (abs (- mu1 mu2))))))

(defun interpret-indeterminacy (_pi mu1 mu2)
  ;; (interpret-indeterminacy 0.3 99 30)
  (let ((beta (beta _pi mu1 mu2)))
    (cond ((= beta 0) "without hesitancy")
	((< beta 0.25) "little hesitancy")
	((< beta 0.5) "some hesitancy")
	((< beta 1.5) "moderate hesitancy")
	((< beta 3.0) "considerable hesitancy")
	((< beta 5.0) "high hesitancy")
	((<= beta 100.0) "very high hesitancy")
	(t "unkown hesitancy"))))

(defun interpret-transaction (mean)
  (cond ((= mean 0) "totally sure of a downtrend")
	((< mean 20) "sell ")
	((< mean 0.1) "some hesitancy")
	((< mean 0.15) "moderate hesitancy")
	((< mean 0.20) "considerable hesitancy")
	((< mean 0.25) "high hesitancy")
	((<= mean 0.3) "very high hesitancy")
	(t "unkown hesitancy")))

(defun interpret-consequent (ifs)
  (let ((diff (abs (- (nth 0 ifs) (nth 1 ifs)))))
    diff))

(defun get-full-real-data (count)
  (reverse (subseq (reverse *rates*) 0 count)))

(defun get-accumulation (start vals)
  (let ((results `(,start)))
    (mapcar (lambda (val)
	   (push (+ (first results) val) results))
	 vals)
    (rest (reverse results))))

;; (get-deltas 10)
;; (get-closes 10)

(defun agent-uncertainty (agents)
  "Used for only one agent."
  (avg (mapcar (lambda (rule)
	      ;; (avg
	      ;;  (mapcar (lambda (r)
	      ;; 	      (second r))
	      ;; 	    rule))
	      (second (alexandria:last-elt rule))
	      )
	    (first (slot-value agents 'rules)))))

(defun insert-population (parent-id &optional label (fitness-fn #'mase) (sort-fn #'<))
       (let* ((best (agents-best (agents-distribution *population* fitness-fn sort-fn) sort-fn))
	      (id (uuid:make-v4-uuid)))
              (with-postgres-connection
    	          (execute (insert-into :populations
		             (set= :id id
			           :parent-id parent-id
			           :label label
			           :generations *generations*
			           :fitness-fn (format nil "~s" *fitness-fn*)
			           :population (compress-population *population*)
			           :best-index (position best *population* :test #'equalp)
			           :instrument (format nil "~s" *instrument*)
			           :timeframe (format nil "~s" *timeframe*)
			           :creation-time (local-time:timestamp-to-unix (local-time:now))
			           :begin (read-from-string (access (first *rates*) :time))
			           :end (read-from-string (access (alexandria:last-elt *rates*) :time))
			           :rules-config (compress-object *rules-config*)
			           :mape (float (agents-mape best))
				   :mase (float (agents-mase best))
			           :pmape (float (agents-pmape best))
			           :rmse (float (agents-rmse best))
				   :recall (float (agents-recall best))
				   :precision (float (agents-precision best))
				   :f1-score (float (agents-f1-score best))
			           :mae (float (agents-mae best))
			           :mse (float (agents-mse best))
			           :accuracy (agents-accuracy best nil)
			           :revenue (float (agents-revenue best))
			           ))))
              id))

(defun prune-populations ()
  "Sets the last child of every root node as the new roots."
  (dolist (id (get-root-populations-ids))
    (let* ((root-id (access id :id))
           (descendants (get-descendants root-id))
           (last-desc (when descendants (last-elt descendants))))
      (when descendants
        (with-postgres-connection
            ;; Setting last child to be orphan.
            (execute (update :populations
                       (set= :parent-id "")
                       (where (:= :id (access last-desc :id)))))
	  ;; Removing descendants but last.
          (dolist (desc (butlast descendants))
            (let ((desc-id (access desc :id)))
              ;; Removing entity.
              (execute (delete-from :populations
                         (where (:= :id desc-id)))
                       )
              ;; Removing closures.
              (execute (delete-from :populations-closure
                         (where (:or (:= :parent-id desc-id)
                                     (:= :child-id desc-id))))
                       )))
          ;; Removing root.
          (execute (delete-from :populations
                     (where (:= :id root-id))))
          ;; Removing root closures.
          (execute (delete-from :populations-closure
                     (where (:or (:= :parent-id root-id)
                                 (:= :child-id root-id)))))
          )))))
;; (prune-populations)

;; (get-root-populations-ids)
;; (with-postgres-connection
;;     (retrieve-all (select :* (from :populations-closure))))
;; (with-postgres-connection
;;     (retrieve-all (select :id (from :populations))))
;; (drop-populations)
;; (drop-tests)
;; (access (last-elt (get-descendants "0AE3D091-AED6-4C66-9498-F0E5647AA3F6")) :id)

;; Initializing Overmind Agents
;; (init *instrument* *timeframe*)


















(defun simple-moving-average (period sequence)
  "This function calculates the simple moving average (SMA) of a sequence."
  (assert (sigma/numeric:positive-integer? period))
  (assert (sigma/sequence:sequence? sequence))
  (loop for end from period to (length sequence) collect
        (/ (sigma/numeric:sum sequence :start (- end period) :end end)
           period)))

(sigma/control:function-alias 'simple-moving-average 'sma)
;; (org.tfeb.hax.memoize:memoize-function 'simple-moving-average)
;; (org.tfeb.hax.memoize:memoize-function 'sma)

(defun arithmetically-weighted-moving-average (period sequence)
  "This function calculates the arithmetically weighted moving average (WMA) of
a sequence, which is the specific WMA generally meant in technical analysis
whenever WMA is discussed."
  (assert (sigma/numeric:positive-integer? period))
  (assert (sigma/sequence:sequence? sequence))
  (loop for end from period to (length sequence) collect
        (/ (sigma/numeric:sum (mapcar #'*
                        ;; These are the weights.  The oldest data point is
                        ;; weighted at 1 and the newest at n for an n-period
                        ;; arithmetically weighted moving average.
                        (loop for i from 1 to period collect i)
                        (subseq sequence (- end period) end)))
           ;; The denominator is actually n+(n-1)+(n-2)+...+2+1, but this is a
           ;; triangular number, which can be determined by this formula.
           (* period (1+ period) 1/2))))

(sigma/control:function-alias 'arithmetically-weighted-moving-average 'awma)
;; (org.tfeb.hax.memoize:memoize-function 'arithmetically-weighted-moving-average)
;; (org.tfeb.hax.memoize:memoize-function 'awma)

;; (awma 2 '(1 2 3 4))

(defun exponential-moving-average (period sequence)
  "This function calculates the exponential moving average (EMA) of a sequence."
  ;;; XXX: This is too slow!
  (assert (sigma/numeric:positive-integer? period))
  (assert (sigma/sequence:sequence? sequence))
  (let ((smoothing-factor (/ 2 (1+ period)))) ; This is alpha.
    (loop for end from period to (length sequence) collect
          ;; This should actually be divided by 1+(1-a)+(1-a)**2+... as an
          ;; infinite summation, but this approaches 1/a.
          (* smoothing-factor
             (sigma/numeric:sum (mapcar #'*
                          ;; These are the weights.
                          (loop for e from 0 to (length sequence) collect
                                (expt (1- smoothing-factor) e))
                          (subseq sequence (- end period) end)))))))

(sigma/control:function-alias 'exponential-moving-average 'ema)
;; (org.tfeb.hax.memoize:memoize-function 'exponential-moving-average)
;; (org.tfeb.hax.memoize:memoize-function 'ema)

(defun moving-average-convergence-divergence (period-a period-b sequence)
  "This is the moving average convergence/divergence (MACD) of a sequence."
  (mapcar #'- (ema period-a sequence) (ema period-b sequence)))

(sigma/control:function-alias 'moving-average-convergence-divergence 'macd)
;; (org.tfeb.hax.memoize:memoize-function 'moving-average-convergence-divergence)
;; (org.tfeb.hax.memoize:memoize-function 'macd)

(defun macd-signal (smoothing-period period-a period-b sequence)
  "This is the MACD of a sequence with an EMA applied to smooth the results.
It is routinely used as a signal/trigger."
  (ema smoothing-period (macd period-a period-b sequence)))

;; (org.tfeb.hax.memoize:memoize-function 'macd-signal)

(defun upward-changes (sequence)
  (mapcar #'(lambda (yesterday today)
              (if (< yesterday today) ; Are we upward?
                (- today yesterday)
                0))
          sequence
          (rest sequence)))

(defun downward-changes (sequence)
  (mapcar #'(lambda (yesterday today)
              (if (> yesterday today) ; Are we downward?
                (- yesterday today)
                0))
          sequence
          (rest sequence)))

(defun relative-strength (moving-average sequence)
  (mapcar #'/
          (funcall moving-average (upward-changes sequence))
          (funcall moving-average (downward-changes sequence))))

(sigma/control:function-alias 'relative-strength 'rs)

(defun ema-rs (period sequence)
  (relative-strength (curry #'ema period) sequence))

(sigma/control:function-alias 'ema-rs 'wilder-rs)

(defun ema-rs-27 (sequence)
  "This is the RS that Wilder recommended originally."
  (ema-rs 27 sequence))

(sigma/control:function-alias 'ema-rs-27 'rs-textbook)

(defun sma-rs (period sequence)
  (relative-strength (curry #'ema period) sequence))

(sigma/control:function-alias 'sma-rs 'cutler-rs)

(defun relative-strength-index (rs sequence)
  (- 100 (* 100 (/ (1+ (funcall rs sequence))))))

(sigma/control:function-alias 'relative-strength-index 'rsi)

(defun ema-rsi (period sequence)
  (rsi (curry #'ema-rs period) sequence))

(sigma/control:function-alias 'ema-rsi 'wilder-rsi)

(defun ema-rsi-27 (sequence)
  (rsi #'ema-rs-27 sequence))

(sigma/control:function-alias 'ema-rsi-27 'rsi-textbook)

(defun sma-rsi (period sequence)
  (rsi (curry #'ema-rs period) sequence))

(sigma/control:function-alias 'sma-rsi 'cutler-rsi)

(defun simple-directional-signal (period sequence)
  (assert (sigma/numeric:positive-integer? period))
  (mapcar #'< sequence (nthcdr period sequence)))

(defun simple-directional-gains (period sequence)
  (let* ((signals (simple-directional-signal period sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun simple-directional-performance (period sequence)
  (product (simple-directional-gains period sequence)))

(defun dual-simple-directional-signal (period-a period-b sequence)
  (reverse (mapcar #'(lambda (a b) (and a b))
                   (reverse (simple-directional-signal period-a sequence))
                   (reverse (simple-directional-signal period-b sequence)))))

(defun dual-simple-directional-gains (period-a period-b sequence)
  (let* ((signals (dual-simple-directional-signal period-a period-b sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun dual-simple-directional-performance (period-a period-b sequence)
  (product (dual-simple-directional-gains period-a period-b sequence)))

(defun sma-crossover-signal (short-period long-period ratio sequence)
  (assert (sigma/numeric:positive-integer? short-period))
  (assert (sigma/numeric:positive-integer? long-period))
  (assert (< short-period long-period))
  (let* ((short-sma (sma short-period sequence))
         (long-sma (sma long-period sequence))
         (start (- (length short-sma) (length long-sma))))
    (mapcar #'(lambda (short long)
                (< (* ratio long) short))
            (nthcdr start short-sma)
            long-sma)))

(defun directional-sma-crossover-signal
  (short-period long-period ratio sequence)
  (assert (sigma/numeric:positive-integer? short-period))
  (assert (sigma/numeric:positive-integer? long-period))
  (assert (< short-period long-period))
  (let* ((short-sma (sma short-period sequence))
         (long-sma (sma long-period sequence))
         (start (1+ (- (length short-sma) (length long-sma)))))
    (mapcar #'(lambda (short long previous-long)
                (cond ((and (< (* ratio long) short)
                            (< previous-long long))
                       :long)
                      ((and (> (/ long ratio) short)
                            (> previous-long long))
                       :short)
                      (t :cash)))
            (nthcdr start short-sma)
            (cdr long-sma)
            long-sma)))

(defun sma-crossover-gains (short-period long-period ratio sequence)
  (let* ((signals (sma-crossover-signal short-period long-period ratio
                                        sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  1))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun directional-sma-crossover-gains (short-period long-period ratio sequence)
  (let* ((signals (directional-sma-crossover-signal
                    short-period long-period ratio sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (case signal
                  (:long (/ tomorrow today))
                  (:cash 1)
                  (:short (/ today tomorrow))))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun sma-crossover-long/short-gains (short-period long-period ratio sequence)
  (let* ((signals (sma-crossover-signal short-period long-period ratio
                                        sequence))
         (start (- (length sequence) (length signals))))
    (mapcar #'(lambda (today tomorrow signal)
                (if signal
                  (/ tomorrow today)
                  (/ today tomorrow)))
            (nthcdr start sequence)
            (nthcdr (1+ start) sequence)
            signals)))

(defun sma-crossover-performance (short-period long-period ratio sequence)
  (product (sma-crossover-gains short-period long-period ratio sequence)))

(defun directional-sma-crossover-performance
  (short-period long-period ratio sequence)
  (product (directional-sma-crossover-gains
             short-period long-period ratio sequence)))

(defun sma-crossover-long/short-performance
  (short-period long-period ratio sequence)
  (product (sma-crossover-long/short-gains short-period long-period ratio
                                           sequence)))

(defun sma-crossover-optimal (sequence)
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance (sma-crossover-performance short-period
                                                                    long-period
                                                                    ratio
                                                                    sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (SMA-CROSSOVER ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun sma-crossover-long/short-optimal (sequence)
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance
                              (sma-crossover-long/short-performance
                                short-period long-period ratio sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (SMA-CROSSOVER-LONG/SHORT ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun directional-sma-crossover-optimal (sequence)
  (let ((optimal-short-period nil)
        (optimal-long-period nil)
        (optimal-ratio nil)
        (optimal-performance nil)
        (b&h-performance (buy-and-hold-performance sequence)))
    (loop for ratio from 1.00 to 1.05 by 0.01 do
          (loop for long-period from 2 to 200 do
                (loop for short-period from 1 to (1- long-period) do
                      (let ((performance
                              (directional-sma-crossover-performance
                                short-period long-period ratio sequence)))
                        (when (or (null optimal-performance)
                                (< optimal-performance performance))
                          (format t "New optimal: (DIRECTIONAL-SMA-CROSSOVER ~A ~A ~A) => ~A ~A B&H => ~A.~%"
                                  short-period long-period ratio
                                  performance
                                  (cond ((< performance b&h-performance) "<")
                                        ((= performance b&h-performance) "=")
                                        ((> performance b&h-performance) ">"))
                                  b&h-performance)
                          (setf optimal-short-period short-period
                                optimal-long-period long-period
                                optimal-ratio ratio
                                optimal-performance performance))))))
    (list optimal-short-period optimal-long-period optimal-ratio
          optimal-performance)))

(defun fibs (low high)
  (let* ((phi (/ (1+ (sqrt 5)) 2))
         (inverse-phi (/ phi))
         (range (- high low))
         (r/2 (/ range 2))
         (mid (+ low r/2))
         (mid+z (+ low (* inverse-phi range)))
         (z (- mid+z mid)))
    (format t "Range: ~$ ~$~%Low: ~$ ~$ ~$~%Mid: ~$ ~$ ~$~%High: ~$ ~$ ~$"
            range r/2
            (- low z) low (+ low z)
            (- mid z) mid mid+z
            (- high z) high (+ high z))))

(defun fibonacci (table &key (key 'adjusted-closing-price))
  (fibs (reduce #'min (funcall key table))
        (reduce #'max (funcall key table))))
