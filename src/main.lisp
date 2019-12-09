;; (ql:quickload :overmind-agents)
;; (ql:quickload :overmind-intuition)
;; (ql:quickload :lparallel)
;; (ql:quickload :random-state)
;; (ql:quickload :cl21)
;; (ql:quickload :marshal)
;; (ql:quickload :cl-json)
;; (ql:quickload :dexador)
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
	))
(in-package :overmind-agents)
(progn
  (defparameter *instrument* :US30_USD
    "The financial instrument used for querying the data used to train or test.")
  (defparameter *timeframe* :D
    "The timeframe used for querying the data used to train or test.")
  (defparameter *all-rates* (get-rates *instrument* 2 *timeframe*)
    "All the rates. Subsets are used during training, validation and testing stages."))

;; (length *all-rates*)

;; (setf *all-rates*
;;       (mapcar (lambda (tuple)
;; 		`((:time . ,(format nil "~a" (local-time:timestamp-to-unix (local-time:parse-timestring (nth 0 tuple)))))
;; 		  (:close-bid . ,(read-from-string (nth 4 tuple))))
;; 		)
;; 	      (cl-csv:read-csv #P"~/skycoin.csv" :separator #\Space)))

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
		    (pmape :type '(:numeric)
			   :not-null t)
                    (mae :type '(:numeric)
                         :not-null t)
                    (mse :type '(:numeric)
                         :not-null t)
                    (rmse :type '(:numeric)
                          :not-null t)
                    (corrects :type '(:numeric[2])
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
                  (mape :type '(:numeric)
                        :not-null t)
		  (pmape :type '(:numeric)
			   :not-null t)
                  (mae :type '(:numeric)
                       :not-null t)
                  (mse :type '(:numeric)
                       :not-null t)
                  (rmse :type '(:numeric)
                        :not-null t)
                  (corrects :type '(:numeric[2])
                            :not-null t)
                  (revenue :type '(:numeric)
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
    (setf *begin* (position (access retrieved-pop :begin)
			    *all-rates* :key (lambda (rate)
					       (read-from-string (access rate :time)))))
    (setf *end* (1+ (position (access retrieved-pop :end)
			      *all-rates* :key (lambda (rate)
						 (read-from-string (access rate :time))))))
    (setf *rates* (subseq *all-rates* *begin* *end*))
    ;; Setting `*timeframe*` and `*instrument*`.
    (setf *timeframe* (read-from-string (access retrieved-pop :timeframe)))
    (setf *instrument* (read-from-string (access retrieved-pop :instrument)))
    ;; Resetting `*cached-agents*`, as these most likely are useless now.
    ;; (setf *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t))
    (setf *generations* (access:access retrieved-pop :generations))
    (let ((fit-fn (read-from-string (access:access retrieved-pop :fitness-fn))))
      (cond ((eq fit-fn :mse)
             (setf *fitnesses* (list (access:access retrieved-pop :mse))))
            ((eq fit-fn :mae)
             (setf *fitnesses* (list (access:access retrieved-pop :mae))))
            ((eq fit-fn :mape)
             (setf *fitnesses* (list (access:access retrieved-pop :mape))))
	    ((eq fit-fn :pmape)
             (setf *fitnesses* (list (access:access retrieved-pop :pmape))))
            ((eq fit-fn :rmse)
             (setf *fitnesses* (list (access:access retrieved-pop :rmse))))
            ((eq fit-fn :corrects)
             (setf *fitnesses* (list (access:access retrieved-pop :corrects))))
            ((eq fit-fn :revenue)
             (setf *fitnesses* (list (access:access retrieved-pop :revenue))))
            (t ;; default
             (setf *fitnesses* nil))))
    ;; Finding out what are the unique agents, as agents can be
    ;; repeated in a population's communities.
    (mapcar (lambda (elt)
	      (pushnew elt unique-agents :test #'equal))
	    (flatten pop))
    ;; Setting parameters according to what is set in the stored population.
    (setf *community-size* (length (first pop)))
    (setf *population-size* (length pop))
    (setf *num-rules* (/ (length (first (slot-value (first (first pop)) 'rules))) 2))
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
  ;; (get-descendants 2)
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

(defun check-zero-metric (sim real)
  (if (< (mean (mapcar (lambda (s r)
			 (abs (/ (- r s) (1+ r))))
		       sim real))
  	 0.001)
      t))

(defun pmape (sim real &optional (zero-metric-constraint nil))
  "Penalized MAPE. If a simulation has a different direction than that of the real price, it gets penalized with a weight."
  (let* ((rmax (apply #'max real))
	 (rmin (apply #'min real)))
    (/ (reduce #'+ (mapcar (lambda (s r)
			     (if (>= (* r s) 0)
				 (abs (/ (- r s) (1+ (abs r))))
				 (* 3
				    (abs (/ (- r s) (1+ (abs r)))))))
			   (scale-sim sim rmin rmax)
			   real))
       (length real))))

(defun mape (sim real &optional (zero-metric-constraint nil))
  "Mean absolute percentage error between a simulated time series `sim` and a real time series `real`."
  (if (and zero-metric-constraint
	   ;; (check-zero-metric sim real)
	   )
      most-positive-fixnum
      (/ (reduce #'+ (mapcar (lambda (s r)
			       (/ (abs (- r s)) (1+ (abs r))))
			     sim
                             real
			     ;; (rest real)
                             ))
	 ;; (length (rest real))
         (length real)
         )))

(defun mape (sim real &optional (zero-metric-constraint nil))
  "Mean absolute percentage error between a simulated time series `sim` and a real time series `real`.
This version scales the simulation."
  (let ((sim (extract-training-clustered-trades sim))
	(real (extract-training-clustered-trades real)))
    (let* ((rmax (apply #'max real))
	   (rmin (apply #'min real)))
      (/ (reduce #'+ (mapcar (lambda (s r)
			       (/ (abs (- r s)) (1+ (abs r))))
			     (scale-sim sim rmin rmax)
			     real
			     ))
	 (length real)
	 ))))

;; (check-zero-metric '(1.001 1.999 1.001 1.999 1.001 1.999)
;; 		   '(1 2 1 2 1 2))

(defun mse (sim real)
  "Mean squared error between a simulated time series `sim` and a real time series `real`."
  (let* ((rmax (apply #'max real))
	 (rmin (apply #'min real)))
    (/ (reduce #'+ (mapcar (lambda (elt) (expt elt 2)) (mapcar #'- (scale-sim sim rmin rmax) real)))
       (length real))))

(defun mae (sim real)
  "Mean absolute error between a simulated time series `sim` and a real time
series `real`."
  (let* ((rmax (apply #'max real))
	 (rmin (apply #'min real)))
    (/ (reduce #'+ (mapcar (lambda (s r)
			     (abs (- s r)))
			   (scale-sim sim rmin rmax)
			   real))
       (length real))))

(defun rmse (sim real)
  "Root mean square error between a simulated time series `sim` and a real time
series `real`."
  (sqrt (mse sim real)))

(defun revenue (sim real)
  "Currently it returns the agent profit at each trade."
  (let (;; (real (rest real))
	;; we only need the real previous, as the simulated is based on the last real price at every moment
	;; (prev (- (second real) (first real)))
	(prev (first real)))
    (/ (apply #'+
              (mapcar (lambda (s r)
                        (let ((real-dir (- r prev))
                              (sim-dir (- s prev)))
                          (setq prev r)
                          (if (> (* real-dir sim-dir) 0)
                              (abs real-dir)
                              (* -1 (abs real-dir))))
                        )
                      sim
                      real))
       (length real))))

;; (float 399/500)
;; (corrects '(3 8 2 5) '(0 10 5 0) nil)
;; (agent-trades (extract-agent-from-pool (first (agents-best (agents-distribution *population*)))))

;; (map nil #'print (agents-indexes-simulation (agents-best (agents-distribution *population*))))
;; (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; ;; Get agent trades.
;; ;; (agents-corrects (agents-best (agents-distribution *population*)) nil)
;; (map nil 'print
;;      (apply #'mapcar (lambda (&rest nums)
;; 		       (apply #'+ nums))
;; 	    (mapcar (lambda (agent)
;; 		      (agent-trades agent))
;; 		    (extract-agents-from-pool
;; 		     (agents-best (agents-distribution *population*))))))

;; (mapcar #'+
;; 	(apply #'mapcar (lambda (&rest nums)
;; 			  (apply #'+ nums))
;; 	       (mapcar (lambda (agent)
;; 			 (agent-trades agent))
;; 		       (extract-agents-from-pool
;; 			(agents-best (agents-distribution *population*)))))
;; 	(get-real-data *data-count*))



;; (map nil 'print
;;      (mapcar (lambda (s r)
;; 	       (- s r))
;; 	     (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; 	     (get-real-data 501)))

(defun corrects (sim real &optional (mape-constraint nil))
  "How many trades were correct."
  ;; (corrects (agents-indexes-simulation (agents-best (agents-distribution *population*))) (get-real-data))
  (let ((sim (extract-training-clustered-trades sim))
	(real (extract-training-clustered-trades real)))
    (let* ((rmax (apply #'max real))
	   (rmin (apply #'min real))
	   (sim (scale-sim sim rmin rmax)))
      (/ (apply #'+
		(mapcar (lambda (s r)
			  (if (> (* s r) 0)
			      1
			      0))
			sim real))
	 (if mape-constraint
	     (* (length real) (mape sim real nil))
	     (length real))))))

(defun corrects (sim real &optional (mape-constraint nil))
  "How many trades were correct."
  ;; (corrects (agents-indexes-simulation (agents-best (agents-distribution *population*))) (get-real-data))
  (let* ((trades-count 0)
	 (trades-sum (apply #'+
			    (mapcar (lambda (s r)
				      (when (/= s 0)
					(incf trades-count))
				      (if (> (* s r) 0)
					  1
					  0))
				    sim real))))
    ;; (if (> trades-count 0)
    ;; 	(/ trades-sum
    ;; 	   trades-count)
    ;; 	0)
    ;; (if (> trades-count 0)
    ;; 	(format nil "~a/~a (~$%)" trades-sum trades-count (float (* 100 (/ trades-sum trades-count))))
    ;; 	"0/0 (0.0)")
    (if (> trades-count 0)
	(make-array 2 :initial-contents `(,trades-sum ,trades-count))
    	#(0 0))
    ))

;; (corrects '(9.5 9.7 9.5 9.4) '(10 9 10 9 10))

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
		(mapcar (lambda (outer-val)
			  (cons outer-val
				inner-val))
			(car lists)))
	      (apply #'combinations (cdr lists)))))

;; (defun shuffle (sequence)
;;   (loop for i from (length sequence) downto 2
;;      do (rotatef (elt sequence (random-int *rand-gen* 0 (1- i)))
;;                  (elt sequence (1- i))))
;;   sequence)

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

(defun get-cluster-data-indexes (agent &optional (perception-fn #'agent-perception))
  (let ((data (funcall perception-fn agent)))
    ))

(defun get-cluster-data (&optional (perception-fn #'agent-perception))
  (let ((data (mapcar #'butlast (apply #'concatenate 'list (mapcar #'butlast (mapcar perception-fn (flatten (extract-agents-from-pool (first *population*)))))))))
    (mapcar #'centroid (km data *agents-cluster-size*))))

(defun get-closest-cluster (inputs cluster-centroids)
  (last-elt
   (largest-number-indexes
    (mapcar (lambda (centroid)
	      (mape inputs centroid nil))
	    cluster-centroids))))

;; (get-closest-cluster '(3689/285 1541/95 1315/57 9074/285 3327/95 2818/95 6167/285) (get-cluster-data))
;; (mapcar #'float (get-cluster-data))

(defun agent-perception (agent)
  "Generates the fibos of a market according to `agent`'s beliefs."
  ;; (agent-perception (extract-agent-from-pool 1))
  (mapcar (lambda (heat)
            (let ((heat (cl21:gethash heat :heat))
                  (close (cl21:gethash heat :close)))
              (let* ((z (cl21:gethash heat :z))
                    (index (alexandria:if-let
                               ((res (search `(,close) (cl21:gethash heat :y)
                                             :key (lambda (elt)
                                                    (when (> close elt) t)))))
                             res
                             (1- (length (cl21:gethash heat :y)))
                             ))
		    (res (append (mapcar (lambda (i)
					   (cl21:nth z (if (< (- index (- *num-inputs* i 1)) 0)
							   i
							   (- index (- *num-inputs* i 1)))))
					 (iota (1- *num-inputs*)))
				 (list (cl21:nth z index)))))
		;; (list (cl21:nth z (if (< (- index 8) 0) 0 (- index 8)))
		;;       (cl21:nth z (if (< (- index 7) 0) 1 (- index 7)))
		;;       (cl21:nth z (if (< (- index 6) 0) 2 (- index 6)))
		;;       (cl21:nth z (if (< (- index 5) 0) 3 (- index 5)))
		;;       (cl21:nth z (if (< (- index 4) 0) 4 (- index 4)))
		;;       (cl21:nth z (if (< (- index 3) 0) 5 (- index 3)))
		;;       (cl21:nth z (if (< (- index 2) 0) 6 (- index 2)))
		;;       (cl21:nth z (if (< (- index 1) 0) 7 (- index 1)))
		;;       (cl21:nth z index)
		;;       close
		;;       )

		(append (let ((mx (apply #'max res)))
			  (mapcar (lambda (r)
				    ;;(* 100 (/ r mx))
				    r
				    )
				  res))
			(list close))
                )))
          ;; TODO: generalize (get-data) so it doesn't need an `instrument`.
          (get-data *instrument* *rates* :levels (slot-value agent 'beliefs))))

;; (agent-perception (extract-agent-from-pool 1))

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
  ;; (time (agent-trades (first (extract-agents-from-pool '(2)))))
  (let ((sig (reduce #'+
		     (append (flatten (slot-value agent 'beliefs))
			     (flatten (slot-value agent 'rules))
			     (flatten (slot-value agent 'activations))
			     (get-real-data-deltas 10)
                             ))))
    (if (gethash sig *cached-agents*)
	(gethash sig *cached-agents*)
	;; (mapcar (lambda (trade)
	;; 	  trade
	;; 	  ;; (process-agent-output trade
	;; 	  ;; 			(slot-value agent 'leverage))
	;; 	  ;; (let ((leverages (slot-value agent 'leverage)))
	;; 	  ;;   (mean (mapcar (lambda (trade leverage)
	;; 	  ;; 		    (process-agent-output trade
	;; 	  ;; 					  leverage))
	;; 	  ;; 		  trades
	;; 	  ;; 		  leverages)))
	;; 	  ;; (let ((leverage (slot-value agent 'leverage)))
	;; 	  ;;   (mean (mapcar (lambda (trade)
	;; 	  ;; 		    (process-agent-output trade
	;; 	  ;; 					  leverage))
	;; 	  ;; 		  trades)))
	;; 	  )
	;; 	(gethash sig *cached-agents*))
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
	    ;; (sb-ext:with-locked-hash-table (*cached-agents*)
	    ;;   (setf (gethash sig *cached-agents*) sim))
	    (setf (gethash sig *cached-agents*) sim)
	    ;; (mapcar (lambda (trade)
	    ;; 	      (process-agent-output trade
	    ;; 	      			    (slot-value agent 'leverage))
	    ;; 	      ;; (let ((leverages (slot-value agent 'leverage)))
	    ;; 	      ;; 	(mean (mapcar (lambda (trade leverage)
	    ;; 	      ;; 			(process-agent-output trade
	    ;; 	      ;; 					      leverage))
	    ;; 	      ;; 		      trades
	    ;; 	      ;; 		      leverages)))
	    ;; 	      ;; (let ((leverage (slot-value agent 'leverage)))
	    ;; 	      ;; 	(mean (mapcar (lambda (trade)
	    ;; 	      ;; 			(process-agent-output trade
	    ;; 	      ;; 					      leverage))
	    ;; 	      ;; 		      trades)))
	    ;; 	      )
	    ;; 	    sim)
	    sim
	    )))))

(defun agents-fitness (agents-indexes &optional (fitness-fn #'mape))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    ;; (mse sim (get-real-data 3))
    (funcall fitness-fn sim (get-real-data-deltas (length sim)))))

(defun ifis-agents (inputs rules trade-scale leverage stdev)
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par stdev))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (apply #'max (butlast inp))))
		(if-coa
		 (reduce #'ifintersection
			 (mapcar (lambda (i)
				   (let ((ifs (nth i agent-ifss)))
				     (reduce #'ifunion
					     (mapcar (lambda (j)
						       (rule
							(* 100 (/ (nth i inp) (+ 1 mx-inp)))
							;; (nth i inp)
							(nth (* j 2) ifs)
							(nth (1+ (* j 2)) ifs)))
						     (iota (floor (/ (length ifs) 2)))))))
				 (iota (length (butlast inp)))))
		 )))
	    inputs)))

(defun ifis-agents (inputs rules stdev)
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par stdev))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (apply #'max (butlast inp))))
		(mapcar #'if-coa
			(mapcar (lambda (i)
				  (let ((ifs (nth i agent-ifss)))
				    (reduce #'ifunion
					    (mapcar (lambda (j)
						      (rule
						       (* 100 (/ (nth i inp) (+ 1 mx-inp)))
						       ;; (nth i inp)
						       (nth (* j 2) ifs)
						       (nth (1+ (* j 2)) ifs)))
						    (iota (floor (/ (length ifs) 2)))))))
				(iota (length (butlast inp)))))))
	    inputs)))

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

(defun ifis-agents (inputs rules stdev)
  "This one considers an activation function and multiple agent profiles."
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par stdev))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let* ((mx-inp (apply #'max (butlast inp)))
		     (act (eval-rules inp mx-inp agent-ifss 0)))
		(cond ((< act 10) (eval-rules inp mx-inp agent-ifss 1))
		      ((< act 20) (eval-rules inp mx-inp agent-ifss 2))
		      ((< act 30) (eval-rules inp mx-inp agent-ifss 3))
		      ((< act 40) (eval-rules inp mx-inp agent-ifss 4))
		      ((< act 50) (eval-rules inp mx-inp agent-ifss 5))
		      ((< act 60) (eval-rules inp mx-inp agent-ifss 6))
		      ((< act 70) (eval-rules inp mx-inp agent-ifss 7))
		      ((< act 80) (eval-rules inp mx-inp agent-ifss 8))
		      ((< act 90) (eval-rules inp mx-inp agent-ifss 9))
		      ;; Agent won't trade then.
		      (t 0))
		))
	    inputs)))

(defun ifis-agents (inputs rules
		    input-min input-max output-min output-max)
  "This one considers clustered perceptions and deltas."
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (i par)
					(if (evenp i)
					    (agents-ifs par input-min input-max)
					    (agents-ifs par output-min output-max))
					)
				      (iota (length params))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (apply #'max (butlast inp))))
		(if-coa
		 (reduce #'ifintersection
			 (mapcar (lambda (i)
				   (let ((ifs (nth i agent-ifss)))
				     (reduce #'ifunion
					     (mapcar (lambda (j)
						       (rule
							;; (* 100 (/ (nth i inp) (+ 1 mx-inp)))
							(nth i inp)
							(nth (* j 2) ifs)
							(nth (1+ (* j 2)) ifs)))
						     (iota (floor (/ (length ifs) 2)))))))
				 (iota (length (butlast inp))))))
		))
	    inputs)))

(defun ifis-agents (inputs rules stdev)
  "This one uses ifintersection."
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par stdev))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (apply #'max (butlast inp))))
		(if-coa
		 (reduce #'ifintersection
			 (mapcar (lambda (i)
				   (let ((ifs (nth i agent-ifss)))
				     (reduce #'ifunion
					     (mapcar (lambda (j)
						       (rule
							;; (* 100 (/ (nth i inp) (+ 1 mx-inp)))
							(nth i inp)
							(nth (* j 2) ifs)
							(nth (1+ (* j 2)) ifs)))
						     (iota (floor (/ (length ifs) 2)))))))
				 (iota (length (butlast inp))))))
		))
	    inputs)))

(defun ifis-agents (inputs rules activations activation-threshold)
  "This one considers an activation function."
  (let* ((agent-ifss (mapcar (lambda (params)
			       (mapcar (lambda (par)
					 (agents-ifs par))
				       params))
			     rules))
	 ;; (std-devs (mapcar (lambda (inp)
	 ;; 		     (standard-deviation
	 ;; 		      (mapcar (lambda (i)
	 ;; 				(second
	 ;; 				 (membership (nth i inp)
	 ;; 					     (agents-ifs (nth i activations)))))
	 ;; 			      (iota *num-inputs*))))
	 ;; 		   inputs))
	 (means (mapcar (lambda (inp)
			  (reduce #'+
				  (mapcar (lambda (i)
					    (second
					     (membership (nth i inp)
							 (agents-ifs (nth i activations)))))
					  (iota *num-inputs*))))
			inputs))
	 ;; (std-devs-mean (mean std-devs))
	 ;; (std-devs-stdev (standard-deviation std-devs))
	 ;; (min-stdev (apply #'min std-devs))
	 ;; (max-mean-idx (nth 5 (largest-number-indexes means)))
	 ;; (mean-max (apply #'max means))
	 )
    (mapcar (lambda (j inp)
	      (if (>= ;; (nth j std-devs)
		   (nth j means)
		   activation-threshold
		   ;; mean-max
		   ;; (nth max-mean-idx means)
		      ;; (print std-devs-stdev)
		      ;; (print (/ std-devs-mean 1))
		      ;; (- std-devs-mean (* 3 std-devs-stdev))

		      )
		  (if-coa
		   (reduce #'ifintersection
			   (mapcar (lambda (i)
				     (let ((ifs (nth i agent-ifss)))
				       (reduce #'ifunion
					       (mapcar (lambda (j)
							 (rule
							  ;; (* 100 (/ (nth i inp) (+ 1 mx-inp)))
							  (nth i inp)
							  (nth (* j 2) ifs)
							  (nth (1+ (* j 2)) ifs)))
						       (iota (floor (/ (length ifs) 2)))))))
				   (iota (length (butlast inp))))))
		  0))
	    (iota (length inputs))
	    inputs)))

(defun agents-distribution (population &optional (fitness-fn #'mape) (sort-fn #'<))
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

(defun agents-reproduce (&optional (fitness-fn #'mape) (sort-fn #'<))
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
;;                  :performance-metrics :corrects))

;; (defparameter *test-agents* (gen-agents 200))
;; (setf *agents-pool* *test-agents*)
;; (setf *population* (list (first (first (agents-err-adjust *test-agents* (- omper:*data-count* 3))))))
;; (agents-err-adjust *test-agents* 60)
;; (let ((sim (agents-simulation (mapcar (lambda (idx)
;;                                         (nth idx *test-agents*))
;;                                       '(67 74)))))
;;   (float (corrects sim (get-real-data (length sim)))))

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

(defun gen-rules (num-rules)
  ;; (gen-rules 2)
  (mapcar (lambda (_)
	    (mapcar (lambda (_) `(,(random-float *rand-gen* 0 100)
				   ,(random-float *rand-gen* 0 100)
				   ,(random-float *rand-gen* 0 1)
				   ;; 0
				   ))
		    (cl21:iota (* 2 num-rules))))
	  (cl21:iota *num-inputs*)))

(defun gen-activations (max-num-rules)
  ;; (gen-rules 1)
  (let* ((percs (flatten (mapcar #'butlast (funcall *perception-fn* nil))))
	 (reals (get-real-data-deltas omper:*data-count*))
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

(defun gen-rules (max-num-rules)
  ;; (gen-rules 1)
  (let* ((percs (flatten (mapcar #'butlast (funcall *perception-fn* nil))))
	 (reals (get-real-data-deltas omper:*data-count*))
	 (mn-perc (apply #'min percs))
	 (mx-perc (apply #'max percs))
	 (mn-real (apply #'min reals))
	 (mx-real (apply #'max reals))
	 (inp-sd (/ (/ (- mx-perc mn-perc) max-num-rules) (* 2 2.355)))
	 (out-sd (/ (/ (- mx-real mn-real) max-num-rules) (* 2 2.355))))
    (mapcar (lambda (_)
	      (let* ( ;; Number of antecedent and consequent membership functions
		     ;; that we'll consider.
		     ;; (num-mfs (random-int *rand-gen* 1 max-num-rules))
		     (num-mfs max-num-rules)
		     ;; Number of antecedent and consequent non-membership functions
		     ;; that we'll consider.
		     ;; (num-nmfs (random-int *rand-gen* 1 max-num-rules))
		     (num-nmfs max-num-rules)
		     ;; Separation between membership functions.
		     (inp-sep (/ (+ (- mx-perc mn-perc) inp-sd) max-num-rules))
		     (out-sep (/ (+ (- mx-real mn-real) out-sd) max-num-rules))
		     ;; Random starting point for the first membership function of the
		     ;; antecedents.
		     (armf (random-float *rand-gen* mn-perc (+ mn-perc inp-sep)))
		     ;; Random starting point for the first non-membership function of the
		     ;; antecedents.
		     (arnmf (random-float *rand-gen* mn-perc (+ mn-perc inp-sep)))
		     ;; Random starting point for the first membership function of the
		     ;; consequents.
		     (crmf (random-float *rand-gen* mn-real (+ mn-real out-sep)))
		     ;; Random starting point for the first non-membership function of the
		     ;; consequents.
		     (crnmf (random-float *rand-gen* mn-real (+ mn-real out-sep)))
		     (amf-means (subseq (shuffle
					 (mapcar (lambda (i)
						   (+ (- (* i inp-sep) inp-sd) armf))
						 (iota max-num-rules)))
					0 num-mfs))
		     (anmf-means (subseq
				  (shuffle
				   (mapcar (lambda (i)
					     (+ (- (* i inp-sep) inp-sd) arnmf))
					   (iota max-num-rules)))
				  0 num-mfs))
		     (cmf-means (subseq
				 (shuffle
				  (mapcar (lambda (i)
					    (+ (- (* i out-sep) out-sd) crmf))
					  (iota max-num-rules)))
				 0 num-mfs))
		     (cnmf-means (subseq
				  (shuffle
				   (mapcar (lambda (i)
					     (+ (- (* i out-sep) out-sd) crnmf))
					   (iota max-num-rules)))
				  0 num-mfs)))
		(apply #'nconc
		       (mapcar (lambda (i) `((,(nth i amf-means)
					       ,(nth i anmf-means)
					       ,(random-float *rand-gen* 0 1)
					       ,inp-sd
					       ,mn-perc
					       ,mx-perc
					       )
					     (,(nth i cmf-means)
					       ,(nth i cnmf-means)
					       ,(random-float *rand-gen* 0 1)
					       ,out-sd
					       ,mn-real
					       ,mx-real
					       )
					     ))
			       (cl21:iota num-mfs)))))
	    (cl21:iota (* 1 *num-inputs*)))))

;; (defun gen-rules-adjust (num-rules num-rel sep armf arnmf crmf crnmf)
;;   (mapcar (lambda (_)
;;             (let* ((amf-means (mapcar (lambda (i)
;;                                         (+ (- (* i sep) (access *rules-config* :sd)) armf))
;;                                       (alexandria:iota num-rel)))
;;                    (anmf-means (mapcar (lambda (i)
;;                                          (+ (- (* i sep) (access *rules-config* :sd)) arnmf))
;;                                        (alexandria:iota num-rel)))
;;                    (cmf-means (mapcar (lambda (i)
;;                                         (+ (- (* i sep) (access *rules-config* :sd)) crmf))
;;                                       (alexandria:iota num-rel)))
;;                    (cnmf-means (mapcar (lambda (i)
;;                                          (+ (- (* i sep) (access *rules-config* :sd)) crnmf))
;;                                        (alexandria:iota num-rel))))
;;               (apply #'nconc
;;                      (mapcar (lambda (i) `((,(nth i amf-means)
;;                                              ,(nth i anmf-means)
;;                                              ;; ,(random-float *rand-gen* 0 1)
;;                                              0
;;                                              )
;;                                            (,(nth i cmf-means)
;;                                              ,(nth i cnmf-means)
;;                                              ;; ,(random-float *rand-gen* 0 1)
;;                                              0
;;                                              )
;;                                            ))
;;                              (cl21:iota num-rel)))))
;;           (cl21:iota num-rules)))

;; (let ((params))
;;  (let* ((num-rel 7)
;;        (sep (/ (+ 100 (access *rules-config* :sd)) num-rel))
;;        (agents (mapcar (lambda (_)
;;                          (make-instance 'agent
;;                                         :rules (gen-rules-adjust
;;                                                 num-rel
;;                                                 sep
;;                                                 (random-float *rand-gen* 0 sep)
;;                                                 (random-float *rand-gen* 0 sep)
;;                                                 (random-float *rand-gen* 0 sep)
;;                                                 (random-float *rand-gen* 0 sep))))
;;                        (cl21:iota num)))
;;        (sim (agents-simulation agents)))
;;   (corrects sim
;;             (get-real-data (length sim)))))

(defun agent-perception-deltas (agent)
  (mapcar (lambda (percs close)
	    (append percs (list close)))
	  (remove nil
		  (maplist (lambda (deltas)
			     (when (>= (length deltas) *num-inputs*)
			       (subseq deltas 0 *num-inputs*)))
			   (get-real-data-deltas (+ *num-inputs* omper:*data-count*))
			   ))
	  (get-real-data omper:*data-count*)))

(defun cluster-perceptions-deltas (perceptions deltas rules-count)
  (let ((assoc (mapcar (lambda (d p)
			 `(,d . ,p))
		       deltas perceptions))
	(delta-clusters (mapcar #'flatten (km (mapcar #'list deltas) rules-count))))
    (mapcar (lambda (input)
	      (apply #'append input))
	    (apply #'mapcar #'list
		   (mapcar (lambda (cluster)
			     (let ((perc-cluster (mapcar (lambda (delta)
				   			   (access assoc delta))
				   			 cluster)))
			       (mapcar (lambda (mean stdev)
					 (list
					  (list mean
						(/ stdev 1))
					  (list (mean cluster)
						(/ (standard-deviation cluster) 1)
						))
					 )
				       (mapcar #'float (mapcar #'mean (apply #'mapcar #'list perc-cluster)))
				       (mapcar #'standard-deviation (apply #'mapcar #'list perc-cluster)))
			       )
			     )
			   delta-clusters)))))

;; (agent-perception-deltas nil)
;; ;; here
;; (float (mape (agents-simulation (create-agents 1 10 #'agent-perception-deltas))
;; 	     (get-real-data-deltas omper:*data-count*)))
;; (agent-moving-average nil)

;; (slot-value (first (create-agents 1 3 #'agent-perception-deltas)) 'rules)

;; (agent-trades (first (create-agents 1 4 #'agent-perception)))

(defun create-agents (agents-count rules-count &optional (perception-fn #'agent-perception))
  (let* ((agents (gen-agents agents-count))
	 (deltas (apply #'mapcar #'list
			(mapcar (lambda (delta)
				  (n-into-m-parts delta agents-count))
				(get-real-data-deltas omper:*data-count*))))
	 (perceptions (mapcar (lambda (agent)
				(mapcar #'butlast
					(funcall perception-fn agent)))
			      agents)))

    (mapcar (lambda (p d agent)
    	      ;; Setting min and max inputs and outputs for the fuzzy systems.
    	      (let ((fp (flatten p)))
    		(setf (slot-value agent 'input-min) (apply #'min fp))
    		(setf (slot-value agent 'input-max) (apply #'max fp))
    		(setf (slot-value agent 'output-min) (apply #'min d))
    		(setf (slot-value agent 'output-max) (apply #'max d)))

    	      ;; Setting agent rules.
    	      (setf (slot-value agent 'rules)
    		    (cluster-perceptions-deltas p
    						d
    						rules-count)))
    	    perceptions
    	    deltas
    	    agents)

    agents
    ))

;; (reduce #'+ (n-into-m-parts 777 10))

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

;; (float (mean (mapcar (lambda (_)
;; 		       (length (remove-if #'plusp (n-into-m-parts 11 1))))
;; 		     (iota 1000))))
;; (float (mean (mapcar (lambda (_)
;; 		       (length (remove-if #'minusp (n-into-m-parts 11 10))))
;; 		     (iota 1000))))


(defun gen-agents (num)
  (mapcar (lambda (_)
	    (let ((agent (make-instance 'agent)))
	      (setf (slot-value agent 'activation-threshold)
		    (let ((sums (mapcar (lambda (inp)
					  (reduce #'+
						  (mapcar (lambda (i)
							    (second
							     (membership (nth i inp)
									 (agents-ifs (nth i (slot-value agent 'activations))))))
							  (iota *num-inputs*))))
					(funcall *perception-fn* nil))))
		      (nth (nth (floor (1- (* omper:*data-count* 0.1)))
				(largest-number-indexes sums))
			   sums)))
	      agent))
	  (iota num)))

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
  (standard-deviation (get-real-data-deltas omper:*data-count*)))
;; (calc-stdev-deltas)

(defun woof (agents)
  ;; Reset all agents' leverages to 1.
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'leverage)
	    (iota *num-inputs* :start 1 :step 0)))
    (let* ((deltas (rest (get-real-data omper:*data-count*)))
           (trades (mapcar (lambda (agent)
                             (butlast (agent-trades agent))
                             )
                           (extract-agents-from-pool agents)))
           ;; (deltas (mapcar (lambda (next curr)
           ;;                   (- next curr))
           ;;                 (rest reals) reals))
	   )

      ;; Clustered deltas.
      (let* ((delta-clusters (mapcar (lambda (deltas)
				       (sort (copy-sequence 'list deltas) #'<))
				     (mapcar #'flatten (km (mapcar #'list deltas) (length agents)))))
             (trade-clusters (make-hash-table :size (length agents)))
	     ;; (idxs (mapcar (lambda (deltas)
	     ;; 		     (position (nth deltas) deltas))
	     ;; 		   delta-clusters))
	     (idxs (mapcar (lambda (cluster)
			     (floor (/ (length cluster) 2)))
			   delta-clusters))
             (mean-deltas (mapcar (lambda (idx cluster)
				    (nth idx cluster))
				  idxs
				  delta-clusters)))

        ;; Clustering deltas and trades.
        (dotimes (i (length deltas))
          ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
          (let ((num-cluster (position (nth i deltas) delta-clusters :test #'find)))
            ;; We save the slice of trades (different trades by different agents)
            ;; in its corresponding cluster.
            (push (mapcar (lambda (trade)
                            (nth i trade))
                          trades)
                  (gethash num-cluster trade-clusters))))
      
        (let* ((mean-trades (apply #'mapcar #'list
				   (mapcar (lambda (idx trades)
					     (apply #'mapcar (lambda (&rest nums)
							       (nth idx nums))
						    (cdr trades)))
					   idxs
					   (sort (copy-sequence 'list (hash-table-alist trade-clusters)) #'< :key #'first))))
               (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
               (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
               (sol-matrix (magicl:multiply-complex-matrices
                            (magicl:inv A)
                            B)))
          ;; Modifying leverages.
          (dotimes (j (length agents))
            (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
		  (iota *num-inputs* :start (realpart (magicl:ref sol-matrix j 0)) :step 0)
                  ;; (realpart (magicl:ref sol-matrix j 0))
		  ))
          )
        ;; (print (apply #'mapcar (lambda (&rest nums)
        ;;                          (mean nums)
        ;;                          ;; (print nums)
        ;;                          )
        ;;               (hash-table-values trade-clusters)))
        )

      ;; (apply #'mapcar #'list trades)
      ;; trades
      ))

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
       (get-real-data-deltas omper:*data-count*)
       (mapcar (lambda (rate)
                 (format nil "~f" rate))
               (agents-indexes-simulation (agents-best (agents-distribution *population*
                                                                            #'mape
                                                                            #'<)
                                                       #'<)))))

;; (csv-real-sim)

(defun woof (agents)
  (ignore-errors
    ;; Reset all agents' leverages to 1.
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'leverage) 1))
    
    (let* ((reals (get-real-data omper:*data-count*))
           (trades (mapcar (lambda (agent)
                             (butlast (agent-trades agent))
                             )
                           (extract-agents-from-pool agents)))
           (deltas (mapcar (lambda (next curr)
                             (- next curr))
                           (rest reals) reals)))

      ;; Clustered deltas.
      (let* ((delta-clusters (mapcar #'flatten (km (mapcar #'list deltas) (length agents))))
             (trade-clusters (make-hash-table :size (length agents)))
             (idxs (mapcar #'median-elt-idx delta-clusters))
             (mean-deltas (mapcar #'median-elt delta-clusters)))

        ;; Clustering deltas and trades.
        (dotimes (i (length deltas))
          ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
          (let ((num-cluster (position (nth i deltas) delta-clusters :test #'find)))
            ;; We save the slice of trades (different trades by different agents)
            ;; in its corresponding cluster.
            (push (mapcar (lambda (trade)
                            (nth i trade))
                          trades)
                  (gethash num-cluster trade-clusters))))

        (let* ((mean-trades (apply #'mapcar #'list
                                   (mapcar (lambda (idx trades)
                                             (apply #'mapcar (lambda (&rest nums)
                                                               (nth idx nums))
                                                    (cdr trades)))
                                           idxs
                                           (sort (copy-sequence 'list (hash-table-alist trade-clusters)) #'< :key #'first))))
               (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
               (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
               (sol-matrix (magicl:multiply-complex-matrices
                            (magicl:inv A)
                            B)))

          (if (= (realpart (magicl:det A)) 0.0)
              (format t ".")
              ;; Modifying leverages.
              (dotimes (j (length agents))
                (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
                      (realpart (magicl:ref sol-matrix j 0)))))
          )))))

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

(defun woof (agents)
  "Repeatedly clusters deltas and trades."
  (ignore-errors
    ;; Reset all agents' leverages to 1.
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'leverage)
	    (iota *num-inputs* :start 1 :step 0)))
    (let* ((trades (mapcar (lambda (agent)
			     (butlast (agent-trades agent))
			     )
			   (extract-agents-from-pool agents)))
	   (deltas (get-real-data-deltas omper:*data-count*)))

      (multiple-value-bind (mean-trades mean-deltas)
	  (cluster-trades-deltas trades deltas (length agents))
	;; (format t "~a~%~a~%" mean-trades mean-deltas)
	;; (format t "~a~%" mean-deltas)
	;; (format t "~a~%" mean-trades)
	(let* ((A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
	       (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
	       (sol-matrix (magicl:multiply-complex-matrices
			    (magicl:inv A)
			    B)))
	  ;; (format t ".")
	  ;; Modifying leverages.
	  (dotimes (j (length agents))
	    ;; (format t "~a" (realpart (magicl:ref sol-matrix j 0)))
	    (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
		  (iota *num-inputs* :start (realpart (magicl:ref sol-matrix j 0)) :step 0)
		  ;; (realpart (magicl:ref sol-matrix j 0))
		  )))))))

(defun woof (agents)
  "This one clusters by trades for the lulz."
  (ignore-errors
    ;; Reset all agents' leverages to 1.
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'leverage)
	    (iota *num-inputs* :start 1 :step 0)))
    
    (let* ((trades (apply #'mapcar #'list (mapcar (lambda (agent)
						    (butlast (agent-trades agent)))
						  (extract-agents-from-pool agents))))
	   (deltas (get-real-data-deltas omper:*data-count*))
	   )

      ;; Clustered trades.
      (let* ((delta-clusters (make-hash-table :size (length agents)))
	     (trade-clusters (mapcar #'flatten (km trades (length agents))))
	     (mean-trades (mapcar (mapcar (lambda (cluster)
					    (apply #'mapcar #'mean cluster)))
				  trade-clusters)))

	;; Clustering deltas and trades.
	(dotimes (i (length trades))
	  ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
	  (let ((num-cluster (position (nth i trades) trade-clusters :test (lambda (elt cluster)
									     (find elt cluster :test #'equal)))))
	    ;; We save the slice of trades (different trades by different agents)
	    ;; in its corresponding cluster.
	    (push (nth i deltas)
		  (gethash num-cluster delta-clusters))))

	(let* ((mean-deltas (mapcar (lambda (deltas)
				      (let ((poss (remove-if #'minusp (cdr deltas)))
					    (negs (remove-if #'plusp (cdr deltas)))))
				      (if (> (length poss) (length negs))
					  (mean poss)
					  (mean negs)))
				    (sort (copy-sequence 'list (hash-table-alist delta-clusters)) #'< :key #'first)))
	       (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
	       (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
	       (sol-matrix (magicl:multiply-complex-matrices
			    (magicl:inv A)
			    B)))
	  ;; Modifying leverages.
	  (dotimes (j (length agents))
	    (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
		  (iota *num-inputs* :start (realpart (magicl:ref sol-matrix j 0)) :step 0)
		  ;; (realpart (magicl:ref sol-matrix j 0))
		  ))
	  )))))

(defun woof (agents)
  (ignore-errors
    ;; Reset all agents' leverages to 1.
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'leverage)
	    (iota *num-inputs* :start 1 :step 0)))
    
    (let* ( ;; (reals (get-real-data omper:*data-count*))
	   (trades (mapcar (lambda (agent)
			     (butlast (agent-trades agent))
			     )
			   (extract-agents-from-pool agents)))
	   ;; (deltas (mapcar (lambda (next curr)
	   ;;                   (- next curr))
	   ;;                 (rest reals) reals))
	   (deltas (get-real-data-deltas omper:*data-count*))
	   )

      ;; Clustered deltas.
      (let* ((delta-clusters (mapcar #'flatten (km (mapcar #'list deltas) (length agents))))
	     (trade-clusters (make-hash-table :size (length agents)))
	     (idxs (mapcar #'median-elt-idx delta-clusters))
	     (mean-deltas (mapcar #'mean delta-clusters)))

	;; Clustering deltas and trades.
	(dotimes (i (length deltas))
	  ;; `num-cluster` is the number of the cluster in which we found the `ith` delta.
	  (let ((num-cluster (position (nth i deltas) delta-clusters :test #'find)))
	    ;; We save the slice of trades (different trades by different agents)
	    ;; in its corresponding cluster.
	    (push (mapcar (lambda (trade)
			    (nth i trade))
			  trades)
		  (gethash num-cluster trade-clusters))))

	(let* ((mean-trades (apply #'mapcar #'list
				   (mapcar (lambda (idx trades)
					     (apply #'mapcar (lambda (&rest nums)
							       (mean nums)
							       ;; (random-elt nums)
							       ;; (nth idx nums)
							       )
						    (cdr trades)))
					   idxs
					   (sort (copy-sequence 'list (hash-table-alist trade-clusters)) #'< :key #'first))))
	       (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
	       (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
	       (sol-matrix (magicl:multiply-complex-matrices
			    (magicl:inv A)
			    B)))
	  ;; Modifying leverages.
	  (dotimes (j (length agents))
	    (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
		  (iota *num-inputs* :start (realpart (magicl:ref sol-matrix j 0)) :step 0)
		  ;; (realpart (magicl:ref sol-matrix j 0))
		  ))
	  )))))

;; (dotimes (i (length *population*))
;;   (multiple-value-bind (res err)
;;       (woof
;;        (nth i *population*))
;;     (print err))
;;   )


;; (progn
;;   (setf omper:*data-count* 61)
;;   ;; (setf *community-size* 2)
;;   ;; Reset all agents to 1.
;;   (dolist (agent *agents-pool*)
;;     (setf (slot-value agent 'leverage) '(1)))
;;   (dolist (community *population*)
;;     (woof community))
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
         ((or (< (agents-mape community nil) error) 
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
      (let ((mape (agents-mape community nil)))
	(when (< mape best-error)
	  (setf best-error mape)
	  (setf best y))
	;; (format t "~a, ~a, ~a~%" mape (float (agents-corrects *best* nil)) (validate-test *best* 0.1 :corrects))
	;; (print mape)
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

;; (defparameter *best* (agents-best (agents-distribution *population* #'corrects #'>) #'>))
;; (defparameter *best* (agents-best (agents-distribution *population* #'mape)))
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
;; (float (agents-corrects *best* nil))
;; (validate-test *best* 1.0 :corrects)

;; 0.905592098739329d0, 0.6025, (0.575 0.5)
;; 0.910618936775545d0, 0.5675, (0.525 0.4)

;; Init using last entry (generations)
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :mape)))))) :id))

;; Init using agents with most corrects
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :corrects)))))) :id))

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
    (let ((mape (agents-mape agents-indexes t)))
      mape)))

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
			       (result `((:train . ((:mape . ,(float (accesses test :training :performance-metrics :mape)))
						    (:corrects . ,(accesses test :training :performance-metrics :corrects))))
					 (:validation . ((:mape . ,(float (accesses validation :testing :performance-metrics :mape)))
							 (:corrects . ,(accesses validation :testing :performance-metrics :corrects))))
					 (:test . ((:mape . ,(float (accesses test :testing :performance-metrics :mape)))
						   (:corrects . ,(accesses test :testing :performance-metrics :corrects)))
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

(defun get-best-descendant (id &optional (fitness-kw :mape))
  (let* ((desc (get-descendants id))
	 (idx (last-elt (largest-number-indexes (mapcar (lambda (entry)
							(access entry fitness-kw))
						      desc)))))
    (nth idx desc)))

(defun get-best-descendant-id (id &optional (fitness-kw :mape))
  (let* ((desc (get-descendants id))
	 (idx (last-elt (largest-number-indexes (mapcar (lambda (entry)
							(access entry fitness-kw))
						      desc)))))
    (access (nth idx desc) :id)))

;; (defmacro param-singleton (sym value)
;;   (print (find-symbol (symbol-name sym)))
;;   (unless (find-symbol (symbol-name sym) :overmind-agents)
;;     `(defparameter ,sym ,value)))

(defun wrap1 ()
  (init)
  (with-postgres-connection (execute (delete-from :populations)))
  (with-postgres-connection (execute (delete-from :populations-closure)))
  ;; (time (train 100000 100 :fitness-fn #'corrects :sort-fn #'> :save-every 1 :epsilon 1.8))
  (train 100000 100 :fitness-fn #'mape :sort-fn #'< :save-every 1 :epsilon 0.00 :gd-epsilon 0.000)
  )
;; (wrap1)
;; (agents-corrects (agents-best (agents-distribution *population* #'corrects #'>) #'>))
;; (dolist (pop *population*)
;;   (print (float (agents-corrects pop))))

(defun get-data-sample (validation-ratio testing-ratio)
  (let* ((*end* (ceiling (+ *end* (* omper:*data-count* (+ validation-ratio testing-ratio)))))
	 (omper:*data-count* (ceiling (* omper:*data-count* (+ 1 validation-ratio testing-ratio)))))
    (mapcar #'butlast (funcall *perception-fn* (extract-agent-from-pool 1)))))

(defun get-clustered-data-points (cluster-index)
  (all-positions cluster-index
		 (mapcar (lambda (datum)
			   (position datum *data-sample-clusters*
				     :test (lambda (datum cluster)
					     (find datum cluster :test #'equal))))
			 *data-sample*)))

;; (length (get-clustered-data-points 1))

(defun extract-training-clustered-trades (trades)
  ;; (mapcar (lambda (idx)
  ;; 	    (nth idx trades))
  ;; 	  (get-clustered-data-points *current-cluster*))
  trades
  )

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

(defun format-corrects (corrects)
  (format nil "~a/~a (~$%)"
	  (aref corrects 0)
	  (aref corrects 1)
	  (if (= (aref corrects 1) 0)
	      0.0
	      (* 100
		 (/ (aref corrects 0)
		    (aref corrects 1))))))

;; modify: draw-optimization, ifis-agents, agents-simulation

(defun draw-optimization (iterations &optional (agents-fitness-fn #'agents-mape) (fitness-fn #'mape) (sort-fn #'<) &key (key #'identity))
  (init)
  (with-postgres-connection (execute (delete-from :populations)))
  (with-postgres-connection (execute (delete-from :populations-closure)))
  (let ((parent-id ""))
    ;; (let ((corrects (agents-corrects (first *population*))))
    ;;   (format t "~%~6a ~f, ~t~a"
    ;; 	      0
    ;; 	      (float (funcall agents-fitness-fn (first *population*)))
    ;; 	      (format-corrects corrects)))
    (dotimes (i iterations)
      (let ((candidate (let ((option (random-float *rand-gen* 0 1)))
			 (cond
			   ;; Remove an agent.
			   ((and (< option 0.25) (> (length (first *population*)) 1))
			    (remove-nth (random-int *rand-gen* 0 (1- (length (first *population*)))) (first *population*)))
			   ;; Replace agent from solution using random agent.
			   ((and (< option 0.5) (> (length (first *population*)) 1))
			    (append
			     (remove-nth (random-int *rand-gen* 0 (1- (length (first *population*)))) (first *population*))
			     (list (random-int *rand-gen* 0 (1- (length *agents-pool*))))
			     ))
			   ;; Create new agent and push to pool.
			   ((< option 0.75)
			    (setf *agents-pool* (append *agents-pool* (gen-agents 1)))
			    (append
			     (first *population*)
			     (list (1- (length *agents-pool*)))))
			   ;; Append random agent from pool.
			   ((<= option 1.0)
			    (append
			     (first *population*)
			     (list (random-int *rand-gen* 0 (1- (length *agents-pool*))))
			     ))
			   ))))
	(let ((cand-error (funcall agents-fitness-fn candidate))
	      (best-error (funcall agents-fitness-fn (first *population*))))
	  (when (and (funcall sort-fn cand-error best-error)
		     ;; (> (length (remove-if #'zerop (agent-trades (extract-agent-from-pool (last-elt candidate))))) 1)
		     ;; Checking if candidate is messing with multiple trades.
		     ;; (let ((filled-trades (flatten (mapcar (lambda (i)
		     ;; 					     (all-positions-not 0 (agent-trades (nth i (extract-agents-from-pool candidate)))))
		     ;; 					   (iota (length candidate))))))
		     ;;   (= (length filled-trades)
		     ;; 	  (length (remove-duplicates filled-trades))))
		     )
	    
	    (setf (first *population*) candidate)
	    (setf parent-id (insert-best-agents parent-id "" fitness-fn sort-fn))

	    (let* ((corrects (agents-corrects candidate))
		   (report (first (get-reports 1 *testing-ratio*)))
		   (val-corrects (accesses report :validation :corrects))
		   (test-corrects (accesses report :test :corrects)))
	      (format t "~%~6a ~f, ~ttrain: ~a, ~tval: ~a, ~ttest: ~a"
		      (1+ i)
		      (float (funcall agents-fitness-fn candidate))
		      (format-corrects corrects)
		      (format-corrects val-corrects)
		      (format-corrects test-corrects))
	      (when (>= (aref corrects 1) 100)
		(return)))
	    ))))))

;; (draw-optimization 100000 #'agents-mape #'mape #'<)
;; (dotimes (_ 30) (draw-optimization 100000 #'agents-mape #'mape #'<))
;; (draw-optimization 3000 #'agents-corrects #'corrects #'>)
;; (print-simulation)
;; (format-corrects (accesses (first (get-reports 1 1.0)) :validation :corrects))
;; (mapcar (lambda (leaf) (format-corrects (accesses leaf :validation :corrects))) (report-all-best-database))
;; (length (first *population*))
;; (dotimes (i (length (first *population*))) (print (length (remove-if #'zerop (agent-trades (nth i (extract-agents-from-pool (first *population*))))))))
;; (length *agents-pool*)
;; (dotimes (i (length (first *population*))) (print (extract-training-clustered-trades (agent-trades (nth i (extract-agents-from-pool (first *population*)))))))

(defparameter *num-inputs* 10)
(defparameter *moving-average-start* 5)
(defparameter *moving-average-step* 10)
(defparameter *mutation-chance* 0.1)
(defparameter *rates* nil
  "The rates used to generate the agents' perceptions.")
;; (iota *num-inputs* :start *moving-average-start* :step *moving-average-step*)

;; (scale-sim '(0 10 4 -20 0) -10 10)

(defun init ()
  (defparameter *testing-ratio* 0.1)
  (setf lparallel:*kernel* (lparallel:make-kernel 32))
  (setf omper:*data-count* 500)
  (setf omper:*partition-size* 100)
  (defparameter *community-size* 1
    "Represents the number of agents in an 'individual' or solution. A simulation (a possible solution) will be generated using this number of agents.")
  (defparameter *population-size* 1
    "How many 'communities', 'individuals' or 'solutions' will be participating in the optimization process.")
  (defparameter *perception-fn* #'agent-perception-deltas)
  (defparameter *begin* (random-int *rand-gen* 0 (floor (- (length *all-rates*) (* omper:*data-count* 4))))
    "The starting timestamp for the data used for training or testing.")
  (defparameter *end* (+ *begin* (ceiling (* *data-count* 3)))
    "The ending timestamp for the data used for training or testing.")
  (defparameter *rates* (subseq *all-rates* *begin* *end*)
    "The rates used to generate the agents' perceptions.")
  (defparameter *trade-scale* (calc-trade-scale))
  (defparameter *generations* 0
    "Keeps track of how many generations have elapsed in an evolutionary process.")
  (defparameter *num-pool-agents* 1
    "How many agents will be created for `*agents-pool*`. Relatively big numbers are recommended, as this increases diversity and does not significantly impact performance.")
  (defparameter *num-rules* 5
    "Represents how many fuzzy rules each of the agents in a solution will have.")
  (defparameter *rules-config* `((:mf-type . :gaussian)
                                 (:sd . 5)
                                 (:mf-mean-adjusting . t)
                                 (:nmf-height-style . :complement)
                                 ;; (:trade-scale . ,(calc-trade-scale))
                                 )
    "Configuration used to create the rules of the agents for a population.")
  (defparameter *agents-pool* (gen-agents *num-pool-agents*)
    "Instances of `agent` that are available to create solutions.")
  (defparameter *population* (gen-communities *community-size* *population-size*)
    "Represents a list of lists of indexes to *agents-pool*.")
  (defparameter *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t)
    "Used for memoizing an agent's simulation.")
  (defparameter *fitnesses* nil
    "List of fitnesses obtained after evolving a population.")
  (defparameter *fitnesses-validation* nil
    "List of fitnesses in the validation stage obtained after evolving a population.")
  (defparameter *fitness-fn* :mape)
  ;; Setting boundaries.
  (let* ((percs (flatten (mapcar #'butlast (funcall *perception-fn* (first *agents-pool*)))))
	 (reals (get-real-data-deltas omper:*data-count*))
	 (mn-perc (apply #'min percs))
	 (mx-perc (apply #'max percs))
	 (mn-real (apply #'min reals))
	 (mx-real (apply #'max reals)))
    (dolist (agent *agents-pool*)
      (setf (slot-value agent 'input-min) mn-perc)
      (setf (slot-value agent 'input-max) mx-perc)
      (setf (slot-value agent 'output-min) mn-real)
      (setf (slot-value agent 'output-max) mx-real))
    )
  (defparameter *agents-cluster-size* 10)
  ;; (defparameter *data-sample* (get-data-sample *validation-ratio* *testing-ratio*))
  (defparameter *data-sample* (get-data-sample 0 0))
  (defparameter *data-sample-clusters* (km *data-sample* *agents-cluster-size*))
  (defparameter *current-cluster* 1)
  )
;; (init)
;; (wrap1)

;; (time (train 100000 100 :fitness-fn #'corrects :sort-fn #'> :save-every 10 :epsilon 1.8))
;; (time (train 100000 100 :fitness-fn #'mape :sort-fn #'< :save-every 1 :epsilon 0.00 :gd-epsilon 0.000))
;; *cached-agents*
;; *population*
;; *generations*

;; (filter-reports *results*)
;; (defparameter *results* (get-reports 1 0.1))

;; Init using last entry (generations)
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :mape)))))) :id))

;; Communities' sizes.
;; (mapcar #'length *population*)

;; Get corrects of *results*.
;; (dolist (res (reverse *results*))
;;   (format t "~a,~a,~a~%" (accesses res :train :corrects)
;; 	  (accesses res :validation :corrects)
;; 	  (accesses res :test :corrects)))

;; Report
;; (map nil (lambda (lst) (format t "gen: ~a ~t mape:~a ~t corrects: ~a ~t revenue: ~a~%" (access:access lst :generations) (float (access:access lst :mape)) (* (float (access:access lst :corrects)) 100) (float (access:access lst :revenue)))) (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :creation-time))))))

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
;; (float (mape (agents-indexes-simulation (agents-best (agents-distribution *population* #'mape
;; 									  #'<)
;; 						     #'<))
;; 	     (get-real-data-deltas omper:*data-count*)))

;; (mapcar (lambda (pop)
;; 	  (float (corrects (agents-indexes-simulation pop)
;; 			   (get-real-data omper:*data-count*))))
;; 	*population*)

;; Check if there are duplicates.
;; (mapcar #'length (mapcar #'remove-duplicates *population*))

;; (setq *last-id* (train 50000 :starting-population *last-id* :fitness-fn #'pmape :sort-fn #'<))
;; (setq *last-id* (train 50000 :starting-population "6DF6BB62-B9AB-4A6B-B6A8-9EF44EFD86FB" :fitness-fn #'pmape :sort-fn #'<))

;; (agent-trades (extract-agent-from-pool (alexandria:random-elt (first *population*))))
;; (slot-value (extract-agent-from-pool (first (first *population*))) 'rules)
;; *generations*

;; continue from database
;; (setq *last-id* (train 50000 :starting-population (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :generations)))))) :id) :fitness-fn #'mape :sort-fn #'<))

;; Init using agents with most corrects
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :corrects)))))) :id))

;; Check most corrects from database.
;; (float (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :corrects)))))) :corrects))

;; Woofing individually.
;; (woof (agents-best (agents-distribution *population*)))
;; (agents-corrects (agents-best (agents-distribution *population*)))
;; (let ((omper:*data-count* (ceiling (* omper:*data-count* 0.1))))
;;   (agents-test (extract-agents-from-pool
;;                 (agents-best (agents-distribution *population*)))
;;                (subseq *all-rates* *begin* (+ *end* omper:*data-count*))))

;; (agents-corrects (agents-best (agents-distribution *population*)))

;; (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; (get-real-data-deltas omper:*data-count*)
;; (agents-corrects (agents-best (agents-distribution *population*)))
;; (mapcar (lambda (agent)
;;           (slot-value agent 'leverage))
;;         (extract-agents-from-pool (agents-best (agents-distribution *population*))))

;; Checking simulation.
;; (agents-corrects (nth 8 *population*))
;; (agents-indexes-simulation (nth 8 *population*))
;; (agents-test (extract-agents-from-pool (nth 8 *population*)) (subseq *all-rates* *begin* (+ *end* (* 11 omper:*data-count*))))
;; (get-real-data 20)

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
;; (get-real-data 20)
;; (agents-corrects (nth 8 *population*))

;; (get-ancestors *last-id*)
;; (access:access (get-population *last-id*) :generations)

;; (length (with-postgres-connection (retrieve-all (select :* (from :populations-closure)))))

;; (access (first (with-postgres-connection (retrieve-all (select (:id :pmape) (from :populations) (order-by (:asc :pmape)))))) :id)

;; (defparameter *results* (report-all-best-database))

;; (get-best-descendant-id "8BD62D44-D989-4FB4-8A0D-F45665602A4A")

(defun train (generations gd-iterations
	      &key (fitness-fn #'pmape)
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
	     ;; (fitness-validation (first (validate-test (agents-best (agents-distribution *population*))
	     ;; 					       1.0 :mape)))
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

(defun print-simulation ()
  (map nil (lambda (s r)
	     (format t "~5f, ~5f~%" s r))
       (let* ((real (get-real-data-deltas omper:*data-count*))
	      (rmax (apply #'max real))
	      (rmin (apply #'min real)))
	 (extract-training-clustered-trades (scale-sim (agents-indexes-simulation (agents-best (agents-distribution *population*))) rmin rmax)))
       (extract-training-clustered-trades (get-real-data-deltas omper:*data-count*))))

(defun insert-best-agents (parent-id label fitness-fn sort-fn)
  (let* ((child-id (insert-population parent-id label fitness-fn sort-fn)))
    (insert-initial-closure child-id)
    (insert-closure parent-id child-id)
    child-id
    ))

(defclass agent ()
  ;; (slot-value (first (slot-value (make-instance 'agents) 'agents)) 'beliefs)
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-rules*) :accessor rules)
   ;; (leverage :initarg :leverage :initform 1)
   (activations :initarg :activations :initform (mapcar (lambda (rule)
						       (first rule))
							(gen-activations 1))
		:accessor activations)
   (activation-threshold :initarg :activation-threshold :initform 0 :accessor activation-threshold)
   (leverage :initarg :leverage :initform 1)
   (input-min :initarg :input-min :initform 0)
   (input-max :initarg :input-max :initform 100)
   (output-min :initarg :output-min :initform 0)
   (output-max :initarg :output-max :initform 100)
   ;; (leverage :initarg :leverage :initform (calc-trade-scale))
   ;; (leverage :initarg :leverage :initform (iota *num-inputs* :start 1 :step 0))
   ;; (leverage :initarg :leverage :initform (random-float *rand-gen* 0 (* 1 (calc-leverage-mean))))
   ;; (leverage :initarg :leverage :initform (mapcar (lambda (_) (random-float *rand-gen* 0
   ;; 									    (* (calc-trade-scale) 2)))
   ;; 						  (iota *num-inputs*)))
   ;; (leverage :initarg :leverage :initform (iota *num-inputs* :start 1 :step 0))
   ;; (rules-deltas :initarg :rules-deltas :initform (iota (* 2 *num-rules* *num-inputs*) :start 0 :step 0))

   ;; (leverage :initarg :leverage :initform (random-float *rand-gen* 0 1))
   ;; (stdev :initarg :stdev :initform (random-float *rand-gen* 5 30))
   ;; (stdev :initarg :stdev :initform (access *rules-config* :sd))
   ;; (stdev :initarg :stdev :initform (+ (access *rules-config* :sd) (random-float *rand-gen* -1 1)))
   ))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules activations activation-threshold leverage input-min input-max output-min output-max))

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
						      (order-by :end (:desc :corrects))))))
	both-match
	;; Couldn't find any. Now trying to retrieve results where instrument matches.
	(alexandria:if-let ((inst-match (retrieve-one (select :*
							(from :populations)
							(where (:= :instrument
								   (format nil "~s" instrument)))
							(order-by :end (:desc :corrects))))))
	  inst-match
	  ;; Couldn't find any. Now trying to retrieve results where timeframe matches.
	  (alexandria:when-let ((time-match (retrieve-one (select :*
							    (from :populations)
							    (where (:= :timeframe
								       (format nil "~s" timeframe)))
							    (order-by :end (:desc :corrects))))))
	    time-match
	    )))))
;; (access (get-most-relevant-population :EUR_USD :H1) :best-index)

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
                 (:instrument . ,(access db-pop :instrument))
                 (:timeframe . ,(access db-pop :timeframe))
                 (:generations . ,(access db-pop :generations))
                 (:fitness-fn . ,(access db-pop :fitness-fn))
                 (:creation-time . ,(access db-pop :creation-time))
                 (:performance-metrics . ((:mae . ,(access db-pop :mae))
                                          (:mse . ,(access db-pop :mse))
                                          (:rmse . ,(access db-pop :rmse))
                                          (:mape . ,(access db-pop :mape))
                                          (:pmape . ,(access db-pop :pmape))
                                          (:corrects . ,(access db-pop :corrects))
                                          (:revenue . ,(access db-pop :revenue)))))
      (:testing (:begin . ,(read-from-string (access (first rates) :time)))
		 (:end . ,(read-from-string (access (alexandria:last-elt rates) :time)))
                (:instrument . ,instrument)
                (:timeframe . ,timeframe)
                ,@(agents-test best rates)
                (:rates . ,rates)))
    ))

;; (ql:quickload :magicl)

;; (magicl:multiply-complex-matrices
;;  (magicl:inv (magicl:make-complex-matrix 3 3 '(1 2 3 1 2 3 1 2 3)))
;;  (magicl:make-complex-matrix 3 1 '(3 6 9)))

;; (magicl:multiply-complex-matrices
;;  (magicl:inv (magicl:make-complex-matrix 3 3 '(1 0 2 1 2 5 1 5 -1)))
;;  (magicl:make-complex-matrix 3 1 '(6 -4 27)))

;; ;; get best's position
;; (defparameter *best-idx*
;;   (position (agents-best (agents-distribution *population*))
;;             *population* :test #'equal))

;; (defun reset-leverages ()
;;   (dolist (community *population*)
;;     (dolist (agent-idx community)
;;       (setf (slot-value (extract-agent-from-pool agent-idx) 'leverage) 1))))

;; (dolist (agent *agents-pool*)
;;   (print (slot-value agent 'leverage)))

;; (mapcar #'agents-pmape *population*)

;; (let ((begin (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 450 :day)))
;;       (end (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 30 :day))))
;;   (mapcar (lambda (agents)
;;             (agents-test (extract-agents-from-pool agents)
;;                          (get-rates-range *instrument*
;;                                           *timeframe*
;;                                           begin end
;;                                           )))
;;           *population*))

;; Get all corrects from current population in training stage.
;; (mapcar (lambda (i)
;; 	  (setf *cached-agents* (make-hash-table :test #'equal))
;; 	  (print (float
;; 		  (accesses (agents-test (extract-agents-from-pool (nth i *population*))
;; 					 (subseq *all-rates* *begin* *end*))
;; 			    :performance-metrics :corrects))))
;; 	(alexandria:iota (length *population*)))

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

(defun get-moving-average (periods)
  (remove nil
	  (mapcar (moving-average periods)
		  (get-real-data
		   (+ omper:*data-count*
		      (1- periods))))))

(defparameter *stochastic-key-high* 5)
(defparameter *stochastic-key-low* 3)
(defparameter *stochastic-key-close* 3)

;; (mapcar (stochastic 5 3 3)
;; 	(get-real-data 10))

(defun get-stochastic-oscillator (periods)
  (remove nil
	  (mapcar (stochastic *stochastic-key-high*
			      *stochastic-key-low*
			      *stochastic-key-close*
			      periods)
		  (get-real-data
		   (+ omper:*data-count*
		      (1- periods))))))

(defun agent-moving-average (agent)
  "TODO: `agent` is unused"
  (mapcar (lambda (mas rate)
	     (append
	      ;; (mapcar (lambda (ma)
	      ;; 		(+ 50
	      ;; 		   (* 1000
	      ;; 		      (/ (- (first rate) ma)
	      ;; 			 (first rate)))))
	      ;; 	      mas)
	      mas
	      rate))
	   (apply #'mapcar #'list
		  (mapcar (lambda (i)
			    (get-moving-average i))
			  (iota *num-inputs* :start *moving-average-start* :step *moving-average-step*)))
	   (mapcar #'list (get-real-data omper:*data-count*))))

(defun agent-stochastic-oscillator (agent)
  "TODO: `agent` is unused"
  (mapcar (lambda (stochastic rate)
            (append
             ;; (mapcar (lambda (s)
             ;;           (/ (+ 100 s) 2))
             ;;         stochastic)
	     stochastic
             rate))
          (apply #'mapcar #'list
                 (mapcar (lambda (i)
                           (get-stochastic-oscillator i i))
                         (iota *num-inputs* :start 1 :step 10)))
          (mapcar #'list (get-real-data omper:*data-count*))))
;; (agent-stochastic-oscillator nil)

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

(defun get-stochastic-oscillator (&optional (%k-periods 5) (%d-periods 5))
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
                                         (+ %d-periods %k-periods)))))
        omper:*data-count*))
;; (get-stochastic-oscillator 20 20)
;; (sort (get-stochastic-oscillator 5 3) (lambda (e1 e2)
;;                                         (if (null e2)
;;                                             0
;;                                             (< e1 e2))) :key #'second)

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

(defun get-reports (count &optional (ratio 0.1))
  (let* ((omper:*data-count* (ceiling (* omper:*data-count* ratio))))
    (mapcar (lambda (db-pop)
	      (let* ((validation (market-report db-pop
						*instrument* *timeframe*
						(subseq *all-rates* *begin* (+ *end* omper:*data-count*))))
		     (test (market-report db-pop
					  *instrument* *timeframe*
					  (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*)))))
		     (result `((:train . ((:mape . ,(float (accesses test :training :performance-metrics :mape)))
					  (:corrects . ,(accesses test :training :performance-metrics :corrects))))
			       (:validation . ((:mape . ,(float (accesses validation :testing :performance-metrics :mape)))
					       (:corrects . ,(accesses validation :testing :performance-metrics :corrects))))
			       (:test . ((:mape . ,(float (accesses test :testing :performance-metrics :mape)))
					 (:corrects . ,(accesses test :testing :performance-metrics :corrects)))
				      ))))
		result))
	    (retrieve-all (select (:*)
			    (from :populations)
			    (order-by (:desc :creation-time))
			    (limit count))))))

(defun run-random-rates (iterations)
  (dotimes (_ iterations)
    (init)
    (train 1000 1000 :fitness-fn #'mape :sort-fn #'< :save-every 10 :epsilon 0.02)
    )
  ;; (train 1000 :fitness-fn #'corrects :sort-fn #'> :save-every 1 :epsilon 1.8))
  (get-reports 1))

;; (filter-reports *reports*)
;; (defparameter *reports* (get-reports 1))
;; (defparameter *results* (run-random-rates 1))
;; *cached-agents*
;; *generations*

(defun filter-reports (reports)
  (let ((train-mape 0)
	(train-corrects 0)
	(val-mape 0)
	(val-corrects 0)
	(test-mape 0)
	(test-corrects 0)
	(len 0))
    (dolist (report reports)
      (when (and ;; (> (accesses report :train :corrects) 0.5)
             (< (accesses report :train :mape) 0.01)
             ;; (> (accesses report :validation :corrects) 0.6)
             ;; (< (accesses report :validation :mape) 0.02)
             )
	(incf train-mape (accesses report :train :mape))
	(incf train-corrects (accesses report :train :corrects))
	(incf val-mape (accesses report :validation :mape))
	(incf val-corrects (accesses report :validation :corrects))
	(incf test-mape (accesses report :test :mape))
	(incf test-corrects (accesses report :test :corrects))
	(incf len)))
    (when (> len 0)
      `((:train-mape . ,(float (/ train-mape len)))
	(:train-corrects . ,(float (/ train-corrects len)))
	(:validation-mape . ,(float (/ val-mape len)))
	(:validation-corrects . ,(float (/ val-corrects len)))
	(:test-mape . ,(float (/ test-mape len)))
	(:test-corrects . ,(float (/ test-corrects len)))
        (:sample-size . ,len)))))

;; (accesses *market-report* :training :performance-metrics :corrects)
;; (accesses *market-report* :testing :performance-metrics :corrects)
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
         (real (get-real-data-deltas (length sim)))

         (mae (mae sim real))
	 (mse (mse sim real))
	 (rmse (rmse sim real))
	 (mape (mape sim real nil))
	 (pmape (pmape sim real))
	 (corrects (corrects sim real nil))
	 (revenue (revenue sim real)))
    `((:performance-metrics . ((:mae . ,mae)
                               (:mse . ,mse)
                               (:rmse . ,rmse)
                               (:mape . ,mape)
                               (:pmape . ,pmape)
                               (:corrects . ,corrects)
                               (:revenue . ,revenue)))
      (:simulation . ,sim))))

(defun agents-indexes-test (agents-indexes rates)
  "Runs a simulation of a market defined by `rates` using `agents-indexes`. `agents-indexes` is used to extract agents from `*agents-pool*`. Returns multiple performance metrics."
  (agents-test (extract-agents-from-pool agents-indexes) rates))

;; (dolist (pop *population*)
;;   (dolist (beliefs (slot-value pop 'beliefs))
;;     (print beliefs)))

;; ;; (dolist (real (get-real-data))
;; ;;   (print real))

;; ;; ;; make a test-all method to test a model against multiple markets, multiple testing sets
;; ;; ;; the objective is to prove that our architecture is generalized enough to work anywhere
;; ;; (let ((*rates* (subseq (ms:unmarshal (read-from-string (file-get-contents "/home/amherag/quicklisp/local-projects/neuropredictions/data/aud_usd.dat"))) 500 1000)))
;; ;;   (corrects (agents-indexes-simulation (agents-best (agents-distribution *population*)))
;; ;; 	  (get-real-data)))

;; (defparameter *best-agent* (agents-best (agents-distribution *population*)))

(defun agents-ifs (params from-x to-x)
  "Uses data encoded in agents."
  (ifs (gaussian-mf (nth 0 params) (nth 1 params) from-x to-x 0 1)
       (gaussian-nmf (nth 0 params) (nth 1 params) from-x to-x 0 0)))

(defun agents-ifs (params stdev)
  (ifs (gaussian-mf (nth 0 params) stdev 0 100 0 (- 1 (nth 2 params)))
       (gaussian-nmf (nth 1 params) stdev 0 100 0 (nth 2 params))))

(defun agents-ifs (params)
  (ifs (gaussian-mf (nth 0 params) (nth 3 params) (nth 4 params) (nth 5 params) 0 (- 1 (nth 2 params)))
       (gaussian-nmf (nth 1 params) (nth 3 params) (nth 4 params) (nth 5 params) 0 (nth 2 params))))

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
			       (if (< (length (remove-if #'zerop trades)) -1)
				   0
				   (apply #'+ trades))
			       )
		    (mapcar (lambda (agent)
			      ;; (extract-training-clustered-trades
			      ;;  (agent-trades agent))
			      (agent-trades agent))
			    agents))))
    ;; (mapcar (lambda (sim real)
    ;; 	      ;; summing the current price + the sum of all trades to obtain next price
    ;; 	      ;; (mse) is in charge of comparing this price against the next one
    ;; 	      (+ sim real))
    ;; 	    ;; summation of trades of every agent in `agents`
    ;; 	    sim
    ;; 	    (get-real-data (length sim)))
    sim
    ))

(defun agent-simulation (agent)
  "Returns a simulation of a single agent trading a dataset."
  (let ((deltas (agent-trades agent)))
    deltas
    ;; (mapcar #'+
    ;; 	    deltas
    ;; 	    (get-real-data (length deltas)))
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
    (revenue sim (get-real-data-deltas (length sim)))))

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
    (revenue sim (get-real-data-deltas (length sim)))))

(defun agents-corrects (agents-indexes &optional (mape-constraint nil))
  "Returns the number of correct direction predictions made by the agents in
`agents-indexes` for the real prices."
  ;; (agents-corrects (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (corrects sim (get-real-data-deltas (length sim)) mape-constraint)))

(defun agents-pmape (agents-indexes &optional (zero-metric-constraint t))
  "Returns the penalized mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (pmape sim (get-real-data-deltas (length sim)) zero-metric-constraint)))

(defun agents-mape (agents-indexes &optional (zero-metric-constraint nil))
  "Returns the mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mape sim (get-real-data-deltas (length sim)) zero-metric-constraint)))

;; (agents-mape (second *population*))
;; (get-real-data-deltas 10)
;; (agents-indexes-simulation (first *population*))

(defun agents-rmse (agents-indexes)
  "Returns the root mean square error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (rmse sim (get-real-data-deltas (length sim)))))

(defun agents-mae (agents-indexes)
  "Returns the mean absolute error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mae sim (get-real-data-deltas (length sim)))))

(defun agents-mse (agents-indexes)
  "Returns the mean squared error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mse sim (get-real-data-deltas (length sim)))))

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

(defun get-real-data-deltas (count)
  (let ((reals (get-real-data count)))
    (mapcar (lambda (curr next)
              (- next curr))
            reals
            (rest reals))))

;; (get-real-data-deltas 10)
;; (get-real-data 10)

(defun get-real-data (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 count)))

(defun get-full-real-data (count)
  (reverse (subseq (reverse *rates*) 0 count)))

(defun get-accumulation (start vals)
  (let ((results `(,start)))
    (mapcar (lambda (val)
	   (push (+ (first results) val) results))
	 vals)
    (rest (reverse results))))

;; (defparameter *num-pool-agents* 2000)
;; (defparameter *num-rules* 2)
;; (defparameter *community-size* 20)
;; (defparameter *population-size* 20)
;; (defparameter *agents-pool* (gen-agents *num-pool-agents*))
;; (defparameter *population* (gen-communities *community-size* *population-size*))
;; (defparameter *cached-agents* (make-hash-table :test #'equal))
;; (defparameter *fitnesses* nil)
;; (defparameter *ifs-sd* 20)
;; (defparameter *continue?* t)
;; (defparameter *num-pool-agents* 10)
;; (defparameter *num-rules* 2)
;; (defparameter *cached-agents* (make-hash-table :test #'equal))
;; (defparameter *ifs-sd* 30)
;; (defparameter *random-best* nil)

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

(defun insert-population (parent-id &optional label (fitness-fn #'mape) (sort-fn #'<))
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
			 :pmape (float (agents-pmape best))
			 :rmse (float (agents-rmse best))
			 :mae (float (agents-mae best))
			 :mse (float (agents-mse best))
			 :corrects (agents-corrects best nil)
			 :revenue (float (agents-revenue best))
			 ))))
    id))
