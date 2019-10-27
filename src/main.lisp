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
  (defparameter *all-rates* (get-rates *instrument* 1 *timeframe*)
    "All the rates. Subsets are used during training, validation and testing stages."))

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
                    (corrects :type '(:numeric)
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
                  (corrects :type '(:numeric)
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
    ;; Resetting `*cached-agents*`, as these most likely are useless now.
    (setf *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t))
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
	    (alexandria:flatten pop))
    ;; Setting parameters according to what is set in the stored population.
    (setf *community-size* (length (first pop)))
    (setf *population-size* (length pop))
    (setf *num-rules* (length (slot-value (first (first pop)) 'rules)))
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
			 (abs (/ (- r s) r)))
		       sim real))
  	 0.01)
      t))

(defun pmape (sim real)
  "Penalized MAPE. If a simulation has a different direction than that of the real price, it gets penalized with a weight."
  (if (check-zero-metric sim real)
      most-positive-fixnum
      (let ((last-real (first (rest real))))
	(/ (reduce #'+ (mapcar (lambda (s r)
				 (prog1
				     (if (>= (* (- r last-real)
						(- s last-real))
					     0)
					 (abs (/ (- r s) r))
					 (* 3
					    (abs (/ (- r s) r))))
				   (setf last-real r)))
			       sim
			       (rest real)))
	   (length (rest real))))))

(defun mape (sim real)
  "Mean absolute percentage error between a simulated time series `sim` and a real time series `real`."
  (if (check-zero-metric sim real)
      most-positive-fixnum
      (/ (reduce #'+ (mapcar (lambda (s r)
			       (abs (/ (- r s) r)))
			     sim
			     (rest real)))
	 (length (rest real)))))

;; (check-zero-metric '(1.001 1.999 1.001 1.999 1.001 1.999)
;; 		   '(1 2 1 2 1 2))

(defun mse (sim real)
  "Mean squared error between a simulated time series `sim` and a real time series `real`."
  (if (check-zero-metric sim real)
      most-positive-fixnum
      (/ (reduce #'+ (mapcar (lambda (elt) (expt elt 2)) (mapcar #'- sim (rest real))))
	 (length (rest real)))))

(defun mae (sim real)
  "Mean absolute error between a simulated time series `sim` and a real time
series `real`."
  (if (check-zero-metric sim real)
      most-positive-fixnum
      (/ (reduce #'+ (mapcar (lambda (s r)
			       (abs (- s r)))
			     sim
			     (rest real)))
	 (length (rest real)))))

(defun rmse (sim real)
  "Root mean square error between a simulated time series `sim` and a real time
series `real`."
  (sqrt (mse sim real)))

(defun revenue (sim real)
  "Currently it returns the agent profit at each trade."
  (let ((real (rest real))
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

(defun corrects (sim real &optional (mape-constraint t))
  "How many trades were correct."
  ;; (corrects (agents-indexes-simulation (agents-best (agents-distribution *population*))) (get-real-data))
  (if (and mape-constraint (> (mape sim real) 0.1))
      (/ 1 (length real))
      (let ((real (rest real))
	    ;; we only need the real previous, as the simulated is based on the last real price at every moment
	    (prev (first real)))
	(/ (apply #'+ (mapcar (lambda (s r)
				(let ((real-dir (- r prev))
				      (sim-dir (- s prev)))
				  ;; (format t "prev: ~a~t real: ~a~t sim: ~a~t real-dir: ~a~t sim-dir: ~a~t~%" prev r s real-dir sim-dir)
				  (setq prev r)
				  ;; (print real-dir)
				  (if (> (* real-dir sim-dir) 0)
				      1
				      0)))
			      sim
			      real))
	   (length real)))))

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

(defclass agent ()
  ;; (slot-value (first (slot-value (make-instance 'agents) 'agents)) 'beliefs)
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-rules*) :accessor rules)
   (leverage :initarg :leverage :initform 1)
   ;; (trade-scale :initarg :trade-scale :initform (random-int *rand-gen* 1 100000))
   (trade-scale :initarg :trade-scale :initform 1)
   ;; (trade-scale :initarg :trade-scale :initform (access *rules-config* :trade-scale))
   ;; (leverage :initarg :leverage :initform (random-float *rand-gen* 0 1))
   ;; (stdev :initarg :stdev :initform (random-float *rand-gen* 5 30))
   (stdev :initarg :stdev :initform (access *rules-config* :sd))
   ;; (stdev :initarg :stdev :initform (+ (access *rules-config* :sd) (random-float *rand-gen* -1 1)))
   ))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules leverage trade-scale stdev))

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

(defun agent-perception (agent)
  "Generates the fibos of a market according to `agent`'s beliefs."
  ;; (agents-perception (extract-agent-from-pool 1))
  (mapcar (lambda (heat)
            (let ((heat (cl21:gethash heat :heat))
                  (close (cl21:gethash heat :close)))
              (let ((z (cl21:gethash heat :z))
                    (index (alexandria:if-let
                               ((res (search `(,close) (cl21:gethash heat :y)
                                             :key (lambda (elt)
                                                    (when (> close elt) t)))))
                             res
                             (1- (length (cl21:gethash heat :y)))
                             )))
                (list (cl21:nth z (if (< (- index 6) 0) 0 (- index 6)))
		      (cl21:nth z (if (< (- index 5) 0) 1 (- index 5)))
		      (cl21:nth z (if (< (- index 4) 0) 2 (- index 4)))
		      (cl21:nth z (if (< (- index 3) 0) 3 (- index 3)))
		      (cl21:nth z (if (< (- index 2) 0) 4 (- index 2)))
		      (cl21:nth z (if (< (- index 1) 0) 5 (- index 1)))
		      (cl21:nth z index)
		      close
		      ))))
          ;; TODO: generalize (get-data) so it doesn't need an `instrument`.
          (get-data *instrument* *rates* :levels (slot-value agent 'beliefs))))


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

(defun agent-trades (agent &optional (perception-fn #'agent-moving-average))
  "Creates a simulation of a single agent's trades."
  ;; (time (length (agent-trades (make-instance 'agent))))
  ;; (time (agent-trades (first (extract-agents-from-pool '(2)))))
  (let ((sig (reduce #'+
		     (append (flatten (slot-value agent 'beliefs))
			     (flatten (slot-value agent 'rules))
                             (mapcar (lambda (rate)
                                       (access rate :close-bid))
                                     (last *rates* 10))
                             ))))
    (if (gethash sig *cached-agents*)
	(mapcar (lambda (trade)
		  (process-agent-output trade
					(slot-value agent 'leverage)))
		(gethash sig *cached-agents*))
	(let* ((data (funcall perception-fn agent))
               (closes (mapcar #'last-elt data)))
	  (let ((sim (ifis-agents data
                                  (slot-value agent 'rules)
				  (slot-value agent 'trade-scale)
                                  (slot-value agent 'leverage)
                                  (slot-value agent 'stdev))))
	    ;; (sb-ext:with-locked-hash-table (*cached-agents*)
	    ;;   (setf (gethash sig *cached-agents*) sim))
	    (setf (gethash sig *cached-agents*) sim)
	    (mapcar (lambda (trade)
		      (process-agent-output trade
					    (slot-value agent 'leverage)))
		    sim))))))

(defun agents-fitness (agents-indexes &optional (fitness-fn #'mape))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    ;; (mse sim (get-real-data 3))
    (funcall fitness-fn sim (get-real-data (length sim)))))

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
						       (rule ;; (* 100 (/ (nth i inp) (+ 1 mx-inp)))
							(nth i inp)
							     (nth (* j 2) ifs)
							     (nth (1+ (* j 2)) ifs)))
						     (iota (floor (/ (length ifs) 2)))))))
				 (iota (length (butlast inp)))))
		 )))
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
  (dolist (community *population*)
    (woof community))
  
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
  (dolist (community *population*)
    (woof community))

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

(defun agents-mf-adjust (agents iterations &optional (fitness-fn #'pmape) (delta 1))
  (let ((best-error 10000))
    (dotimes (_ iterations)
      (woof agents)
      (let* ((sim (agents-simulation agents))
             (errs (mapcar #'- sim (rest (get-real-data (length sim)))))
             (errs-idxs (largest-number-indexes (mapcar #'abs errs)))
             (sims (mapcar (lambda (agent)
                             ;; We're ignoring the last one as that is the final prediction,
                             ;; i.e. there's no real data price for that.
                             (butlast (agent-simulation agent)))
                           agents))
             (errs-sims (mapcar (lambda (idx)
                                  (largest-number-index
                                   (mapcar (lambda (sim)
                                             (nth idx sim))
                                           sims)))
                                errs-idxs))
             (modified-agents '()))
        (dotimes (i (length errs-idxs))
          (let* ((agent-idx (nth i errs-sims))
                 (agent (nth agent-idx agents))
                 (err (nth (nth i errs-idxs) errs))
                 (rule-idx (random-int *rand-gen* 0 (1- *num-rules*)))
                 (rule (alexandria:last-elt
                        (nth rule-idx
                             (slot-value agent 'rules)))))
            (cond ((> err 0)
                   (setf
                    (first (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
                    (random-float *rand-gen* (if (<= (- (first rule) delta) 0)
                                                 0
                                                 (- (first rule) delta))
                                  (1- (first rule))))
                   (setf
                    (second (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
                    (random-float *rand-gen* (if (<= (- (second rule) delta) 0)
                                                 0
                                                 (- (second rule) delta))
                                  (1- (second rule)))))
                  ((< err 0)
                   (setf
                    (first (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
                    (random-float *rand-gen*
                                  (1+ (first rule))
                                  (if (>= (+ (first rule) delta) 100)
                                      100
                                      (+ (first rule) delta))))
                   (setf
                    (second (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
                    (random-float *rand-gen*
                                  (1+ (second rule))
                                  (if (>= (+ (second rule) delta) 100)
                                      100
                                      (+ (second rule) delta)))))
                  ((= err 0)))
            (pushnew agent-idx modified-agents)
            (when (>= (length modified-agents)
                      (length agents))
              (return))))
        (let ((err (funcall fitness-fn sim (get-real-data (length sim)))))
          (format t "~a~%" err)
          (if (< err best-error)
              (setf best-error err)
              (return)))
        ;;(setf *cached-agents* (make-hash-table :test #'equal))
        ))))
;; (agents-mf-adjust (extract-agents-from-pool (first *population*)) 1000)
;; (agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 10000)

;; Mixed.
;; (dotimes (_ 10)
;;   (let ((best (agents-best (agents-distribution *population*))))
;;     (agents-mf-adjust (extract-agents-from-pool best) 1)
;;     ;; (agents-brute-force 1 best)
;;     ))
;; (agents-brute-force 1 (agents-best (agents-distribution *population*)))


(defun coco (errors solution)
  (sort
   (remove nil
           (apply #'nconc
                  (maplist (lambda (err)
                             (unless (find (first err)
					   (first solution))
			       ;; (list (alexandria:flatten
			       ;; 	      (list (first solution)
			       ;; 		    (car err)))
			       ;; 	     (reduce #'+ (mapcar #'+ (cadar errs)
			       ;; 				 (cadr err))))
			       ))
                           errors)))
   #'<
   ;; (lambda (elt1 elt2)
   ;;   (< (abs elt1) (abs elt2)))
   :key #'second))

(defun agents-err-adjust (agents-indexes iterations &optional (fitness-fn #'pmape) (delta 3))
  (let* (;; (real-data (rest (get-real-data (length (agent-simulation (first agents))))))
         (errors (mapcar #'list
                         (alexandria:iota (length agents-indexes))
                         ;; (mapcar #'agent-trades agents)
                         (mapcar (lambda (idx)
				   (agents-mape (list idx))
                                   ;; (mapcar #'-
                                   ;;         (agent-simulation agent)
                                   ;;         real-data)
				   )
                                 agents-indexes)))
	 (results (sort errors #'< :key #'second))
         ;; (results (sort (apply #'nconc
         ;;                       (maplist (lambda (errs)
         ;;                                  (mapcar (lambda (err)
         ;;                                            (list (alexandria:flatten
         ;;                                                   (list (caar errs)
         ;;                                                         (car err)))
         ;;                                                  (reduce #'+ (mapcar #'+ (cadar errs)
         ;;                                                                      (cadr err)))))
         ;;                                          (rest errs)))
         ;;                                errors))
	 ;; 		#'<
         ;;                ;; (lambda (elt1 elt2)
         ;;                ;;   (< (abs elt1) (abs elt2)))
         ;;                :key #'second))
	 )
    ;; (dotimes (_ iterations)
    ;;   (setf results (coco errors (first results))))
    ;; results

    ;; (dotimes (_ iterations)
    ;;   (setf results (coco errors (first results))))
    results
    ))

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
  (let ((options '(0.236 0.382 0.5 0.618 1 1.618)))
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
	  (cl21:iota *num-inputs*)
	  ))

(defun gen-rules (max-num-rules)
  ;; (gen-rules 2)
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
                   (sep (/ (+ 100 (access *rules-config* :sd)) max-num-rules))
                   ;; Random starting point for the first membership function of the
                   ;; antecedents.
                   (armf (random-float *rand-gen* 0 sep))
                   ;; Random starting point for the first non-membership function of the
                   ;; antecedents.
                   (arnmf (random-float *rand-gen* 0 sep))
                   ;; Random starting point for the first membership function of the
                   ;; consequents.
                   (crmf (random-float *rand-gen* 0 sep))
                   ;; Random starting point for the first non-membership function of the
                   ;; consequents.
                   (crnmf (random-float *rand-gen* 0 sep))
                   (amf-means (subseq (shuffle
                                       (mapcar (lambda (i)
                                                 (+ (- (* i sep) (access *rules-config* :sd)) armf))
                                               (alexandria:iota max-num-rules)))
                                      0 num-mfs))
                   (anmf-means (subseq
                                (shuffle
                                 (mapcar (lambda (i)
                                           (+ (- (* i sep) (access *rules-config* :sd)) arnmf))
                                         (alexandria:iota max-num-rules)))
                                0 num-mfs))
                   (cmf-means (subseq
                               (shuffle
                                (mapcar (lambda (i)
                                          (+ (- (* i sep) (access *rules-config* :sd)) crmf))
                                        (alexandria:iota max-num-rules)))
                               0 num-mfs))
                   (cnmf-means (subseq
                                (shuffle
                                 (mapcar (lambda (i)
                                           (+ (- (* i sep) (access *rules-config* :sd)) crnmf))
                                         (alexandria:iota max-num-rules)))
                                0 num-mfs)))
              (apply #'nconc
                     (mapcar (lambda (i) `((,(nth i amf-means)
                                             ,(nth i anmf-means)
                                             ,(random-float *rand-gen* 0 1)
                                             )
                                           (,(nth i cmf-means)
                                             ,(nth i cnmf-means)
                                             ,(random-float *rand-gen* 0 1)
                                             )
                                           ))
                             (cl21:iota num-mfs)))))
          (cl21:iota *num-inputs*)))

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

(defun gen-agents (num)
  (mapcar (lambda (_)
	    (make-instance 'agent))
	  (cl21:iota num)))

(defun calc-trade-scale ()
  (/ 1 (/ (avg (mapcar (lambda (next curr)
			 (abs (- (access next :close-bid)
				 (access curr :close-bid))))
		       (rest *rates*)
		       *rates*))
	  *community-size* 10)))
;; (calc-trade-scale)

;; (defun meow (agents iterations)
;;   (let (train-corrects validation-corrects test-corrects
;; 		       (prev-data-count omper:*data-count*)
;; 		       (chunk-count (floor (/ omper:*data-count* *community-size*))))
;;     (dotimes (g iterations)
;;       ;; We'll be working on chunks of `(1+ *community-size*)` size.
;;       ;; This way we don't have to calculate `trades` and `deltas` for all of
;;       ;; the trades each time.
;;       (setf omper:*data-count* (1+ *community-size*))
;;       (dolist (i (shuffle (iota chunk-count)))
;; 	;; Setting correct `*rates*` for the `i`th chunk.
;; 	(setf *rates* (subseq *all-rates* *begin* (- *end* (- chunk-count i))))
;; 	;; Checking if we want to optimize from the head or the tail.
;; 	(let ((i (if (oddp g)
;; 		     (- chunk-count (1+ i))
;; 		     i)))
;; 	  ;; (setf *cached-agents* (make-hash-table :test #'equal))
;; 	  (format t "~a." i)
;; 	  ;; Changing `i`th leverage to 1 so it doesn't affect currect calculation.
;; 	  (dolist (agent-idx agents)
;; 	    (let ((lvg (slot-value (extract-agent-from-pool agent-idx) 'leverage)))
;; 	      (when (< i (length lvg))
;; 		(setf (nth i lvg) 1))))
;; 	  (let* ((reals (get-real-data omper:*data-count*))
;; 		 (trades (mapcar (lambda (agent)
;; 				   ;; (subseq (butlast (agent-trades agent))
;; 				   ;; 	   (* i *community-size*)
;; 				   ;; 	   (* (1+ i) *community-size*))
;; 				   (butlast (agent-trades agent))
;; 				   )
;; 				 (extract-agents-from-pool agents)))
;; 		 (deltas (mapcar (lambda (next curr)
;; 				   (- next curr))
;; 				 (rest reals) reals)
;; 		   ;; (subseq (mapcar (lambda (next curr)
;; 		   ;; 		   (- next curr))
;; 		   ;; 		 (rest reals) reals)
;; 		   ;; 	 (* i *community-size*)
;; 		   ;; 	 (* (1+ i) *community-size*))
;; 		   )
;; 		 (A (magicl:make-complex-matrix (length deltas) (length deltas) (alexandria:flatten trades)))
;; 		 (B (magicl:make-complex-matrix (length deltas) 1 deltas))
;; 		 (sol-matrix (magicl:multiply-complex-matrices
;; 			      (magicl:inv A)
;; 			      B)))

;;             (km (mapcar (lambda (trade delta)
;;                           (concatenate 'list
;;                                        trade
;;                                        (list delta)))
;;                         trades
;;                         deltas)
;;                 2)

;; 	    ;; Modifying leverages.
;; 	    (dotimes (j (length agents))
;; 	      (if (< i (length (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)))
;; 		  (setf (nth i (slot-value (extract-agent-from-pool (nth j agents)) 'leverage))
;; 			(realpart (magicl:ref sol-matrix j 0)))
;; 		  (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
;; 		        (concatenate 'list (copy-list (slot-value (extract-agent-from-pool (nth j agents)) 'leverage))
;; 				     (list (realpart (magicl:ref sol-matrix j 0)))))
;; 		  ))
;; 	    )))

;;       ;; Resetting data-count.
;;       (setf omper:*data-count* prev-data-count)
      
;;       ;; ;; Train error.
;;       ;; ;; (setf *cached-agents* (make-hash-table :test #'equal))
;;       ;; (setf train-corrects
;;       ;; 	    (float (accesses
;;       ;; 		    (agents-test (extract-agents-from-pool agents)
;;       ;; 				 (subseq *all-rates* *begin* *end*))
;;       ;; 		    :performance-metrics :corrects)))

;;       ;; ;; Validation error.
;;       ;; (setf omper:*data-count* (floor (* prev-data-count 0.1)))
;;       ;; ;; (setf *cached-agents* (make-hash-table :test #'equal))
;;       ;; (setf validation-corrects
;;       ;; 	    (float (accesses
;;       ;; 		    (agents-test (extract-agents-from-pool agents)
;;       ;; 				 (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
;;       ;; 		    :performance-metrics :corrects)))


;;       ;; ;; Test error.
;;       ;; ;; (setf omper:*data-count* (ceiling (* prev-data-count 0.1)))
;;       ;; ;; (setf *cached-agents* (make-hash-table :test #'equal))
;;       ;; (setf test-corrects
;;       ;; 	    (float (accesses
;;       ;; 		    (agents-test (extract-agents-from-pool agents)
;;       ;; 				 (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*))))
;;       ;; 		    :performance-metrics :corrects)))
;;       ;; (format t "~%train: ~a~t validation: ~a~t test: ~a~t~%" train-corrects validation-corrects test-corrects)
;;       ;; Removing the least important agent and replacing it
;;       ;; with a random agent.
;;       ;; (let ((idx (largest-number-index
;;       ;; 		  (mapcar (lambda (agent-idx)
;;       ;; 			    (mean (slot-value (extract-agent-from-pool agent-idx) 'leverage)))
;;       ;; 			  agents))))
;;       ;; 	(format t "swapping ~a - " idx)
;;       ;; 	(setf (nth idx agents)
;;       ;; 	      (random-int *rand-gen* 0 (1- *num-pool-agents*))))
;;       )
;;     ;; Resetting `*rates*`.
;;     (setf *rates* (subseq *all-rates* *begin* *end*))
;;     ))

;; (progn
;;   (setf omper:*data-count* 21)
;;   (setf *community-size* 5)
;;   ;; Reset all agents to 1.
;;   (dolist (agent *agents-pool*)
;;     (setf (slot-value agent 'leverage) '(1)))
;;   (meow (first *population*)
;; 	10000))
;; *cached-agents*

;; Get standard deviation of clusters of deltas.
;; (let* ((reals (get-real-data omper:*data-count*))
;;        (deltas (mapcar (lambda (next curr)
;; 			 (- next curr))
;; 		       (rest reals) reals))
;;        (results (make-hash-table :size (floor (/ omper:*data-count* 2)))))
;;   (dotimes (_ 30)
;;     (dotimes (x (1- 70))
;;       (ignore-errors
;; 	(push (standard-deviation (mapcar #'standard-deviation (mapcar #'flatten (km (mapcar #'list deltas) (+ 2 x)))))
;; 	      (gethash (+ 2 x) results))
;; 	)))
;;   (maphash (lambda (k v)
;; 	     (setf (gethash k results)
;; 		   (mean (gethash k results))))
;; 	   results)
;;   (map nil #'print (sort (hash-table-alist results)
;;   	#'< :key #'cdr)))

(defun woof (agents)
  (ignore-errors
    (let* ((reals (get-real-data omper:*data-count*))
           (trades (mapcar (lambda (agent)
                             (butlast (agent-trades agent))
                             )
                           (extract-agents-from-pool agents)))
           (deltas (mapcar (lambda (next curr)
                             (- next curr))
                           (rest reals) reals)))

      ;; Reset all agents' leverages to '(1).
      (dolist (agent *agents-pool*)
	(setf (slot-value agent 'leverage) 1))

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

	(print delta-clusters)

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
                                                    trades))
					   idxs
                                           (reverse (hash-table-values trade-clusters)))))
               (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
               (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
               (sol-matrix (magicl:multiply-complex-matrices
                            (magicl:inv A)
                            B)))
          ;; Modifying leverages.
          (dotimes (j (length agents))
            (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
                  (list (realpart (magicl:ref sol-matrix j 0)))))
          )
        ;; (print (apply #'mapcar (lambda (&rest nums)
        ;;                          (mean nums)
        ;;                          ;; (print nums)
        ;;                          )
        ;;               (hash-table-values trade-clusters)))
        )

      ;; (apply #'mapcar #'list trades)
      ;; trades
      )))

(defun median-elt (sample)
  (nth (floor (/ (length sample) 2))
       (sort (copy-sequence 'list sample) #'<)))

(defun median-elt-idx (sample)
  (position (median-elt sample) sample))

;; (let ((seq '(4 5 1 2 3 6 7 8)))
;;   (median-elt-idx seq))

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
                                   (mapcar (lambda (trades)
                                             (apply #'mapcar (lambda (&rest nums)
                                                               (mean nums))
                                                    (cdr trades)))
                                           (sort (copy-sequence 'list (hash-table-alist trade-clusters)) #'< :key #'first))))
               (A (magicl:make-complex-matrix (length mean-deltas) (length mean-deltas) (flatten mean-trades)))
               (B (magicl:make-complex-matrix (length mean-deltas) 1 mean-deltas))
               (sol-matrix (magicl:multiply-complex-matrices
                            (magicl:inv A)
                            B)))
          ;; Modifying leverages.
          (dotimes (j (length agents))
            (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
                  (realpart (magicl:ref sol-matrix j 0))))
          )))))

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
          ;; Modifying leverages.
          (dotimes (j (length agents))
            (setf (slot-value (extract-agent-from-pool (nth j agents)) 'leverage)
                  (realpart (magicl:ref sol-matrix j 0))))
          )))))

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

;; ;; All communities leverages.
;; (dolist (community *population*)
;;   (dolist (agent-idx community)
;;     (print (slot-value (extract-agent-from-pool agent-idx) 'leverage)))
;;   (terpri))

(progn
  (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (setf omper:*data-count* 101)
  (setf omper:*partition-size* 100)
  (defparameter *community-size* 10
    "Represents the number of agents in an 'individual' or solution. A simulation (a possible solution) will be generated using this number of agents.")
  (defparameter *population-size* 10
    "How many 'communities', 'individuals' or 'solutions' will be participating in the optimization process.")
  (defparameter *begin* (random-int *rand-gen* 0 (- (length *all-rates*) (* *data-count* 3)))
    "The starting timestamp for the data used for training or testing.")
  (defparameter *end* (+ *begin* (* *data-count* 3))
    "The ending timestamp for the data used for training or testing.")
  (defparameter *rates* (subseq *all-rates* *begin* *end*)
    "The rates used to generate the agents' perceptions.")
  (defparameter *num-inputs* 10
    "Keeps track of how many generations have elapsed in an evolutionary process.")
  (defparameter *generations* 0
    "Keeps track of how many generations have elapsed in an evolutionary process.")
  (defparameter *num-pool-agents* 10000
    "How many agents will be created for `*agents-pool*`. Relatively big numbers are recommended, as this increases diversity and does not significantly impact performance.")
  (defparameter *num-rules* 5
    "Represents how many fuzzy rules each of the agents in a solution will have.")
  (defparameter *rules-config* `((:mf-type . :gaussian)
                                 (:sd . 6)
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
  (defparameter *fitness-fn* :pmape))

;; (with-postgres-connection (execute (delete-from :populations)))
;; (with-postgres-connection (execute (delete-from :populations-closure)))
;; (time (train 100000 :fitness-fn #'corrects :sort-fn #'> :save-every 1 :epsilon 1.8))
;; (time (train 100000 :fitness-fn #'mape :sort-fn #'< :save-every 1 :epsilon 0.001))
;; *cached-agents*
;; *population*
;; *generations*

;; (filter-reports (get-reports))

;; Communities' sizes.
;; (mapcar #'length *population*)

;; Report
;; (map nil (lambda (lst) (format t "gen: ~a ~t mape:~a ~t corrects: ~a ~t revenue: ~a~%" (access:access lst :generations) (float (access:access lst :mape)) (* (float (access:access lst :corrects)) 100) (float (access:access lst :revenue)))) (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :creation-time))))))

;; Print metrics.
;; (length (mapcar (lambda (rate)
;; 		  (format t "~f~%" rate))
;; 		;; (butlast (agents-indexes-simulation (agents-best (agents-distribution *population*))))
;; 		(agents-indexes-simulation (agents-best (agents-distribution *population*
;; 									     #'corrects
;; 									     #'>)
;; 							#'>))))
;; (mapcar #'print (rest (get-real-data omper:*data-count*)))

;; Get metrics.
;; (float (corrects (agents-indexes-simulation (agents-best (agents-distribution *population* #'pmape
;; 									      #'<)
;; 							 #'<))
;; 			  (get-real-data omper:*data-count*)))

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
;; (setq *last-id* (train 50000 :starting-population (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :generations)))))) :id) :fitness-fn #'pmape :sort-fn #'<))

;; Init using last entry (generations)
;; (init-from-database (access (first (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:desc :generations)))))) :id))

;; (agents-indexes-simulation (agents-best (agents-distribution *population*)))
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
;; 	(extract-agents-from-pool (nth 8 *population*))))
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

(defun train (generations &key (fitness-fn #'pmape)
                            (sort-fn #'<)
                            (starting-population "")
                            (save-every 100)
			    (epsilon 0.01))
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
	(setf *agents-pool* (gen-agents *num-pool-agents*))
	(setf *population* (gen-communities *community-size* *population-size*))
	(setf *cached-agents* (make-hash-table :test #'equal :size 2000 :synchronized t))
	(setf *generations* 0)
	(setf *fitnesses* nil)
	;; Reset all agents' leverages to 1.
	(dolist (agent *agents-pool*)
	  (setf (slot-value agent 'leverage) 1))
	)
      (init-from-database starting-population))
  (format t "start.~%")
  (let ((parent-id starting-population)
	(can-save? nil))
    (dotimes (_ generations) 
      ;; (when (and (access *rules-config* :mf-mean-adjusting)
      ;; 		 (< (random-float *rand-gen* 0 1) 0.20))
      ;; 	(agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 10))
      (let* ((fitness (agents-reproduce fitness-fn sort-fn)))
        (when (or (null *fitnesses*)
                  (> fitness 0.6)
                  (funcall sort-fn fitness (first *fitnesses*)))
          (when can-save?
            (let* ((child-id (insert-population parent-id "" fitness-fn sort-fn)))
              (insert-initial-closure child-id)
              (insert-closure parent-id child-id)
              (setf parent-id child-id)

              ;; Remove later
              ;; (when (or t (> *generations* 500))
              ;; 	(meow (agents-best (agents-distribution *population*)) 1)
              ;; 	;; (agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 10)
              ;;   ;; (when (= (1- omper:*data-count*) *community-size*)
              ;; 	;;   (agents-brute-force 1 (agents-best (agents-distribution *population*))))
              ;; 	)
              (setf can-save? nil)))
	  
          (push fitness *fitnesses*)
          (format t "~a: ~a~%" *generations* (float fitness)))
	;; Checking if error threshold is met.
	(when (funcall sort-fn fitness epsilon)
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
                ,@(agents-test best *rates*)
                (:rates . ,*rates*)))
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

(defun reset-leverages ()
  (dolist (community *population*)
    (dolist (agent-idx community)
      (setf (slot-value (extract-agent-from-pool agent-idx) 'leverage) 1))))

;; (dolist (agent *agents-pool*)
;;   (print (slot-value agent 'leverage)))

(defparameter *best-community* nil)
(defparameter *best-corrects* 0)
(defun agents-brute-force (sample-size agents)
  (let ((best-community)
	(best-corrects 0))
    (dotimes (i sample-size)
      ;; (setf *cached-agents* (make-hash-table :test #'equal))
      (let (train-corrects validation-corrects test-corrects)
	(let* ( ;; (agents (subseq (alexandria:shuffle
	       ;; 			(alexandria:iota *num-pool-agents*))
	       ;; 		       0 *community-size*))
	       (trades (mapcar (lambda (agent)
				 (butlast (agent-trades agent)))
			       (extract-agents-from-pool agents)))
	       (reals (get-real-data omper:*data-count*))
	       (deltas (mapcar (lambda (next curr)
				 (- next curr))
			       (rest reals) reals))
	       (A (magicl:make-complex-matrix (length deltas) (length deltas) (alexandria:flatten trades)))
	       (B (magicl:make-complex-matrix (length deltas) 1 deltas))
	       (sol-matrix (magicl:multiply-complex-matrices
			    (magicl:inv A)
			    B)))

	  ;; Modifying leverages.
	  (dotimes (i (length agents))
	    (setf (slot-value (extract-agent-from-pool (nth i agents)) 'leverage)
		  (magicl:ref sol-matrix i 0)))

	  (let ((prev-data-count omper:*data-count*))
	    ;; Train error.
	    ;; (setf *cached-agents* (make-hash-table :test #'equal))
	    (setf train-corrects
		  (float (accesses
			  (agents-test (extract-agents-from-pool agents)
				       (subseq *all-rates* *begin* *end*))
			  :performance-metrics :corrects)))


	    ;; Validation error.
	    ;; (setf omper:*data-count* (ceiling (* prev-data-count 0.1)))
	    ;; (setf *cached-agents* (make-hash-table :test #'equal))
	    (setf validation-corrects
		  (float (accesses
			  (agents-test (extract-agents-from-pool agents)
				       (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
			  :performance-metrics :corrects)))


	    ;; Test error.
	    ;; (setf omper:*data-count* (ceiling (* prev-data-count 0.1)))
	    ;; (setf *cached-agents* (make-hash-table :test #'equal))
	    (setf test-corrects
		  (float (accesses
			  (agents-test (extract-agents-from-pool agents)
				       (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*))))
			  :performance-metrics :corrects)))

	    ;; Resetting data-count.
	    (setf omper:*data-count* prev-data-count))
	  ;; (print (magicl:det A))

	  ;; Resetting leverages back to 1.
	  (dotimes (i (length agents))
	    (setf (slot-value (extract-agent-from-pool (nth i agents)) 'leverage)
		  1))

	  (when (and (/= (realpart (magicl:det A)) 0)
		     (> validation-corrects *best-corrects*))
	    (format t "new best found.~%")
	    (setf best-community agents)
	    (setf best-corrects validation-corrects)
	    (setf *best-corrects* validation-corrects)
	    ;; Later remove this.
	    (setf *best-community* agents)
	    )

	  (format t "train: ~a~t validation:~a~t test:~a~t~%" train-corrects validation-corrects test-corrects)
	  ;; `((:train . ,train-corrects) (:vtest . ,validation-corrects) (:atest . ,test-corrects))
	  )))
    best-community))
;; (agents-brute-force 1 (agents-best (agents-distribution *population*)))

(defun agents-random-search (num-agents iterations)
  (let (best)
    (dotimes (_ iterations)
      (let ((community (subseq (shuffle (iota *num-pool-agents*)) 0 num-agents)))
	(woof community)
	(when (or (null best)
		  (< (agents-pmape community)
		     (agents-pmape best)))
	  (print (agents-pmape community))
	  (setf best community))))
    best))
;; (agents-random-search 10 1000)
;; *cached-agents*

;; (reset-leverages)

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

(defun agent-perception-percentages (agent)
  "TODO: `agent` is unused"
  (reverse
   (remove nil
	   (maplist (lambda (rates)
		      (when (>= (length rates) (1+ *num-inputs*))
			(let ((percentages (mapcar (lambda (rate)
						     (+ 50
							(* 1000
							   (/ (- (first rates)
								 rate)
							      (first rates)))))
						   (rest (subseq rates 0 (1+ *num-inputs*)))
						   )))
			  (reverse (push (first rates)
					 percentages)))
			))
		    (reverse (get-real-data (+ omper:*data-count* *num-inputs*)))))))

(defun agent-moving-average (agent)
  "TODO: `agent` is unused"
  (mapcar (lambda (mas rate)
	     (append
	      (mapcar (lambda (ma)
			(+ 50
			   (* 1000
			      (/ (- (first rate) ma)
				 (first rate)))))
		      mas)
	      rate))
	   (apply #'mapcar #'list
		  (mapcar (lambda (i)
			    (get-moving-average i))
			  (iota *num-inputs* :start 10 :step 5)))
	   (mapcar #'list (get-real-data omper:*data-count*))))

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
			     (let* ((high (maximum history :key key-high))
				    (low (minimum history :key key-low))
				    (close (funcall key-close (elt history 0)))
				    (%k (* 100 (/ (- close low)
						  (- high low))))
				    (%d (funcall %d-history %k)))
			       (list %k %d))))))

(defun get-reports ()
  (let* ((omper:*data-count* (ceiling (* omper:*data-count* 0.1))))
    (mapcar (lambda (db-pop)
	      (let* ((validation (market-report db-pop
						*instrument* *timeframe*
						(subseq *all-rates* *begin* (+ *end* omper:*data-count*))))
		     (test (market-report db-pop
					  *instrument* *timeframe*
					  (subseq *all-rates* *begin* (+ *end* (* 2 omper:*data-count*)))))
		     (result `((:train . ((:mape . ,(float (accesses test :training :performance-metrics :mape)))
					  (:corrects . ,(float (accesses test :training :performance-metrics :corrects)))))
			       (:validation . ((:mape . ,(float (accesses validation :testing :performance-metrics :mape)))
					       (:corrects . ,(float (accesses validation :testing :performance-metrics :corrects)))))
			       (:test . ((:mape . ,(float (accesses test :testing :performance-metrics :mape)))
					 (:corrects . ,(float (accesses test :testing :performance-metrics :corrects))))
				      ))))
		(print result)))
	    (retrieve-all (select (:*)
			    (from :populations)
			    (order-by (:desc :creation-time))
			    (limit 1000))))))

(defun run-random-rates (iterations)
  (dotimes (_ iterations)

    (defparameter *begin* (random-int *rand-gen* 0 (- (length *all-rates*) (* *data-count* 3)))
      "The starting timestamp for the data used for training or testing.")
    (defparameter *end* (+ *begin* (* *data-count* 3))
      "The ending timestamp for the data used for training or testing.")
    (defparameter *rates* (subseq *all-rates* *begin* *end*)
      "The rates used to generate the agents' perceptions.")
      
    (train 1000 :fitness-fn #'corrects :sort-fn #'> :save-every 1 :epsilon 1.6))

  (filter-reports (get-reports)))

(defun filter-reports (reports)
  (let ((train-mape 0)
	(train-corrects 0)
	(val-mape 0)
	(val-corrects 0)
	(test-mape 0)
	(test-corrects 0)
	(len 0))
    (dolist (report reports)
      (when (and (> (accesses report :train :corrects) 0.6)
		 (< (accesses report :train :mape) 0.03)
		 (> (accesses report :validation :corrects) 0.5)
		 (< (accesses report :validation :mape) 0.03))
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
	(:test-corrects . ,(float (/ test-corrects len)))))
    ))

;; (defparameter *results* (run-random-rates 10))
;; *generations*

(defun move-somewhere ()
  ;; Print prices.
  ;; (mapcar (lambda (rate)
  ;; 	    (format t "~f~%" rate))
  ;; 	  (accesses (second *market-validations*) :testing :corrects))
  ;; (accesses (second *market-validations*) :testing :performance-metrics)
  ;; (mapcar (lambda (rate)
  ;; 	    (format t "~f~%" (access rate :close-bid)))
  ;; 	  (last (accesses (second *market-validations*) :testing :rates)
  ;; 		(floor (* omper:*data-count* 0.1))))

  (mapcar (lambda (test)
	    (list (float (accesses test :training :performance-metrics :corrects))
		  (float (accesses test :testing :performance-metrics :corrects))))
	  *market-validations*)

  ;; ;; Check correlation.
  ;; (ql:quickload :cl-mathstats)
  (defparameter *tests-corrects*
    (remove-if (lambda (elt)
  		 (< (first elt) 0.55))
  	       (mapcar (lambda (test)
  			 (list (float (accesses test :training :performance-metrics :corrects))
  			       (float (accesses test :testing :performance-metrics :corrects))))
  		       *market-validations*)))
  ;; (mean (mapcar #'second *tests-corrects*))
  (cl-mathstats:correlation (mapcar (lambda (test)
  				      (float (accesses test :training :performance-metrics :corrects)))
  				    *market-tests*)
  			    (mapcar (lambda (test)
  				      (float (accesses test :testing :performance-metrics :corrects)))
  				    *market-tests*))
  )

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
         (real (get-real-data (length sim)))
         
         (mae (mae sim real))
	 (mse (mse sim real))
	 (rmse (rmse sim real))
	 (mape (mape sim real))
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

(defun agents-ifs (params stdev)
  (ifs (gaussian-mf (nth 0 params) stdev 0 (- 1 (nth 2 params)))
       (gaussian-nmf (nth 1 params) stdev 0 (nth 2 params))))

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
  (let ((sim (apply #'mapcar (lambda (&rest trades)
			       (apply #'+ trades))
		    (mapcar (lambda (agent)
			      (agent-trades agent))
			    (extract-agents-from-pool agents-indexes)))))
    (mapcar (lambda (sim real)
	      ;; summing the current price + the sum of all trades to obtain next price
	      ;; (mse) is in charge of comparing this price against the next one
	      (+ sim real))
	    ;; summation of trades of every agent in `agents`
	    sim
	    (get-real-data (length sim)))))

(defun agents-indexes-simulation (agents-indexes)
  "Returns a simulation of multiple agents trading a dataset. The agents are
extracted from `*agents-pool*` using the indexes stored in `agents-indexes`."
  ;; (agents-indexes-simulation (first *population*))
  ;; (woof agents-indexes)
  (agents-simulation (extract-agents-from-pool agents-indexes)))

(defun agents-simulation (agents)
  "Returns a simulation of multiple agents trading a dataset."
  (let ((sim (apply #'mapcar (lambda (&rest trades)
			       (apply #'+ trades))
		    (mapcar (lambda (agent)
			      (agent-trades agent))
			    agents))))
    (mapcar (lambda (sim real)
	      ;; summing the current price + the sum of all trades to obtain next price
	      ;; (mse) is in charge of comparing this price against the next one
	      (+ sim real))
	    ;; summation of trades of every agent in `agents`
	    sim
	    (get-real-data (length sim)))))

(defun agent-simulation (agent)
  "Returns a simulation of a single agent trading a dataset."
  (let ((deltas (agent-trades agent)))
    (mapcar #'+
	    deltas
	    (get-real-data (length deltas)))))

;; (length (agent-simulation (first *agents-pool*)))

(defun agents-mutate (&optional (chance 0.2))
  "Mutates the pool of agents if a according to `chance`."
  ;; (agents-mutate 0.5)

  ;; (let* ((agents (alexandria:random-elt *population*))
  ;; 	 (trades (mapcar (lambda (agent)
  ;; 			   (butlast (agent-trades agent)))
  ;; 			 (extract-agents-from-pool agents)))
  ;; 	 (reals (get-real-data omper:*data-count*))
  ;; 	 (deltas (mapcar #'- (rest reals) reals))
  ;; 	 (A (magicl:make-complex-matrix (length deltas) (length deltas) (alexandria:flatten trades)))
  ;; 	 (B (magicl:make-complex-matrix (length deltas) 1 deltas))
  ;; 	 (sol-matrix (magicl:multiply-complex-matrices
  ;; 		      (magicl:inv A)
  ;; 		      B)))

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
  (when (> chance (random-float *rand-gen* 0 1.0))
    (let ((community-idx (position (agents-worst (agents-distribution *population*))
  				   *population* :test #'equal)))
      (setf (nth (random-int *rand-gen* 0 (1- (length (nth community-idx *population*))))
		 (nth community-idx *population*))
	    (random-int *rand-gen* 0 (1- *num-pool-agents*))))))

(defun agents-profit (agents-indexes)
  "Uses the market simulation created by the agents represented by `agents-indexes` to generate a list of profits generated by comparing the simulation against the real market data."
  ;; (agents-profit '(1 2 4))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (revenue sim (get-real-data (length sim)))))

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
    (revenue sim (get-real-data (length sim)))))

(defun agents-corrects (agents-indexes)
  "Returns the number of correct direction predictions made by the agents in
`agents-indexes` for the real prices."
  ;; (agents-corrects (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (corrects sim (get-real-data (length sim)))))

(defun agents-pmape (agents-indexes)
  "Returns the penalized mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (pmape sim (get-real-data (length sim)))))

(defun agents-mape (agents-indexes)
  "Returns the mean absolute percentage error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mape sim (get-real-data (length sim)))))

(defun agents-rmse (agents-indexes)
  "Returns the root mean square error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (rmse sim (get-real-data (length sim)))))

(defun agents-mae (agents-indexes)
  "Returns the mean absolute error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mae sim (get-real-data (length sim)))))

(defun agents-mse (agents-indexes)
  "Returns the mean squared error obtained by the agents' simulation in
`agents-indexes` for the real prices."
  ;; (agents-mse (first *population*))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    (mse sim (get-real-data (length sim)))))

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

(defun get-real-data (count)
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 count)))

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

(defun insert-population (parent-id &optional label (fitness-fn #'pmape) (sort-fn #'<))
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
			 :corrects (float (agents-corrects best))
			 :revenue (float (agents-revenue best))
			 ))))
    id))
