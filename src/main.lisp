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
        :computable-reals
	:random-state
	:overmind-code
	:overmind-input
	:overmind-perception
	:overmind-intuition
	:overmind-agents.config
	:overmind-agents.db))
(in-package :overmind-agents)

(progn
  (defparameter *instrument* :EUR_USD
    "The financial instrument used for querying the data used to train or test.")
  (defparameter *timeframe* :D
    "The timeframe used for querying the data used to train or test.")
  (defparameter *train-size* 200
    "Size of the dataset used for the training stage.")
  (defparameter *test-size* 50
    "Size of the dataset used for the testing stage.")
  (defparameter *begin* 300
    "The starting timestamp for the data used for training or testing.")
  (defparameter *end* 2300
    "The ending timestamp for the data used for training or testing.")
  (defparameter *all-rates* (get-rates *instrument* 1 *timeframe*)
    )
  (defparameter *rates* (subseq *all-rates* *begin* *end*)
    "The rates used to generate the agents' perceptions."))

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
    (setf *cached-agents* (make-hash-table :test #'equal))
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

(defun pmape (sim real)
  "Penalized MAPE. If a simulation has a different direction than that of the real price, it gets penalized with a weight."
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
       (length (rest real)))))

(defun mape (sim real)
  "Mean absolute percentage error between a simulated time series `sim` and a real time series `real`."
  (/ (reduce #'+ (mapcar (lambda (s r)
                           (abs (/ (- r s) r)))
                         sim
                         (rest real)))
     (length (rest real))))

(defun mse (sim real)
  "Mean squared error between a simulated time series `sim` and a real time series `real`."
  (/ (reduce #'+ (mapcar (lambda (elt) (expt elt 2)) (mapcar #'- sim (rest real))))
     (length (rest real))))

(defun mae (sim real)
  "Mean absolute error between a simulated time series `sim` and a real time
series `real`."
  (/ (reduce #'+ (mapcar (lambda (s r)
                           (abs (- s r)))
                         sim
                         (rest real)))
     (length (rest real))))

(defun rmse (sim real)
  "Root mean square error between a simulated time series `sim` and a real time
series `real`."
  (sqrt (mse sim real)))

(defun revenue (sim real)
  "Currently it returns the agent profit at each trade."
  (let ((sim sim)
	(real (rest real))
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

(defun corrects (sim real)
  "How many trades were correct."
  ;; (corrects (agents-indexes-simulation (agents-best (agents-distribution *population*))) (get-real-data))
  (let ((sim sim)
	(real (rest real))
	;; we only need the real previous, as the simulated is based on the last real price at every moment
	(prev (first real)))
    (/ (apply #'+ (mapcar (lambda (s r)
			    (let ((real-dir (- r prev))
				  (sim-dir (- s prev)))
                              ;; (format t "prev: ~a~t real: ~a~t sim: ~a~t real-dir: ~a~t sim-dir: ~a~t~%" prev r s real-dir sim-dir)
			      (setq prev r)
			      (if (> (* real-dir sim-dir) 0)
				  1
				  0)))
			  sim
			  real))
       (length real))))

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

(defun shuffle (sequence)
  (loop for i from (length sequence) downto 2
     do (rotatef (elt sequence (random-int *rand-gen* 0 (1- i)))
                 (elt sequence (1- i))))
  sequence)

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
   ;; (leverage :initarg :leverage :initform (random-float *rand-gen* 0 1))
   (stdev :initarg :stdev :initform (random-float *rand-gen* 2 30))
   ;; (stdev :initarg :stdev :initform (access *rules-config* :sd))
   ))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules leverage stdev))

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
                (list (cl21:nth z (if (< (- index 4) 0) 0 (- index 4)))
		      (cl21:nth z (if (< (- index 3) 0) 1 (- index 3)))
		      (cl21:nth z (if (< (- index 2) 0) 2 (- index 2)))
                      (cl21:nth z (if (< (- index 1) 0) 3 (- index 1)))
                      (cl21:nth z index)
                      close
                      ))))
          ;; TODO: generalize (get-data) so it doesn't need an `instrument`.
          (get-data *instrument* *rates* :levels (slot-value agent 'beliefs))))

(defun agent-trades (agent)
  "Creates a simulation of a single agent's trades."
  ;; (time (length (agent-trades (make-instance 'agent))))
  ;; (time (agent-trades (first (extract-agents-from-pool '(2)))))
  (let ((sig (reduce #'+
		     (append (alexandria:flatten (slot-value agent 'beliefs))
			     (alexandria:flatten (slot-value agent 'rules))
                             ;; (list (slot-value agent 'leverage))
                             ;; (list (slot-value agent 'stdev))
                             ;; (mapcar (lambda (rate)
                             ;;           (access rate :close-bid))
                             ;;         *rates*)
                             ))))
    (if (cl21:gethash *cached-agents* sig)
	(cl21:gethash *cached-agents* sig)
	(let* ((data (agent-perception agent))
               (closes (mapcar #'alexandria:last-elt data)))
	  (let ((sim (ifis-agents data
                                  (slot-value agent 'rules)
                                  (slot-value agent 'leverage)
                                  (slot-value agent 'stdev))))
	    (setf (cl21:gethash *cached-agents* sig) sim)
	    sim)))))

(defun agents-fitness (agents-indexes &optional (fitness-fn #'mape))
  (let ((sim (agents-indexes-simulation agents-indexes)))
    ;; (mse sim (get-real-data 3))
    (funcall fitness-fn sim (get-real-data (length sim)))))

(defun ifis-agents (inputs rules leverage stdev)
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par stdev))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (max (nth 0 inp) (nth 1 inp) (nth 2 inp) (nth 3 inp) (nth 4 inp))))
		(process-agent-output
		 (if-coa
		  (reduce #'ifunion ;; or
			  (mapcar (lambda (ifs)
				    (reduce #'ifintersection ;; and
					    (list (rule (* 100 (/ (nth 0 inp) (+ 1 mx-inp)))
							(nth 0 ifs)
							(nth 1 ifs))
						  (rule (* 100 (/ (nth 1 inp) (+ 1 mx-inp)))
							(nth 2 ifs)
							(nth 3 ifs))
						  (rule (* 100 (/ (nth 2 inp) (+ 1 mx-inp)))
							(nth 4 ifs)
							(nth 5 ifs))
						  (rule (* 100 (/ (nth 3 inp) (+ 1 mx-inp)))
							(nth 6 ifs)
							(nth 7 ifs))
						  (rule (* 100 (/ (nth 4 inp) (+ 1 mx-inp)))
							(nth 8 ifs)
							(nth 9 ifs)))))
				  agent-ifss)))
                 leverage)))
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
  (dotimes (i (floor (/ (length *population*) 2)))
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
          ))))

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

(defun agents-mf-adjust (agents iterations &optional (fitness-fn #'pmape) (delta 3))
  (dotimes (_ iterations)
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
		  (random-int *rand-gen* (if (<= (- (first rule) delta) 0)
					     0
					     (- (first rule) delta))
			      (1- (first rule))))
		 (setf
		  (second (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
		  (random-int *rand-gen* (if (<= (- (second rule) delta) 0)
					     0
					     (- (second rule) delta))
			      (1- (second rule)))))
		((< err 0)
		 (setf
		  (first (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
		  (random-int *rand-gen*
			      (1+ (first rule))
			      (if (>= (+ (first rule) delta) 100)
				  100
				  (+ (first rule) delta))))
		 (setf
		  (second (alexandria:last-elt (nth rule-idx (slot-value agent 'rules))))
		  (random-int *rand-gen*
			      (1+ (second rule))
			      (if (>= (+ (second rule) delta) 100)
				  100
				  (+ (second rule) delta)))))
		((= err 0)))
	  (pushnew agent-idx modified-agents)
	  (when (>= (length modified-agents)
		    (length agents))
	    (return))))
      ;; (print (funcall fitness-fn sim (get-real-data (length sim))))
      ;;(setf *cached-agents* (make-hash-table :test #'equal))
      )))


(defun coco (errors solution)
  (sort
   (remove nil
           (apply #'nconc
                  (maplist (lambda (errs)
                             (mapcar (lambda (err)
                                       (unless (find (first err)
                                                     (first solution))
                                         (list (alexandria:flatten
                                                (list (first solution)
                                                      (car err)))
                                               (reduce #'+ (mapcar #'+ (cadar errs)
                                                                   (cadr err))))))
                                     errs))
                           errors)))
   (lambda (elt1 elt2)
     (< (abs elt1) (abs elt2)))
   :key #'second))

(defun agents-err-adjust (agents iterations &optional (fitness-fn #'pmape) (delta 3))
  (let* ((real-data (rest (get-real-data (length (agent-simulation (first agents))))))
         (errors (mapcar #'list
                         (alexandria:iota (length agents))
                         ;; (mapcar #'agent-trades agents)
                         (mapcar (lambda (agent)
                                   (mapcar #'-
                                           (agent-simulation agent)
                                           real-data))
                                 agents)))
         (results (sort (apply #'nconc
                               (maplist (lambda (errs)
                                          (mapcar (lambda (err)
                                                    (list (alexandria:flatten
                                                           (list (caar errs)
                                                                 (car err)))
                                                          (reduce #'+ (mapcar #'+ (cadar errs)
                                                                              (cadr err)))))
                                                  (rest errs)))
                                        errors))
                        (lambda (elt1 elt2)
                          (< (abs elt1) (abs elt2)))
                        :key #'second)))
    (dotimes (_ iterations)
      (setf results (coco errors (first results))))
    results
    ))

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

;; (agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 10000)
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
  ;; (gen-rules 1)
  (mapcar (lambda (_)
	    (mapcar (lambda (_) `(,(random-float *rand-gen* 0 100)
				   ,(random-float *rand-gen* 0 100)
				   ,(random-float *rand-gen* 0 1)
				   ;; 0
				   ))
		    (cl21:iota 10)))
	  (cl21:iota num-rules)))

(defun gen-agents (num)
  (mapcar (lambda (_)
	    (make-instance 'agent))
	  (cl21:iota num)))

(progn
  (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (defparameter *rules-config* '((:mf-type . :gaussian)
                                 (:sd . 10)
                                 (:mf-mean-adjusting . nil)
                                 (:nmf-height-style . :complement)
                                 (:trade-scale . 3000))
    "Configuration used to create the rules of the agents for a population.")
  (defparameter *generations* 0
    "Keeps track of how many generations have elapsed in an evolutionary process.")
  (defparameter *num-pool-agents* 1000
    "How many agents will be created for `*agents-pool*`. Relatively big numbers are recommended, as this increases diversity and does not significantly impact performance.")
  (defparameter *num-rules* 3
    "Represents how many fuzzy rules each of the agents in a solution will have.")
  (defparameter *agents-pool* (gen-agents *num-pool-agents*)
    "Instances of `agent` that are available to create solutions.")
  (defparameter *community-size* 10
    "Represents the number of agents in an 'individual' or solution. A simulation (a possible solution) will be generated using this number of agents.")
  (defparameter *population-size* 20
    "How many 'communities', 'individuals' or 'solutions' will be participating in the optimization process.")
  (defparameter *population* (gen-communities *community-size* *population-size*)
    "Represents a list of lists of indexes to *agents-pool*.")
  (defparameter *cached-agents* (make-hash-table :test #'equal)
    "Used for memoizing an agent's simulation.")
  (defparameter *fitnesses* nil
    "List of fitnesses obtained after evolving a population.")
  (defparameter *fitness-fn* :pmape))

;; (setf *population* (gen-communities *community-size* 1))

;; (defparameter *best* (extract-agents-from-pool (nth 0 *population*)))

;; (agents-test *best* (subseq *all-rates* *begin* *end*))
;; (agents-test (extract-agents-from-pool (nth 0 *population*)) (subseq *all-rates* *begin* *end*))

;; (dotimes (_ 10000)
;;   (let* ((agents (nth 0 *population*))
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

;;     (let ((leverages (mapcar (lambda (i)
;; 			       (abs (magicl:ref sol-matrix i 0)))
;; 			     (alexandria:iota (length agents))))
;; 	  (n 1))
;;       (dotimes (x n)
;; 	(let ((idxs (append (subseq (largest-number-indexes leverages) 0 n)
;; 			    (last (largest-number-indexes leverages) n)))
;; 	      ;; (idxs (last (largest-number-indexes leverages) n))
;; 	      ;; (idxs (subseq (largest-number-indexes leverages) 0 n))
;; 	      )
;; 	  (setf (nth (nth x idxs)
;; 		     agents)
;; 		(random-int *rand-gen* 0 (1- *num-pool-agents*))))))

;;     (let ((best-error (accesses (agents-test *best*
;; 					     (subseq *all-rates* *begin* *end*))
;; 				:performance-metrics :mape))
;; 	  (current-error (accesses (agents-test (extract-agents-from-pool agents)
;; 						(subseq *all-rates* *begin* *end*))
;; 				   :performance-metrics :mape)))
;;       (when (or t (< current-error best-error))
;; 	(setf *best* (extract-agents-from-pool agents))
;; 	(format t "current: ~a~t best: ~a~%" current-error best-error)
;; 	;; (print (float current-error))
;; 	))
;;     ))

(defun train (generations &key (fitness-fn #'pmape)
                            (sort-fn #'<)
                            (starting-population "")
                            (save-every 100))
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
	(setf *cached-agents* (make-hash-table :test #'equal))
	(setf *generations* 0)
	(setf *fitnesses* nil))
      (init-from-database starting-population))
  (let ((parent-id starting-population)
	(can-save? nil))
    (dotimes (_ generations)
      (when (and (access *rules-config* :mf-mean-adjusting)
		 (< (random-float *rand-gen* 0 1) 0.20))
	(agents-mf-adjust (extract-agents-from-pool (agents-best (agents-distribution *population*))) 1))
      (let ((fitness (agents-reproduce fitness-fn sort-fn)))
	(when (or (null *fitnesses*)
		  (funcall sort-fn fitness (first *fitnesses*)))
	  (when can-save?
	    (let* ((child-id (insert-population parent-id)))
	      (insert-initial-closure child-id)
	      (insert-closure parent-id child-id)
	      (setf parent-id child-id)
	      (setf can-save? nil)))
	  
	  (push fitness *fitnesses*)
	  (format t "~a: ~a~%" *generations* fitness)
	  ))
      
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

;; (with-postgres-connection (execute (delete-from :populations)))
;; (with-postgres-connection (execute (delete-from :populations-closure)))

;; (setq *last-id* (train 1000 :fitness-fn #'pmape :sort-fn #'<))

;; (setq *last-id* (train 50000 :starting-population *last-id* :fitness-fn #'pmape :sort-fn #'<))
;; (setq *last-id* (train 50000 :starting-population "6DF6BB62-B9AB-4A6B-B6A8-9EF44EFD86FB" :fitness-fn #'pmape :sort-fn #'<))

;; (map nil (lambda (lst) (format t "gen: ~a ~tcorrects: ~a ~t revenue: ~a~%" (access:access lst :generations) (* (float (access:access lst :corrects)) 100) (float (access:access lst :revenue)))) (with-postgres-connection (retrieve-all (select (:*) (from :populations) (order-by (:asc :creation-time))))))

;; continue from database
;; (setq *last-id* (train 50000 :starting-population (access (first (with-postgres-connection (retrieve-all (select (:id :pmape) (from :populations) (order-by (:asc :pmape)))))) :id) :fitness-fn #'pmape :sort-fn #'<))

;; (get-ancestors *last-id*)
;; (access:access (get-population *last-id*) :generations)

;; (length (with-postgres-connection (retrieve-all (select :* (from :populations-closure)))))

;; (access (first (with-postgres-connection (retrieve-all (select (:id :pmape) (from :populations) (order-by (:asc :pmape)))))) :id)

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
  (setf *cached-agents* (make-hash-table :test #'equal))
  (let* ((*instrument* instrument)
	 (*timeframe* timeframe)
	 (*rates* rates)
	 ;; (db-pop (get-most-relevant-population instrument timeframe))
	 (pop (decompress-object (access db-pop :population)))
	 (best (nth (access db-pop :best-index) pop))
	 (sim (agents-simulation best))
	 (next-sim-price (alexandria:last-elt sim)))
    (format t "(length sim) = ~a" (length sim))
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

(defun agents-brute-force (sample-size)
  (mapcar
   (lambda (_)
     (let (train-corrects before-corrects test-corrects)
       ;; (defparameter *test-agents* (gen-agents 1000))
       ;; (setf *agents-pool* *test-agents*)
       ;; (setf *population* (list (first (first (agents-err-adjust *test-agents* (- omper:*data-count* 3))))))
       ;; (setf *population* (list (first (first (agents-err-adjust *test-agents* 0)))))
       
       ;; (setf *population* (subseq (mapcar #'first (agents-err-adjust *test-agents* *community-size*)) 0 *population-size*))

       ;; Set population as only the best.
       ;; (setf *population* (list (agents-best (agents-distribution *population*))))

       (dotimes (agents-idx (length *population*))
	 ;; Reset the leverages to 1
         (reset-leverages)
	 ;; (setf *cached-agents* (make-hash-table :test #'equal))
	 
	 (let* ((agents (nth agents-idx *population*))
		(trades (mapcar (lambda (agent)
				  (butlast (agent-trades agent)))
				(extract-agents-from-pool agents)))
		(reals (get-real-data omper:*data-count*)
		  ;; (mapcar (lambda (rate)
		  ;; 	   (access rate :close-bid))
		  ;; 	 (reverse (subseq (reverse (subseq *all-rates* (1+ *begin*) *end*)) 0 (1+ omper:*data-count*))))
		  )
		(deltas (mapcar #'- (rest reals) reals))
		(A (magicl:make-complex-matrix (length deltas) (length deltas) (alexandria:flatten trades)))
		(B (magicl:make-complex-matrix (length deltas) 1 deltas))
		(sol-matrix (magicl:multiply-complex-matrices
			     (magicl:inv A)
			     B)))

	   ;; (setf *cached-agents* (make-hash-table :test #'equal))
	   ;; Before adjusting error, testing stage.
	   (push (print (float (accesses
                                (agents-test (extract-agents-from-pool agents)
                                             (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
                                :performance-metrics :corrects)))
		 before-corrects)

	   ;; Modifying leverages.
	   (dotimes (i (length agents))
	     (setf (slot-value (extract-agent-from-pool (nth i agents)) 'leverage)
	   	   (realpart (magicl:ref sol-matrix i 0))))
	   ;; (print (magicl:det A))
	   
           ;; (setf *cached-agents* (make-hash-table :test #'equal))
           ;; Train error.
           (push (float (accesses
			 (agents-test (extract-agents-from-pool agents)
				      (subseq *all-rates* *begin* *end*))
			 :performance-metrics :corrects))
                 train-corrects)

           ;; (setf *cached-agents* (make-hash-table :test #'equal))
           ;; Test error.
           (push (float (accesses
			 (agents-test (extract-agents-from-pool agents)
				      (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
			 :performance-metrics :corrects))
                 test-corrects)

           (print train-corrects)
	   (print before-corrects)
           (print test-corrects)
           (print "")
	   ))
       
       ;; (slot-value (extract-agent-from-pool (first (first *population*))) 'leverage)
       
       ;; ;; Calculating the number of correct trades during training stage.
       ;; (push (float
       ;;        (/ (reduce #'+
       ;;  		 (mapcar (lambda (i)
       ;;  			   (accesses
       ;;  			    (agents-test (extract-agents-from-pool (nth i *population*))
       ;;  					 (subseq *all-rates* *begin* *end*))
       ;;  			    :performance-metrics :mse))
       ;;  			 (alexandria:iota (length *population*))))
       ;;  	 (length *population*)))
       ;;       train-corrects
       ;;       )

       ;; ;; We have to reset `*cached-agents*` if we want to test them with other
       ;; ;; `*rates*`.
       ;; (setf *cached-agents* (make-hash-table :test #'equal))

       ;; ;; Calculating the number of correct trades during testing stage.
       ;; (setf test-corrects
       ;;       (float
       ;;        (/ (reduce #'+
       ;;  		 (mapcar (lambda (i)
       ;;  			   (accesses
       ;;  			    (agents-test (extract-agents-from-pool (nth i *population*))
       ;;  					 (subseq *all-rates* *begin* (+ *end* omper:*data-count*)))
       ;;  			    :performance-metrics :corrects))
       ;;  			 (alexandria:iota (length *population*))))
       ;;  	 (length *population*))))

       (print (list train-corrects before-corrects test-corrects))
       ))
   (alexandria:iota sample-size)))
;; (agents-brute-force 1)

(defun agents-random-search ()
  (let (best)
    (dotimes (_ 20)
      (let ((community (mapcar (lambda (_)
				 (alexandria:random-elt (alexandria:iota *num-pool-agents*)))
			       (alexandria:iota omper:*data-count*))))
	(when (or (null best)
		  (< (agents-pmape community)
		     (agents-pmape best)))
	  (print (agents-pmape community))
	  (setf best community))))
    best))

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

(defun move-somewhere ()
  (defparameter *market-report* (market-report (get-most-relevant-population *instrument* *timeframe*)
                                               *instrument* *timeframe*
                                               (subseq *all-rates* *begin* (+ *end* omper:*data-count*))))
  ;; (init-from-database (access (get-most-relevant-population *instrument* *timeframe*) :id))
  (list (float (accesses *market-report* :training :performance-metrics :corrects))
        (float (accesses *market-report* :testing :performance-metrics :corrects)))
  ;; '(0.7241379 0.55172414)
  (defparameter *market-tests* (mapcar (lambda (db-pop)
                                         (market-report db-pop
                                                        *instrument* *timeframe*
                                                        (subseq *all-rates* *begin* (+ *end* omper:*data-count*))))
                                       (retrieve-all (select (:*)
						       (from :populations)
						       (order-by (:asc :creation-time))
						       (limit 30)))))

  (mapcar (lambda (test)
            (list (float (accesses test :training :performance-metrics :corrects))
                  (float (accesses test :testing :performance-metrics :corrects))))
          *market-tests*))

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
	 (corrects (corrects sim real))
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
  (* (/ (- (* out 2) 100) (access *rules-config* :trade-scale)) leverage))
;; (process-agent-output 100 1)

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
      (let* ( ;; (x (alexandria:shuffle x))
             ;; (y (alexandria:shuffle y))
             (site (random-int *rand-gen* 1 (1- *community-size*)))
	     (x1 (subseq x 0 site))
	     (x2 (nthcdr site x))
	     (y1 (subseq y 0 site))
	     (y2 (nthcdr site y)))
	(let ((newx (append x1 y2))
	      (newy (append y1 x2)))
	  (values (if (= (length (remove-duplicates newx))
			 *community-size*)
		      newx
		      x)
		  (if (= (length (remove-duplicates newy))
			 *community-size*)
		      newy
		      y))))
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

(defun agents-mutate (&optional (chance 0.9))
  "Mutates the pool of agents if a according to `chance`."
  ;; (agents-mutate 0.5)
  (when (> chance (random-float *rand-gen* 0 1.0))
    (let ((community (nth (random-int *rand-gen* 0 (1- *population-size*)) *population*))
	  (mutation (random-int *rand-gen* 0 (1- *num-pool-agents*))))
      (unless (find mutation community)
	(setf (nth (random-int *rand-gen* 0 (1- *community-size*))
		   community)
	      mutation)))
    ))

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

(defun insert-population (parent-id &optional label)
  (let* ((best (agents-best (agents-distribution *population*)))
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
			 :mape (agents-mape best)
			 :pmape (agents-pmape best)
			 :rmse (agents-rmse best)
			 :mae (agents-mae best)
			 :mse (agents-mse best)
			 :corrects (agents-corrects best)
			 :revenue (agents-revenue best)
			 ))))
    id))
