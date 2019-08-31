;; (ql:quickload :overmind-intuition)
;; (ql:quickload :lparallel)
;; (ql:quickload :random-state)
;; (ql:quickload :cl21)
;; (ql:quickload :marshal)
;; (ql:quickload :cl-json)
;; (ql:quickload :dexador)
(defpackage overmind-agents
  (:use :cl
 	:lparallel
	:random-state
	:overmind-code
	:overmind-input
	:overmind-perception
	:overmind-intuition
	:overmind-agents.config))
(in-package :overmind-agents)

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
	    (mapcar (lambda (_) `(,(random-int *rand-gen* 0 100)
				   ,(random-int *rand-gen* 0 100)
				   ,(float (/ (random-int *rand-gen* 0 100) 100))))
		    (cl21:iota 4)))
	  (cl21:iota num-rules)))

(defun gen-agents (num)
  (mapcar (lambda (_)
	    (make-instance 'agent))
	  (cl21:iota num)))

(defun gen-communities (size count)
  "How many communities (`count`) of agents of size `size`"
  ;; (gen-communities 3 5)
  (mapcar (lambda (_)
	    (mapcar (lambda (_)
		      (random-int *rand-gen* 0 (1- *num-agents*)))
		    (cl21:iota size)))
	  (cl21:iota count)))

(defclass agent ()
  ;; (slot-value (first (slot-value (make-instance 'agents) 'agents)) 'beliefs)
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-rules*) :accessor rules)))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules))

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

(defun agents-best (distribution)
  (cadar (sort (copy-tree distribution) #'> :key (lambda (elt) (first elt)))))

(defun agents-without-worst (distribution)
  ;; (agents-without-worst (agents-distribution *population*))
  (mapcar #'cadr (cdr (sort (copy-tree distribution) #'< :key (lambda (elt) (first elt))))))

(defun agent-trades (agent)
  "Creates a simulation of a single agent's trades."
  ;; (agent-trades (make-instance 'agent))
  (let ((sig (reduce #'+
		     (append (alexandria:flatten (slot-value agent 'beliefs))
			     (alexandria:flatten (slot-value agent 'rules))))))
    (if (cl21:gethash *cached-agents* sig)
	(cl21:gethash *cached-agents* sig)
	(let* ((closes)
	       (data (mapcar (lambda (heat)
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
				   (push close closes)
				   (list (cl21:nth z (if (< (- index 2) 0) 0 (- index 2)))
					 (cl21:nth z (if (< (- index 1) 0) 1 (- index 1)))
					 (cl21:nth z index)
					 close
					 ))))
			     (get-data :EUR_USD *rates* :levels (slot-value agent 'beliefs)))))
	  (let ((sim (ifis-agents data (slot-value agent 'rules))))
	    (setf (cl21:gethash *cached-agents* sig) sim)
	    sim)
	  ))))

(defun agents-fitness (agents)
  (let ((sim (mapcar (lambda (sim real)
		       ;; summing the current price + the sum of all trades to obtain next price
		       ;; (mse) is in charge of comparing this price against the next one
		       (+ sim real))
		     ;; summation of trades of every agent in `agents`
		     (apply #'mapcar (lambda (&rest trades)
				       (apply #'+ trades))
			    (mapcar (lambda (agent)
				      (agent-trades agent))
				    agents))
		     (get-real-data))))
    ;; (mse sim (get-real-data))
    (dirnum sim (get-real-data))
    ))

(defun ifis-agents (inputs rules)
  (let ((agent-ifss (mapcar (lambda (params)
			      (mapcar (lambda (par)
					(agents-ifs par))
				      params))
			    rules)))
    (mapcar (lambda (inp)
	      (let ((mx-inp (max (nth 0 inp) (nth 1 inp) (nth 2 inp))))
		(process-agent-output
		 (if-coa
		  (reduce #'ifunion ;; or
			  (mapcar (lambda (ifs)
				    (reduce #'ifintersection ;; and
					    (list (rule (* 100 (/ (nth 0 inp) (+ 1 mx-inp)))
							(nth 0 ifs)
							(alexandria:last-elt ifs))
						  (rule (* 100 (/ (nth 1 inp) (+ 1 mx-inp)))
							(nth 1 ifs)
							(alexandria:last-elt ifs))
						  (rule (* 100 (/ (nth 2 inp) (+ 1 mx-inp)))
							(nth 2 ifs)
							(alexandria:last-elt ifs)))))
				  agent-ifss))))))
	    inputs)))

(defun agents-distribution (population)
  ;; (agents-distribution *population*)
  (let* ((fitnesses (pmapcar #'agents-fitness (mapcar (lambda (indexes)
							(mapcar (lambda (index)
								  (nth index *agents-pool*))
								indexes))
						      population)))
	 (sum (apply #'+ fitnesses)))
    (values
     (mapcar (lambda (fitness x)
	       (cons (/ fitness sum) (list x))
	       )
	     fitnesses
	     population)
     (first (sort (copy-seq fitnesses) #'>)))))

(defun agents-reproduce ()
  (agents-mutate)

  (dotimes (i (floor (/ (length *population*) 2)))
    (let ((x (agents-selectone (agents-distribution *population*)))
	  (y (agents-selectone (agents-distribution *population*)))
	  )
      (multiple-value-bind (newx newy)
	  (agents-crossover x y)
	(setf *population* (append (list x) (agents-without-worst (agents-distribution *population*))))
	(setf *population* (append (list y) (agents-without-worst (agents-distribution *population*)))))))

  (multiple-value-bind (_ f) (agents-distribution *population*)
    f))

(defun avg (lst)
  (/ (reduce #'+ lst) (length lst)))

(defun agent-uncertainty (agents)
  "Used for only one agent."
  (avg (mapcar (lambda (rule)
	      ;; (avg
	      ;;  (mapcar (lambda (r)
	      ;; 	      (second r))
	      ;; 	    rule))
	      (second (last rule))
	      )
	    (first (slot-value agents 'rules)))))

(defun write-to-file (filename content)
  (with-open-file (str filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str content)))

;; ;; understanding logic start
;; (defparameter *rates* (get-rates :EUR_USD 1 :H1))
;; (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents "/home/amherag/quicklisp/local-projects/neuropredictions/data/aud_usd.dat"))) 300 800))
;; (progn
;;   (setf lparallel:*kernel* (lparallel:make-kernel 4))
;;   (defparameter *num-agents* 2000)
;;   (defparameter *num-rules* 2)
;;   (defparameter *community-size* 20)
;;   (defparameter *population-size* 20)
;;   (defparameter *agents-pool* (gen-agents *num-agents*))
;;   (defparameter *population* (gen-communities *community-size* *population-size*))
;;   (defparameter *cached-agents* (make-hash-table :test #'equal))
;;   (defparameter *fitnesses* nil)
;;   (defparameter *ifs-sd* 20))
;; (time
;;  (dotimes (x 10000)
;;    (let ((fitness (agents-reproduce)))
;;      (format t "~a: ~a~%" x fitness)
;;      (push fitness *fitnesses*))))

;; (purge-non-believers *population*)
;; (dolist (pop *population*)
;;   (dolist (beliefs (slot-value pop 'beliefs))
;;     (print beliefs)))

;; ;; (dolist (real (get-real-data))
;; ;;   (print real))

;; ;; make a test-all method to test a model against multiple markets, multiple testing sets
;; ;; the objective is to prove that our architecture is generalized enough to work anywhere
;; (let ((*rates* (subseq (ms:unmarshal (read-from-string (file-get-contents "/home/amherag/quicklisp/local-projects/neuropredictions/data/aud_usd.dat"))) 500 1000)))
;;   (dirnum (agents-simulation (agents-best (agents-distribution *population*)))
;; 	  (get-real-data)))

;; (defparameter *best-agent* (agents-best (agents-distribution *population*)))



(defun agents-ifs (params)
  (ifs (gaussian-mf (nth 0 params) *ifs-sd* 0 (- 1 (nth 2 params)))
       (gaussian-nmf (nth 1 params) *ifs-sd* 0 (nth 2 params))))

(defun purge-non-believers (population)
  "Searches in `pop` if an agent is not believing in anything, i.e. a vector of
`NIL`s. Replaces this sequence with a random belief vector."
  (dotimes (i (length population))
    (dotimes (j (length (slot-value (nth i population) 'beliefs)))
      (if (every #'null (nth j (slot-value (nth i population) 'beliefs)))
	  (setf (nth j (slot-value (nth i population) 'beliefs))
	  	(first (gen-beliefs 1)))))))

(defun process-agent-output (out)
  (/ (- (* out 2) 100) 10000))

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
      (let* ((site (random-int *rand-gen* 0 (1- *community-size*)))
	     (x1 (subseq x 0 site))
	     (x2 (nthcdr site x))
	     (y1 (subseq y 0 site))
	     (y2 (nthcdr site y)))
	(setf x (append x1 y2))
	(setf y (append y1 x2))
	(values
	 (append x1 y2)
	 (append y1 x2))
	)
      ;; no change
      (values x y)))

(defun agents-simulation (agents-indexes)
  "Returns a simulation of multiple agents trading a dataset."
  ;; (agent-simulation (make-instance 'agent))
  (mapcar #'+
	  (apply #'mapcar (lambda (&rest trades)
			    (apply #'+ trades))
		 (mapcar #'agent-simulation
			 (mapcar (lambda (index)
				   (nth index *agents-pool*))
				 agents-indexes)))
	  (get-real-data)))

(defun agents-mutate (&optional (chance 0.9))
  "Mutates the pool of agents if a according to `chance`."
  ;; (agents-mutate 0.5)
  (if (> chance (random-float *rand-gen* 0 1.0))
      (setf (nth (random-int *rand-gen* 0 (1- *community-size*))
		 (nth (random-int *rand-gen* 0 (1- *population-size*)) *population*))
	    (random-int *rand-gen* 0 (1- *num-agents*)))))

(defun agents-describe (agents)
  ;; (agents-describe (agents-best (agents-distribution *population*)))
  (let* ((all-levels (mapcar (lambda (belief) (remove nil belief)) (slot-value agents 'beliefs)))
	 (closes)
	 (data (mapcar (lambda (levels)
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
				       (push close closes)
				       (list (cl21:nth z (if (< (- index 2) 0) 0 (- index 2)))
					     (cl21:nth z (if (< (- index 1) 0) 1 (- index 1)))
					     (cl21:nth z index)
					     close
					     )))
				   )
				 (get-data :EUR_USD *rates* :levels levels)
				 )
			 )
		       all-levels))
	 (agents-profits (mapcar (lambda (agent-data agent-rules)
				   (let ((sim (cl-json:decode-json-from-string
					       (dex:post "http://localhost:5000/ifis-agents"
							 ;; :headers '(("Content-Type" . "application/json"))
							 :content `(("inputs". ,(cl-json:encode-json-to-string
										 (list agent-data)))
								    ("rules" . ,(cl-json:encode-json-to-string (list agent-rules))))
							 ))))
				     (reduce #'+ (dir sim (get-real-data)))
				     ))
				 data
				 (slot-value agents 'rules)
				 ))
	 (agents-perception (let ((simp-fibos (mapcar (lambda (agent-data)
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
						      data)))
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
					(let ((key (list `(:action ,(first (last simp-rule)))
							 `(:fibo-type ,fibo-type)
							 `(:fibo-strength ,(first clause))))
					      (num-agents )
					      (pi-fibo (second clause))
					      (pi-action (second (last simp-rule)))
					      (profit (cl21:nth agents-profits agent-number))
					      (perception (cl21:nth (cl21:nth (cl21:nth agents-perception agent-number)
									      fibo-type)
								    (first clause))))

					  (when (cl21:gethash interprets key) ;; already exists, I need to average
					    (setq pi-fibo (/ (+ (access :pi-fibo (cl21:gethash interprets key))
								pi-fibo)
							     2)
						  pi-action (/ (+ (access :pi-action (cl21:gethash interprets key))
								  pi-action)
							       2)
						  profit (/ (+ (access :profit (cl21:gethash interprets key))
							       profit)
							    2)
						  perception (/ (+ (access :perception (cl21:gethash interprets key))
								   perception)
								2)))
				 
					  (if (cl21:gethash dups key)
					      (setf (cl21:gethash interprets key)
						    (list `(:num-agents ,(access :num-agents (cl21:gethash interprets key)))
							  `(:pi-fibo ,pi-fibo)
							  `(:pi-action ,pi-action)
							  `(:perception ,(float perception))
							  `(:profit ,profit)))
					      (progn
						(setf (cl21:gethash dups key) t)
						(setf (cl21:gethash interprets key)
						      (list `(:num-agents ,(if (cl21:gethash interprets key)
									       (1+ (access :num-agents (cl21:gethash interprets key)))
									       1))
							    `(:pi-fibo ,pi-fibo)
							    `(:pi-action ,pi-action)
							    `(:perception ,(float perception))
							    `(:profit ,profit)))
						)
					      )
					  )
					)
				      (cl21:iota 3)
				      (reverse (rest (reverse simp-rule))))
			      ))
			  agent-rules)))
	      (cl21:iota (length (slot-value agents 'rules)))
	      (slot-value agents 'rules))

      (format nil "~{~a~^~%~}"
	      (mapcar (lambda (elt)
			(let* ((key (car elt))
			       (body (cdr elt))
			       (num-agents (access :num-agents body))
			       (profit (round (* (access :profit body) 10000)))
			       (perception (round (* (access :perception body) 100)))
			       (fibo-strength (access :fibo-strength key))
			       (fibo-type (access :fibo-type key))
			       (pi-fibo (round-to (access :pi-fibo body) 3))
			       (action (access :action key))
			       (pi-action (round-to (access :pi-action body) 3))
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
			      (if (= (access :num-agents elt1)
				     (access :num-agents elt2))
				  (> (access :profit elt1)
				     (access :profit elt2))
				  (> (access :num-agents elt1)
				     (access :num-agents elt2))))
			    )))
      
      )))

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

(defun get-real-data ()
  (reverse (subseq (reverse (mapcar (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 100)))

(defun get-accumulation (start vals)
  (let ((results `(,start)))
    (mapcar (lambda (val)
	   (push (+ (first results) val) results))
	 vals)
    (rest (reverse results))))

(defun mse (series1 series2)
  (/ (reduce #'+ (mapcar (lambda (elt) (expt elt 2)) (mapcar #'- series1 (rest series2))))
     (length series1)))

(defun dir (series1 series2)
  "Currently it returns the agent profit at each trade."
  (let ((sim series1)
	(real (rest series2))
	;; we only need the real previous, as the simulated is based on the last real price at every moment
	;; (prev (- (second series2) (first series2)))
	(prev (first series2))
	)
    (mapcar (lambda (s r)
	   (let ((real-dir (- r prev))
		 (sim-dir (- s prev)))
	     (setq prev r)
	     (if (equal (plusp real-dir)
			(plusp sim-dir))
		 (abs sim-dir)
		 (* -1 (abs sim-dir))))
	   )
	 sim
	 real)
    ))

(defun dirnum (series1 series2)
  "How many trades were correct."
  (let ((sim series1)
	(real (rest series2))
	;; we only need the real previous, as the simulated is based on the last real price at every moment
	;; (prev (- (second series2) (first series2)))
	(prev (first series2))
	)
    (apply #'+ (mapcar (lambda (s r)
			 (let ((real-dir (- r prev))
			       (sim-dir (- s prev)))
			   (setq prev r)
			   (if (equal (plusp real-dir)
				      (plusp sim-dir))
			       1
			       0))
			 )
		       sim
		       real))))

(defparameter *num-agents* 2000)
(defparameter *num-rules* 2)
(defparameter *community-size* 20)
(defparameter *population-size* 20)
(defparameter *agents-pool* (gen-agents *num-agents*))
(defparameter *population* (gen-communities *community-size* *population-size*))
(defparameter *cached-agents* (make-hash-table :test #'equal))
(defparameter *fitnesses* nil)
(defparameter *ifs-sd* 20)
(defparameter *continue?* t)
(defparameter *num-agents* 10)
(defparameter *num-rules* 2)
(defparameter *cached-agents* (make-hash-table :test #'equal))
(defparameter *ifs-sd* 30)
(defparameter *random-best* nil)
