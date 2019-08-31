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

(defparameter *continue?* t)
(defparameter *ifs-sd* 30)
(defparameter *num-agents* 10)
(defparameter *num-rules* 2)
(defparameter *cached-agents* (make-hash-table :test #'equal))
(defparameter *random-best* nil)

(defun combinations (&rest lists)
  ;; (combinations '(4 10 20) '(4 10 20) '(4 10 20)
  ;; 	      '("aud_usd" "eur_gbp" ;; "eur_jpy" "usd_jpy"
  ;; 		"eur_usd" "gbp_usd" "usd_cad"))
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
		(mapcar (lambda (outer-val)
			  (cons outer-val
				inner-val))
			(car lists)))
	      (apply #'combinations (cdr lists)))))

;; (defun access (item alist)
;;   (second (assoc item alist)))

;; (access :profit '((:meow 5) (:profit (1 31 12))))

(defun shuffle (sequence)
  ;; (shuffle '(1 2 3 4 5 6))
  (loop for i from (length sequence) downto 2
     do (rotatef (elt sequence (random-int *rand-gen* 0 (1- i)))
                 (elt sequence (1- i))))
  sequence)

(defun round-to (number precision &optional (what #'round))
  ;; (round-to 3.3612 1)
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))

(defun gen-beliefs (n)
  ;; (gen-beliefs 1000)
  (let ((options '(0.236 0.382 0.5 0.618 1 1.618)))
    (mapcar (lambda (_)
	      (let ((nums (mapcar (lambda (fibo)
				    ;; (if (= (random-int *rand-gen* 0 1) 0)
				    ;;     (+ fibo (/ (random-int *rand-gen* 0 5) 100))
				    ;;     (- fibo (/ (random-int *rand-gen* 0 5) 100)))
			      
				    ;; (let ((r (random-int *rand-gen* 0 1)))
				    ;; 	(cond ((= r 0) (round-to (+ fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
				    ;; 	      ((= r 1) (round-to (- fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
				    ;; 	      ))
				    (round-to (float (/ (random-int *rand-gen* 0 200) 100)) 3)
				    )
				  (cl21:take (1+ (random-int *rand-gen* 1 (1- (length options))))
					(shuffle options)))))
		(shuffle (concatenate 'list (make-list (- (length options) (length nums))) nums))))
	    (cl21:iota n))))

(defun gen-rules (num-agents num-rules)
  ;; (gen-rules 2 2)
  (mapcar (lambda (_)
	    (mapcar (lambda (_)
		      (mapcar (lambda (_) `(,(random-int *rand-gen* 0 100)
					     ,(random-int *rand-gen* 0 100)
					     ,(float (/ (random-int *rand-gen* 0 100) 100))))
			      (cl21:iota 4)))
		    (cl21:iota num-rules)))
	  (cl21:iota num-agents)))

(defclass agents ()
  ;; (slot-value (make-instance 'agents) 'rules)
  ((beliefs :initarg :beliefs :initform (gen-beliefs *num-agents*) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-agents* *num-rules*) :accessor rules)))

(defmethod ms:class-persistent-slots ((self agents))
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
  (cdar (sort (copy-tree distribution) #'< :key (lambda (elt) (first elt)))))

(defun agents-without-worst (distribution)
  (mapcar #'cdr (cdr (sort (copy-tree distribution) #'> :key (lambda (elt) (first elt))))))

(defun agents-fitness (agents &optional (sim-and-dir? nil))
  ;; (agents-fitness (cdr (nth 0 (agents-distribution *population*))))
  (let ((sig (reduce #'+
		     (append (alexandria:flatten (slot-value agents 'beliefs))
			     (alexandria:flatten (slot-value agents 'rules))))))
    (if (and (not sim-and-dir?) (cl21:gethash *cached-agents* sig))
	(cl21:gethash *cached-agents* sig)
	(handler-case (let* ((all-levels (mapcar (lambda (belief) (remove nil belief)) (slot-value agents 'beliefs)))
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
						     ;; FIXME: accept a variable instrument
						     (get-data :EUR_USD *rates* :levels levels))
					     )
					   all-levels)))
			(let ((sim (ifis-agents data (slot-value agents 'rules)))
			      ;; (sim (apply #'mapcar (lambda (&rest sim-point)
			      ;; 			     (apply #'+ sim-point))
			      ;; 		  (mapcar (lambda (agent-rules)
			      ;; 			    (ifis-agents data agent-rules))
			      ;; 			  (slot-value agents 'rules))))
			      )
			  (let ((err (mse sim (get-real-data)))
				;; (err (apply #'+ (dir sim (get-real-data))))
				;; (err (apply #'+ (dirnum sim (get-real-data))))
				)
			    (if (not sim-and-dir?)
				(setf (cl21:gethash *cached-agents* sig) err))
			    (if sim-and-dir?
				(values (if (< err 0)
					    0
					    err)
					;; simulated rates
					(let ((tmp (append (list (first sim)) sim)))
					  (reverse (rest (reverse tmp))))
					;; profit at each trade
					(append (list 0) (dir sim (get-real-data))))
				err))
			  )
			)
	  (error (c)
	    ;; (format t "Condition: ~a.~%Error in agents: ~a ~%~% ~a.~&" c (slot-value agents 'beliefs) (slot-value agents 'rules))
	    (format t "Condition: ~a~%" c)
	    100)
	  )
	)))

(defun agents-distribution (population)
  ;; (agents-distribution *population*)
  (let* ((fitnesses (pmapcar #'agents-fitness population))
	 (sum (apply #'+ fitnesses)))
    (values
     (mapcar (lambda (fitness x)
	       (cons (/ fitness sum) x)
	       )
	     fitnesses
	     population)
     (first (sort (copy-seq fitnesses) #'<)))))

(defun agents-reproduce (population)
  (let* ((offspring nil)
	 (d (agents-distribution population))
	 (fitness (multiple-value-bind (_ f) (agents-distribution population)
		    f))
	 (best (copy-tree (agents-best d)))
	 (best-beliefs (copy-tree (slot-value best 'beliefs)))
	 (best-rules (copy-tree (slot-value best 'rules)))
	 (best-removed? nil))
    (dotimes (i (/ (length population) 2))
      (let* ((x (agents-selectone d))
	     (y (agents-selectone d))
	     (crossover? (agents-crossover x y)))
	(when (and (equalp x best) crossover?) (setf best-removed? t))
	(when (and (equalp y best) crossover?) (setf best-removed? t))
	))
    (values
     (append (list (make-instance 'agents
				  :beliefs best-beliefs
				  :rules best-rules))
	     (agents-without-worst (agents-distribution population)))
     fitness)
    ))

(defun random-search (generations)
  ;; (time (random-search 10))
  (let ((best-fitness 100)
	(best-agents nil))
    (cl21:doeach (x (cl21:iota generations))
      (when *continue?*
	(let* ((agents (make-instance 'agents))
	       (fitness (agents-fitness agents nil)))
	  (if (< fitness best-fitness)
	      (progn
		(setq best-fitness fitness)
		(setq best-agents agents)
		(setf *random-best* agents)
		(print best-fitness))
	      (print best-fitness)))))
    best-agents))

(defun agents-extract (agents n)
  (make-instance 'agents
		 :beliefs (list (cl21:nth (slot-value agents 'beliefs) n))
		 :rules (list (cl21:nth (slot-value agents 'rules) n))))

(defun avg (lst)
  (/ (reduce #'+ lst) (length lst)))

(defun agent-uncertainty (agents)
  "Used for only one agent."
  (avg (mapcar (lambda (rule)
	      (second (last rule)))
	    (first (slot-value agents 'rules)))))

(defun write-to-file (filename content)
  (with-open-file (str filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str content)))

;; ;; understanding logic start
;; (defparameter *rates* (get-rates :EUR_USD 1 :H1))
;; (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents "/home/amherag/quicklisp/local-projects/neuropredictions/data/aud_usd.dat"))) 0 400))
;; (setf lparallel:*kernel* (lparallel:make-kernel 1))
;; (defparameter *num-agents* 50)
;; (defparameter *num-rules* 2)
;; (defparameter *population* (mapcar (lambda (_) (make-instance 'agents)) (cl21:iota 10)))
;; (defparameter *cached-agents* (make-hash-table :test #'equal))
;; (defparameter *fitnesses* nil)
;; (defparameter *ifs-sd* 20)
;; (time
;;  (cl21:doeach (x (cl21:iota 100))
;;    (multiple-value-bind (agents fitness)
;;        (agents-reproduce *population*)
;;      (setf *population* agents)
;;      (format t "~a~%" fitness)
;;      (push fitness *fitnesses*))))

;; (purge-non-believers *population*)
;; (dolist (pop *population*)
;;   (dolist (beliefs (slot-value pop 'beliefs))
;;     (print beliefs)))

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

(defun ifis-agents (inputs rules)
  (let ((agents-ifss (mapcar (lambda (rule)
			       (mapcar (lambda (params)
					 (mapcar (lambda (par)
						   (agents-ifs par))
						 params))
				       rule))
			     rules)))
    (apply #'mapcar (lambda (&rest sims)
		      (+ (first (first (first sims)))
			 (apply #'+ (mapcar #'second sims))))
	   (mapcar (lambda (agent-ifss inps)
		     (mapcar (lambda (inp)
			       (let ((mx-inp (max (nth 0 inp) (nth 1 inp) (nth 2 inp))))
				 (list (last inp)
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
							 agent-ifss)))))))
			     inps))
		   agents-ifss
		   inputs))))

(defun agents-do-all ()
  (cl21:doeach (config (combinations '(4 10 20) '(4 10 20) '(4 10 20)
				     '("aud_usd" "eur_gbp" ;; "eur_jpy" "usd_jpy"
				       "eur_usd" "gbp_usd" "usd_cad")))

    (defparameter *file-name* (format nil "results/~a_h1-~aagents-~arules-~aind-100gen"
				      (nth 3 config) (nth 0 config) (nth 1 config) (nth 2 config)))
  
    (when (not (probe-file
		(format nil "~a_population.dat"
			*file-name*
			(nth 3 config) (nth 0 config) (nth 1 config) (nth 2 config))))
    
      (format t "Running ~s~%" *file-name*)

      (defparameter *cached-agents* (make-hash-table :test #'equal))
      (defparameter *num-agents* (nth 0 config))
      (defparameter *num-rules* (nth 1 config))
      (defparameter *population* (mapcar (lambda (_) (make-instance 'agents)) (cl21:iota (nth 2 config))))
  
      (defparameter *fitnesses* nil)
  
      (defparameter *error* nil)
      (defparameter *sim* nil)
      (defparameter *profits* nil)

      ;; setting training data-set
      (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents (concatenate 'string "data/" (nth 3 config) ".dat")))) 0 500))

      (cl21:doeach (x (cl21:iota 100))
	(multiple-value-bind (agents fitness)
	    (agents-reproduce *population*)
	  (setf *population* agents)
	  (push fitness *fitnesses*)))

      (defparameter *best-agent* (agents-best (agents-distribution *population*)))

      ;; extracting report data for training stage
      (multiple-value-bind (err sim profits)
	  (agents-fitness (agents-best (agents-distribution *population*)) t)
	(setf *error* err)
	(setf *sim* sim)
	(setf *profits* profits))

      ;; writing population
      (write-to-file (format nil "~a_population.dat" *file-name*)
		     (format nil "~s" (ms:marshal *population*)))

      ;; writing report for training stage
      (write-to-file (format nil "~a_training_report.csv" *file-name*)
		     (concatenate 'string (format nil "real,sim,profits,acc-profits,error~%")
				  (format nil "~{~a~%~}" (mapcar (lambda (&rest rest)
								   (format nil "~{~a~^,~}"rest))
								 (get-real-data)
								 *sim*
								 *profits*
								 (get-accumulation 0 *profits*)
								 (reverse *fitnesses*))
					  )))
      ;; writing training error
      (write-to-file (format nil "~a_training_error.txt" *file-name*)
		     (format nil "~s" *error*))

      ;; writing training interpretation
      (write-to-file (format nil "~a_training_interpretation.txt" *file-name*)
		     (agents-describe *best-agent*))

      ;; =========================
      ;; setting testing dataset
      (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents (concatenate 'string "data/" (nth 3 config) ".dat")))) 100 600))

      ;; extracting report data for testing stage
      (multiple-value-bind (err sim profits)
	  (agents-fitness (agents-best (agents-distribution *population*)) t)
	(setf *error* err)
	(setf *sim* sim)
	(setf *profits* profits))
    
      ;; writing report for testing stage
      (write-to-file (format nil "~a_testing_report.csv" *file-name*)
		     (concatenate 'string (format nil "real,sim,profits,acc-profits~%")
				  (format nil "~{~a~%~}" (mapcar (lambda (&rest rest)
								   (format nil "~{~a~^,~}"rest))
								 (get-real-data)
								 *sim*
								 *profits*
								 (get-accumulation 0 *profits*))
					  )))
      ;; writing testing error
      (write-to-file (format nil "~a_testing_error.txt" *file-name*)
		     (format nil "~s" *error*))

      ;; writing testing interpretation
      (write-to-file (format nil "~a_testing_interpretation.txt" *file-name*)
		     (agents-describe *best-agent*))
      )))

;; (agents-do-all)

(defun agents-selectone (distribution)
  "Done"
  (let ((random (random-float *rand-gen* 0 1.0)) 
	(prob 0) 
	genotype)
    (some (lambda (pair) 
	    (incf prob (car pair))
	    (if (> random prob)
		nil
		(setf genotype (cdr pair)))) 
	  distribution)
    (agents-mutate genotype)))

(defun agents-crossover (x y &optional (chance 0.6))
  "Done."
  (let ((n-agents (length (slot-value x 'rules)))
	(n-rules (length (first (slot-value x 'rules))))
	(n-beliefs (length (first (slot-value x 'beliefs))))
	(x-beliefs (slot-value x 'beliefs))
	(x-rules (slot-value x 'rules))
	(y-beliefs (slot-value y 'beliefs))
	(y-rules (slot-value y 'rules)))
    (if (<= (random-float *rand-gen* 0 1.0) chance)
	(let* ((site (random-int *rand-gen* 0 (length x-beliefs))))
	  (setf (slot-value x 'beliefs)
		(append (subseq x-beliefs 0 site) (cl21:nthcdr y-beliefs site)))
	  (setf (slot-value y 'beliefs)
		(append (cl21:nthcdr y-beliefs site) (subseq x-beliefs 0 site)))

	  (setf (slot-value x 'rules)
		(append (subseq x-rules 0 site) (cl21:nthcdr y-rules site)))
	  (setf (slot-value y 'rules)
		(append (cl21:nthcdr y-rules site) (subseq x-rules 0 site)))
	  ))))

(defun agents-mutate (agents &optional (chance 0.2))
  "Done."
  (let (changed?)
    (setf (slot-value agents 'beliefs)
	  (mapcar (lambda (agent-beliefs)
		    (if (> (random-float *rand-gen* 0 1.0) chance)
			agent-beliefs
			(progn
			  (setq changed? t)
			  (first (gen-beliefs 1))
			  ))
		    ) 
		  (slot-value agents 'beliefs)))
    (setf (slot-value agents 'rules)
	  (mapcar (lambda (agent-rules)
		    (mapcar (lambda (rule)
			      (mapcar (lambda (params)
					(if (> (random-float *rand-gen* 0 1.0) chance)
					    params
					    (progn
					      (setq changed? t)
					      (caaar (gen-rules 1 1))
					      )))
				      rule)
			      )
			    agent-rules))
		  (slot-value agents 'rules)))
    (values agents changed?)))

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
    (mapcar (lambda (s r)
	      (let ((real-dir (- r prev))
		    (sim-dir (- s prev)))
		(setq prev r)
		(if (equal (plusp real-dir)
			   (plusp sim-dir))
		    1
		    0))
	      )
	    sim
	    real)
    ))
