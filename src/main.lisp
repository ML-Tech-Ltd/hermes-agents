;; (ql:quickload :overmind-agents)
;; (ql:quickload :mlforecasting)
;; (mlforecasting:start :port 2001)
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
  (:export :get-last-tests
	   :loop-optimize-test)
  (:nicknames :omage))
(in-package :overmind-agents)

(setf lparallel:*kernel* (lparallel:make-kernel (1- (cl-cpus:get-number-of-processors))))

(defun init-database ()
  "Creates all the necessary tables for Overmind Agents."
  (with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
    (query (:create-table 'populations
	       ((id :type string
		    :primary-key t)
		(population :type bytea)
		(instrument :type string)
		(timeframe :type string))))
    (query (:create-table 'tests
	       ((id :type string
		    :primary-key t)
		(instrument :type string)
		(timeframe :type string)
		(creation-time :type integer)
		(fitnesses :type bytea)
		(decision :type string)
		(tp :type double-float)
		(sl :type double-float)
		(entry-price :type double-float)
		)))))
;; (init-database)

;; (lparallel:pmapcar (lambda (i)
;; 		     (sleep (random-float *rand-gen* 0 2))
;; 		     (postmodern:with-connection '("amherag" "amherag" "asafrade" "localhost")
;; 		       (postmodern:query (:insert-into '|users|
;; 					   :set 'email (format nil "~a" (random-int *rand-gen* 0 10000000000))
;; 					   'password "Something"
;; 					   'membership "Something"
;; 					   'validated 0))))
;; 		   (iota 100))

(defun ->delta-close (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates))
	 (penultimate-candle (nth (- lrates (1+ offset) 1) rates)))
    (- (access last-candle :close-bid)
       (access penultimate-candle :close-bid))))

(defun ->high-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (access last-candle :high-bid)
       (if (> (access last-candle :open-bid)
	      (access last-candle :close-bid))
	   (access last-candle :open-bid)
	   (access last-candle :close-bid)))))

(defun ->low-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (if (> (access last-candle :open-bid)
	      (access last-candle :close-bid))
	   (access last-candle :close-bid)
	   (access last-candle :open-bid))
       (access last-candle :low-bid))))

(defun ->candle-height (rates &key (offset 0))
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (abs (- (access last-candle :close-bid)
	    (access last-candle :open-bid)))))

;; (->delta-close-bid *rates* :offset 1)

(defparameter *beliefs*
  `((:perception-fns . (((:fn . ->delta-close) (:offset . 2))
			((:fn . ->delta-close) (:offset . 1))
			((:fn . ->delta-close) (:offset . 0))))
    (:lookahead-count . 10)
    (:lookbehind-count . 4)))

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn in perception-fns
       collect (funcall (access fn :fn) rates :offset (access fn :offset)))))

;; (funcall (gen-perception-fn (access *beliefs* :perception-fns)) *rates*)

(defclass agent ()
  ((beliefs :initarg :beliefs :initform *beliefs* :accessor beliefs)
   (rules :initarg :rules :initform nil :accessor rules)
   (fitnesses :initarg :fitnesses :initform nil :accessor fitnesses)))

(defmethod ms:class-persistent-slots ((self agent))
  '(beliefs rules fitnesses))

;; (let* ((agents (update-agents-fitnesses (gen-agents 10 3 *rates*) *rates*))
;;        (ds-agents (ms:unmarshal (ms:marshal agents))))
;;   (list (access (evaluate-agents agents *rates*) :avg-revenue)
;; 	(access (evaluate-agents ds-agents *rates*) :avg-revenue)))

(defun get-tp-sl (rates &optional (lookahead-count 10))
  (let* ((init-rate (access (first rates) :close-bid))
	 (max-pos 0)
	 (max-neg 0))
    (loop for rate in (subseq rates 0 lookahead-count)
       do (let ((delta-high (- (access rate :high-bid) init-rate))
		(delta-low (- (access rate :low-bid) init-rate)))
	    ;; Checking for possible price stagnation. If true, ignore.
	    (unless (and (< init-rate delta-high)
			 (> init-rate delta-low))
	      (when (> delta-high max-pos)
		(setf max-pos delta-high))
	      (when (< delta-low max-neg)
		(setf max-neg delta-low)))))
    `((:tp . ,(if (>= max-pos (abs max-neg)) max-pos max-neg))
      (:sl . ,(if (>= (abs max-neg) max-pos) max-pos max-neg)))))
;; (get-tp-sl *rates*)

;; (funcall (access *perception-fn* :perception-fn) (subseq *rates* 10 20))

(defun get-same-direction-outputs-idxs (rates count &optional (lookahead-count 10) (lookbehind-count 10))
  (let ((pred (if (> (random-float *rand-gen* 0 1) 0.5) #'plusp #'minusp))
	(idxs (shuffle (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count)))
	(result))
    (loop for idx in idxs
       do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
	    (when (and (< (length result) count)
		       (funcall pred (access tp-sl :tp))
		       (/= (access tp-sl :sl) 0))
	      (push idx result))))
    result))

;; (get-same-direction-outputs-idxs *rates* 5)

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
  (let* ((perception-fn (gen-perception-fn (accesses agent :beliefs :perception-fns)))
	 (lookahead-count (accesses agent :beliefs :lookahead-count))
	 (lookbehind-count (accesses agent :beliefs :lookbehind-count))
	 (idxs (get-same-direction-outputs-idxs rates num-rules lookahead-count lookbehind-count))
	 (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
	 (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count)))
	 (inp-sd (mapcar (lambda (inp) (standard-deviation inp)) (apply #'mapcar #'list chosen-inputs)))
	 (tps (loop for output in chosen-outputs collect (access output :tp)))
	 (sls (loop for output in chosen-outputs collect (access output :sl)))
	 (tp-sd (standard-deviation tps))
	 (sl-sd (standard-deviation sls))
	 (mn-inp (- (apply #'min (flatten chosen-inputs)) (apply #'max inp-sd)))
	 (mx-inp (+ (apply #'max (flatten chosen-inputs)) (apply #'max inp-sd)))
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

(defun gen-agent (num-rules rates)
  (let ((agent (make-instance 'agent)))
    (setf (rules agent) (gen-ifis agent num-rules rates))
    agent))

(defun gen-agents (num-agents num-rules rates)
  (loop repeat num-agents collect (gen-agent num-rules rates)))

;; (gen-agent 4 *rates*)

(defun evaluate-ifis (agent inputs)
  (let ((rule-groups (-> (rules agent) :rules))
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

;; (loop repeat 10 collect (-> (access (evaluate-ifis (gen-agent 4 *rates*) '(0 0 0)) :tp) :cx))

(defun evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (access (first rates) :close-bid))
	(revenue 0)
	(max-pos 0)
	(max-neg 0)
	(exit-time)
	;; Needs to be (length rates) in case the trade never finishes.
	(finish-idx (length rates)))
    (loop for rate in (rest rates)
       for idx from 1 below (length (rest rates))
       do (let ((low (access rate :low-bid))
		(high (access rate :high-bid))
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

(defun evaluate-agents (agents rates)
  (let* ((lookbehind-count (get-max-lookbehind agents))
	 (revenues)
	 (max-poses)
	 (max-negses)
	 (idx lookbehind-count)
	 (trades-won 0)
	 (trades-lost 0)
	 (entry-prices)
	 (exit-prices)
	 (tps)
	 (sls)
	 (entry-times)
	 (exit-times))
    (loop while (< idx (length rates))
       do (let* ((input-dataset (get-input-dataset rates idx))
		 (output-dataset (get-output-dataset rates idx))
		 (agent (pick-most-relevant-agent agents input-dataset))
		 (perception-fn (gen-perception-fn (access (beliefs agent) :perception-fns)))
		 (response (evaluate-ifis agent (funcall perception-fn input-dataset)))
		 (tp (-> (access response :tp) :cx))
		 (sl (-> (access response :sl) :cx))
		 (trade (evaluate-trade tp sl output-dataset))
		 (revenue (access trade :revenue))
		 (max-pos (access trade :max-pos))
		 (max-neg (access trade :max-neg))
		 (exit-time (access trade :exit-time))
		 (finish-idx (access trade :finish-idx)))
	    (unless (= revenue 0)
	      (if (> revenue 0)
		  (incf trades-won)
		  (incf trades-lost))
	      (push tp tps)
	      (push sl sls)
	      (push (access (nth idx rates) :time) entry-times)
	      (push exit-time exit-times)
	      (push (access (nth idx rates) :close-bid) entry-prices)
	      (push (access (nth finish-idx output-dataset) :close-bid) exit-prices)
	      (push max-pos max-poses)
	      (push max-neg max-negses)
	      (push revenue revenues))
	    (incf idx finish-idx)))
    `((:avg-revenue . ,(if (> (length revenues) 0) (mean revenues) 0))
      (:stdev-revenue . ,(if (> (length revenues) 0) (standard-deviation revenues) 0))
      (:total-revenue . ,(if (> (length revenues) 0) (reduce #'+ revenues) 0))
      (:avg-max-pos . ,(if (> (length max-poses) 0) (mean max-poses) 0))
      (:stdev-max-pos . ,(if (> (length max-poses) 0) (standard-deviation max-poses) 0))
      (:avg-max-neg . ,(if (> (length max-negses) 0) (mean max-negses) 0))
      (:stdev-max-neg . ,(if (> (length max-negses) 0) (standard-deviation max-negses) 0))
      (:max-max-pos . ,(if (> (length max-poses) 0) (apply #'max max-poses) 0))
      (:max-max-neg . ,(if (> (length max-negses) 0) (apply #'max max-negses) 0))
      (:avg-tp . ,(if (> (length tps) 0) (mean tps) 0))
      (:stdev-tp . ,(if (> (length tps) 0) (standard-deviation tps) 0))
      (:max-tp . ,(if (> (length max-negses) 0) (if (> (first tps) 0) (apply #'max tps) (apply #'min tps)) 0))
      (:min-tp . ,(if (> (length max-negses) 0) (if (> (first tps) 0) (apply #'min tps) (apply #'max tps)) 0))
      (:max-sl . ,(if (> (length max-negses) 0) (if (> (first sls) 0) (apply #'max sls) (apply #'min sls)) 0))
      (:min-tp . ,(if (> (length max-negses) 0) (if (> (first sls) 0) (apply #'min sls) (apply #'max sls)) 0))
      (:avg-sl . ,(if (> (length sls) 0) (mean sls) 0))
      (:stdev-sl . ,(if (> (length sls) 0) (standard-deviation sls) 0))
      (:trades-won . ,trades-won)
      (:trades-lost . ,trades-lost)
      (:revenues . ,(reverse revenues))
      (:entry-times . ,(reverse entry-times))
      (:exit-times . ,(reverse exit-times))
      (:entry-prices . ,(reverse entry-prices))
      (:exit-prices . ,(reverse exit-prices))
      (:tps . ,(reverse tps))
      (:sls . ,(reverse sls)))))

(defun get-prediction (agents rates)
  (let* ((agent (pick-most-relevant-agent agents rates))
	 (perception-fn (gen-perception-fn (access (beliefs agent) :perception-fns)))
	 (response (evaluate-ifis agent (funcall perception-fn rates)))
	 (tp (-> (access response :tp) :cx))
	 (sl (-> (access response :sl) :cx)))
    `((:tp . ,tp)
      (:sl . ,sl))))

(defun update-agent-fitnesses (agent rates)
  (let ((fitnesses (evaluate-agents (list agent) rates)))
    (setf (fitnesses agent) fitnesses)
    agent))

(defun update-agents-fitnesses (agents rates)
  (loop for agent in agents
     do (update-agent-fitnesses agent rates))
  agents)

;; (evaluate-agents (gen-agents 1 4 *input-dataset*) *output-dataset*)

;; (agent-dominated? (update-agent-fitnesses (gen-agent 4 *rates*) *output-dataset*) *agents*)

(defun get-max-lookbehind (agents)
  (let ((mx 0))
    (loop for agent in agents
       do (let ((count (accesses agent :beliefs :lookbehind-count)))
	    (when (> count mx)
	      (setf mx count))))
    mx))

(defun agent-dominated? (agent agents)
  (let* ((fitnesses (access agent :fitnesses))
	 (avg-revenue-0 (access fitnesses :avg-revenue))
	 (trades-won-0 (access fitnesses :trades-won))
	 (trades-lost-0 (access fitnesses :trades-lost))
	 (total-revenue-0 (access fitnesses :total-revenue))
	 (stdev-revenue-0 (access fitnesses :stdev-revenue))
	 (is-dominated? nil))
    ;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
    (loop for agent in agents
       do (let* ((fitnesses (access agent :fitnesses))
		 (avg-revenue (access fitnesses :avg-revenue))
		 (trades-won (access fitnesses :trades-won))
		 (trades-lost (access fitnesses :trades-lost))
		 (total-revenue (access fitnesses :total-revenue))
		 (stdev-revenue (access fitnesses :stdev-revenue)))
	    ;; (format t "~a, ~a, ~a~%" avg-revenue trades-won trades-lost)
	    (when (and (> avg-revenue avg-revenue-0)
		       (< stdev-revenue stdev-revenue-0)
		       ;; (> trades-won trades-won-0)
		       ;; (< trades-lost trades-lost-0)
		       ;; (> total-revenue trades-won-0)
		       )
	      (setf is-dominated? t)
	      (return))))
    is-dominated?))

(defun ensure-pareto-frontier (agents)
  "Checks that `agents` does not contain any agent that is dominated by any other agent."
  (loop for agent in agents
     when (not (agent-dominated? agent agents))
     collect agent))

;; (ensure-pareto-frontier (update-agents-fitnesses (gen-agents 4 4 *input-dataset*) *output-dataset*))

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

;; (format t "~5$" 10)

(defun optimization (agents gen-agent-fn rates iterations &optional report-fn)
  "`agents` is assumed to be a Pareto frontier. `finally-fn`"
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if agents agents (list (update-agent-fitnesses (funcall gen-agent-fn) rates)))))
    (loop repeat iterations
       do (let ((agent (update-agent-fitnesses (funcall gen-agent-fn) rates)))
	    (unless (agent-dominated? agent agents)
	      ;; Checking if old agents are dominated by this new agent.
	      ;; (setf agents (list agent))
	      (setf agents (append (list agent)
	      			   (loop for a in agents
	      			      when (not (agent-dominated? a (list agent)))
	      			      collect a)))
	      ;; (when report-fn
	      ;; 	(funcall report-fn agents rates))
	      )))
    agents))

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

(defun get-agents (instrument timeframe)
  (let ((agents (with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
		  (query (:select 'population :from 'populations :where (:and (:= 'instrument (format nil "~a" instrument))
									      (:= 'timeframe (format nil "~a" timeframe))))))))
    (when agents
      (decompress-object (caar agents)))))
;; (null (get-agents :EUR_USD :H1))

(defun remove-bad-agents (&optional (max-agents-per-pool 600))
  (loop for timeframe in ominp:*timeframes*
     do (loop for instrument in ominp:*instruments*
	   do (let* ((agents (get-agents instrument timeframe))
		     (observations (loop for agent in agents
				      collect (let* ((fitnesses (access agent :fitnesses))
						     (avg-revenue (access fitnesses :avg-revenue))
						     (stdev-revenue (access fitnesses :stdev-revenue)))
						(list avg-revenue stdev-revenue)))))
		;; (when agents
		;;   (store-agents filtered-agents instrument timeframe))
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
		    (store-agents representative-agents instrument timeframe)))))))

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

;; (remove-bad-agents)
;; (length (get-agents :AUD_USD :H1))

(defun store-agents (agents instrument timeframe)
  (let ((exists? (get-agents instrument timeframe)))
    (if exists?
	;; Updating.
	(with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
	  (query (:update 'populations
		   :set
		   'population (compress-object agents)
		   :where (:and (:= 'instrument (format nil "~a" instrument))
				(:= 'timeframe (format nil "~a" timeframe))))))
	;; Creating new instance.
	(with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
	  (query (:insert-into 'populations
		   :set
		   'id (format nil "~a" (uuid:make-v4-uuid))
		   'population (compress-object agents)
		   'instrument (format nil "~a" instrument)
		   'timeframe (format nil "~a" timeframe)))))))

;; (store-agents (gen-agents 2 4 *rates*) :EUR_USD :H1)

;; (format t "~a" :EUR_USD)

(defun is-market-close ()
  (let ((day-of-week (local-time:timestamp-day-of-week (local-time:now) :timezone local-time:+utc-zone+))
	(hour (local-time:timestamp-hour (local-time:now) :timezone local-time:+utc-zone+)))
    (or
     ;; Friday
     (and (= day-of-week 5)
	  (>= hour 21)
	  )
     ;; Saturday
     (= day-of-week 6)
     ;; Sunday
     (and (= day-of-week 0)
	  (< hour 20)))))

(defun get-tests (instrument timeframe &optional (limit 10))
  (with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
    (query (:limit (:order-by (:select '*
				       :from 'tests
				       :where (:and (:= 'instrument (format nil "~a" instrument))
						    (:= 'timeframe (format nil "~a" timeframe))))
			      (:desc 'creation-time))
		   '$1)
	   limit
	   :alists)))

(defun get-last-test (instrument timeframe)
  (car (get-tests instrument timeframe 1)))
;; (access (get-last-test :EUR_USD :H1) :id)
;; (get-tests :AUD_USD :H1 10)

(defun get-last-tests (instruments timeframes &optional (limit 1))
  (let* ((instruments (loop for instrument in instruments collect (format nil "~a" instrument)))
	 (timeframes (loop for timeframe in timeframes collect (format nil "~a" timeframe)))
	 (tests (with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
		  (query (:select '* :from
				  (:as (:select '*
						(:as (:over (:row-number)
							    (:partition-by 'instrument 'timeframe
									   :order-by (:desc 'creation-time)))
						     :idx)
						:from 'tests
						:where (:and (:in 'instrument (:set instruments))
							     (:in 'timeframe (:set timeframes))))
				       'results)
				  :where (:<= 'idx '$1))
			 limit
			 :alists))))
    ;; Decompressing fitnesses.
    (loop for test in tests
       when (/= (access test :tp) 0)
       collect (progn
		 (setf (access test :fitnesses)
		       (decompress-object (access test :fitnesses)))
		 test))
    ))

;; (time (get-last-tests ominp:*forex* ominp:*timeframes* 3))

;; (accesses (first (get-last-tests '(:EUR_USD :GBP_USD) '(:H1 :D) 3)) :fitnesses :min-tp)

;; (get-last-tests '(:EUR_USD :GBP_USD) '(:H1 :D) 3)
;; (get-last-tests '(:GBP_USD) '(:H1))

(defun store-test (instrument timeframe fitnesses prediction rates)
  (with-connection (list *db-name* *db-user* *db-pass* *db-hostname*)
    (query (:insert-into 'tests
	     :set
	     'id (format nil "~a" (uuid:make-v4-uuid))
	     'instrument (format nil "~a" instrument)
	     'timeframe (format nil "~a" timeframe)
	     'creation-time (local-time:timestamp-to-unix (local-time:now))
	     'fitnesses (compress-object fitnesses)
	     'decision (if (= (access prediction :tp) 0)
			   "HOLD"
			   (if (> (access prediction :tp) 0)
			       "BUY"
			       "SELL"))
	     'tp (access prediction :tp)
	     'sl (access prediction :sl)
	     'entry-price (access (last-elt rates) :close-bid)
	     ))))

(defun loop-optimize-test (iterations &key (creation-dataset-size 200) (training-dataset-size 200) (num-rules 3) (report-fn nil) (max-agents-per-pool 600))
  (loop (unless (is-market-close))
     (dolist (instrument ominp:*forex*)
       (dolist (timeframe ominp:*shortterm*)
	 (unless (is-market-close)
	   (let* ((rates (get-rates-count instrument timeframe
					  (+ creation-dataset-size training-dataset-size)
					  :provider :oanda :type :fx))
		  (agents (get-agents instrument timeframe))
		  (training-dataset (get-output-dataset rates training-dataset-size)))
	     (when agents
	       (let ((fitnesses (evaluate-agents agents training-dataset))
		     (prediction (get-prediction agents training-dataset)))
		 (when (and (/= (access prediction :tp) 0)
			    (< (access prediction :tp) 100)
			    (/= (access prediction :sl) 0)
			    (< (access prediction :sl) 100)
			    (/= (access fitnesses :trades-won) 0)
			    (/= (+ (access fitnesses :trades-won)
				   (access fitnesses :trades-lost))
				0))
		   (store-test instrument timeframe fitnesses prediction rates))))
	     (let* ((creation-dataset (get-input-dataset rates creation-dataset-size))
		    (trained-agents (optimization agents
						  (lambda () (gen-agent num-rules creation-dataset))
						  training-dataset
						  iterations
						  report-fn)))
	       (store-agents trained-agents instrument timeframe))))))
     (remove-bad-agents max-agents-per-pool)))

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
(defun describe-agents (agents)
  )

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

;; (get-agents :EUR_USD :H1)
;; (get-agents :AUD_USD :H1)
;; (loop-optimize-test 10)

;; (get-agents :EUR_USD :H1)

;; (evaluate-agents (gen-agents 3 3 *rates*) (get-output-dataset *rates* (- (length *rates*) 5)))

;; (get-prediction (gen-agents 3 3 *rates*) *rates*)

;; (defparameter *rates* (get-rates-count :EUR_USD :H1 600 :provider :oanda :type :fx))
;; (defparameter *creation-dataset* (get-input-dataset *rates* 200))
;; (defparameter *training-dataset* (get-input-dataset (get-output-dataset *rates* 200) 200))
;; (defparameter *testing-dataset* (get-output-dataset *rates* 200))
;; (defparameter *results* nil)
;; (defparameter *agents* nil)
;; (defparameter *agents* (optimization nil
;; 				     (lambda () (gen-agent 3 *creation-dataset*))
;; 				     *training-dataset*
;; 				     100
;; 				     (lambda (agents rates)
;; 				       (format t "Training:~%")
;; 				       (report agents rates)
;; 				       (format t "Testing:~%")
;; 				       (report agents *testing-dataset*)
;; 				       (format t "~%-------------~%"))))

;; ((first (-> (access (first *agents*) :rules) :rules)))
;; (first (-> (access (gen-agent 2 *creation-dataset*) :rules) :rules))


;; (let* ((idx 0)
;;        (training-results (reverse (loop
;; 				     for i from 0
;; 				     for result in *results*
;; 				     when (oddp i)
;; 				     collect (nth idx result))))
;;        (testing-results (reverse (loop
;; 				    for i from 0
;; 				    for result in *results*
;; 				    when (evenp i)
;; 				    collect (nth idx result)))))
;;   (format t "Correlation: ~a~%"
;; 	  (cl-mathstats:correlation training-results testing-results))
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
