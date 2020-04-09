(in-package :overmind-agents)

(setf omper:*data-count* (* 252 1))
(setf omper:*partition-size* 100)

(defparameter *num-inputs* 1)
(defparameter *num-rules* 2)

(defparameter *perception-fn*
  (lambda (agent)
    (declare (ignore agent))
    (apply #'mapcar (lambda (&rest ins) (flatten ins))
	   (list
	    (io-sma 10 :offset 0)
	    ;; (io-ema 10 :offset 0)
	    ;; (io-awma 10 :offset 0)
	    ;; (io-macd 5 10 :offset 0)
	    ;; (io-macd-signal 3 5 10 :offset 0)
	    ;; (io-stochastic-oscillator 10 20 :offset 0)
	    ;; (io-fibos 5 :offset 0)
		 
	    ;; (io-closes :offset 0)
	    ;; (io-highs :offset 0)
	    ;; (io-lows :offset 0)
	    ;; (io-high-heights :offset 0)
	    ;; (io-low-heights :offset 0)
	    ;; (io-candle-heights :offset 0)
	    ;; (io-volumes :offset 0)

	    ;; (io-heights :offset 0)
	    ;; (io-deltas 10 :offset 0)
		 
	    ;; Do not remove this last one.
	    (io-closes)
	    ))))

(defparameter *community-size* 1
  "Represents the number of agents in an 'individual' or solution. A simulation (a possible solution) will be generated using this number of agents.")
(defparameter *population-size* 1
  "How many 'communities', 'individuals' or 'solutions' will be participating in the optimization process.")

(defparameter *mutation-chance* 0.1)

(defparameter *num-pool-agents* *community-size*
  "How many agents will be created for `*agents-pool*`. Relatively big numbers are recommended, as this increases diversity and does not significantly impact performance.")


(defparameter *fitness-fn* :mase)

(defparameter *rules-config* `((:mf-type . :gaussian)
			       (:sd . 5)
			       (:mf-mean-adjusting . t)
			       (:nmf-height-style . :complement))
  "Configuration used to create the rules of the agents for a population.")
