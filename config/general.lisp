(in-package :overmind-agents)

(org.tfeb.hax.memoize:clear-memoized-functions)

;; (defparameter *testing-ratio* 2.5)
(defparameter *testing-ratio* 0.15)
;; (defparameter *testing-ratio* 0.05)
(defparameter *required-activations* 9)
(defparameter *activation-level* 1)
(defparameter *only-best* 1)
(defparameter *delta-gap* 1)

(setf lparallel:*kernel* (lparallel:make-kernel 32))

;; Random range.
;; (defparameter *min-dataset-size* 5000)
;; (defparameter *begin* (random-int *rand-gen* 0 (floor (- *min-dataset-size* (+ omper:*data-count* (* omper:*data-count* 2 *testing-ratio*) *num-inputs* *delta-gap* omper:*partition-size*))))
;; "The starting timestamp for the data used for training or testing.")
;; (defparameter *end* (1- (+ *begin* omper:*data-count* *num-inputs* *delta-gap* omper:*partition-size*))
;;   "The ending timestamp for the data used for training or testing.")

(defparameter *provider* :oanda)
(defparameter *markets-type* :fx)

;; last N range
(defparameter *min-dataset-size*
  (ceiling (+ omper:*data-count* (* omper:*data-count* 2 *testing-ratio*) *num-inputs* *delta-gap* omper:*partition-size*)))
(defparameter *begin* (floor (- *min-dataset-size* (+ omper:*data-count* (* omper:*data-count* 2 *testing-ratio*) *num-inputs* *delta-gap* omper:*partition-size*))))
(defparameter *end* (1- (floor (- *min-dataset-size* (+ (* omper:*data-count* 2 *testing-ratio*))))))

(defparameter *all-rates* (get-rates-count *instrument* *timeframe* *min-dataset-size* :provider *provider* :type *markets-type*))
;; (defparameter *all-rates* (get-random-rates-count *instrument* *timeframe* *min-dataset-size* :provider *provider* :type *markets-type*))
(defparameter *rates* (subseq *all-rates* *begin* *end*)
  "The rates used to generate the agents' perceptions.")

(defparameter *generations* 0
  "Keeps track of how many generations have elapsed in an evolutionary process.")

(defparameter *agents-pool* (try-until-successful (gen-agents *num-pool-agents*))
  "Instances of `agent` that are available to create solutions.")
(defparameter *population* (gen-communities *community-size* *population-size*)
  "Represents a list of lists of indexes to *agents-pool*.")

(defparameter *cached-agents* (make-hash-table :test #'equal :size 2000)
    "Used for memoizing an agent's simulation.")
  (defparameter *fitnesses* nil
    "List of fitnesses obtained after evolving a population.")
  (defparameter *fitnesses-validation* nil
    "List of fitnesses in the validation stage obtained after evolving a population.")
