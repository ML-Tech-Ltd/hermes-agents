(defpackage overmind-agents.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:import-from :omage.utils
                :defparameters)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :*plots-directory*
           :*data-directory*
           :appenv
           :developmentp
           :productionp
	   :*db-path*
	   :*is-production*
	   :*is-log*

	   ;; Algorithm Configuration ;;
	   #:*seconds-to-optimize-per-pattern*
	   #:*max-creation-dataset-size*
	   #:*max-training-dataset-size*
	   #:*max-testing-dataset-size*
	   #:*number-of-agent-rules*
	   #:*number-of-agent-inputs*
	   #:*evaluate-agents-activation-threshold*
	   #:*instruments*
	   #:*timeframes*
	   #:*max-tp*
	   #:*min-sl*
	   #:*min-n-times-spread-sl*
	   #:*lookahead*
	   #:*n-times-sl-for-max-sl*
	   #:*min-agent-avg-return*
	   #:*stagnation-threshold*
	   #:*consensus-threshold*
	   #:*train-slide-step*
	   #:*train-min-chunk-size*
	   #:*train-max-chunk-size*
	   #:*creation-slide-step*
	   #:*creation-min-chunk-size*
	   #:*creation-max-chunk-size*
	   #:*trade-every-dp-p*
	   #:*agents-min-rr*
	   #:*agents-min-rr-creation*
	   #:*agents-min-rr-trading*
	   #:*agents-min-rr-signal*
	   #:*min-num-trades-training*
	   #:*optimize-p*
	   #:*random-lookahead-p*
	   #:*random-lookahead-min*
	   #:*random-lookahead-max*
	   #:*train-tf*
	   #:*initial-agent-count*
	   #:*use-nested-signals-p*
	   #:*test-size*
	   #:*all-timeframes*
	   #:*min-pips-sl*
	   #:*max-pips-sl*
	   #:*type-groups*
	   #:*seconds-interval-testing*
	   #:*fracdiff-d*
	   #:*fracdiff-threshold*
	   #:*trades-sort-by*
	   )
  (:nicknames #:omage.config))
(in-package :overmind-agents.config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algorithm Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameters
    (("Should the service run in production mode?"
      *is-production* t)
     ("Should we keep a log of what's going on in the algorithm?"
      *is-log* t)

     ("How many datapoints to look ahead to determine TP/SL when creating
     agents."
      *lookahead* 40)
     ("Should we use a random value for `*lookahead*`?"
      *random-lookahead-p* nil)
     ("Minimum value of a random `*lookahead*`."
      *random-lookahead-min* 40)
     ("Maximum value of a random `*lookahead*`."
      *random-lookahead-max* 120)

     ("The returned signals should be sorted by *TRADES-SORT-BY*. This
     parameter is used by GET-NESTED-SIGNALS."
      *trades-sort-by* :activation)

     ("Maximum TP we can have for any pairing. The number represents PIPs."
      *max-tp* 100)
     ("Minimum SL we can have for any pairing. The number represents PIPs."
      *min-sl* 0)

     ("If activation for an agent is 1, then we use the SL we read
from the creation dataset, but determining what SL to use if
activation = 0 is not straightforward. We use this parameter to obtain
the max SL as a factor of SL (we multiply *n-times-sl-for-max-sl* by
the SL read from the creation dataset)."
      *n-times-sl-for-max-sl* 2)
     ("When creating a signal, the agent can potentially output 0 < SL
     < spread. This parameter is used to determine a minimum SL that
     is greater than the current spread."
      *min-n-times-spread-sl* 2)
     ("For how many seconds the algorithm will optimize a population
     of agents."
      *seconds-to-optimize-per-pattern* 100)
     ("The maximum size for the creation dataset."
      *max-creation-dataset-size* 3000)
     ("The maximum size for the training dataset."
      *max-training-dataset-size* 3000)
     ("The maximum size for the testing dataset."
      *max-testing-dataset-size* 200)
     ("Create signals/test every N seconds."
      *seconds-interval-testing* (* 15 60))
     (""
      *min-num-trades-training* 7)
     (""
      *number-of-agent-rules* 2)
     (""
      *number-of-agent-inputs* 5)
     (""
      *initial-agent-count* 5)
     (""
      *optimize-p* t)
     (""
      *min-agent-avg-return* 0.0)
     (""
      *stagnation-threshold* 0.5)
     (""
      *min-pips-sl* 3)
     (""
      *max-pips-sl* 20)
     ("For example, if you want a R/R of 1:2, then 2 is the value
     you're looking for."
      *agents-min-rr-creation* 3)
     ("For example, if you want a R/R of 1:2, then 2 is the value
     you're looking for."
      *agents-min-rr-trading* 2)
     ("For example, if you want a R/R of 1:2, then 2 is the value
     you're looking for."
      *agents-min-rr-signal* 1)
     ("Number of agents that must agree on direction in order to
     accept a trade."
      *consensus-threshold* 1)
     ("Minimum activation required for a trade to be added to the
     metrics generated during agent pool evaluation."
      *evaluate-agents-activation-threshold* 0.0)
     ("When `T`, we test using the agents individually, rather than as
     a pool where we look for the single most activated agent, or we
     use a consensus method."
      *use-nested-signals-p* t)

     ("During optimization, do we treat every single DP as an
     opportunity to open a trade (`T`)? or do we wait until each
     opened trade is closed (`NIL`)?"
      *trade-every-dp-p* t)

     ;; Fractional differentiation.
     ("Order of differentiation parameter in fractional differentiation."
      *fracdiff-d* 0.3)
     ("Threshold parameter in fractional differentiation."
      *fracdiff-threshold* 1e-5)

     ("How many datapoints are we going to use from the testing
     dataset to report metrics."
      *test-size* 200)

     ("Used in automatic pattern finding algorithm. How many
     datapoints are we going to use for 'sliding' to the next chunk of
     training datapoints."
      *train-slide-step* 50)
     ("Used in automatic pattern finding algorithm. The mininum size
     of a training dataset subset found by the algorithm."
      *train-min-chunk-size* 300)
     ("Used in automatic pattern finding algorithm. The maximum size
     of a training dataset subset found by the algorithm."
      *train-max-chunk-size* (+ (* *train-min-chunk-size* 2) 100))
     ("Used in automatic pattern finding algorithm. How many
     datapoints are we going to use for 'sliding' to the next chunk of
     creation datapoints."
      *creation-slide-step* 50)
     ("Used in automatic pattern finding algorithm. The mininum size
     of a creation dataset subset found by the algorithm."
      *creation-min-chunk-size* 300) ;; 400
     ("Used in automatic pattern finding algorithm. The maximum size
     of a creation dataset subset found by the algorithm."
      *creation-max-chunk-size* (+ (* *creation-min-chunk-size* 2) 100)) ;; 1200

     ("What segments we'll be considering for our pool of agents."
      *type-groups* '((:bullish) (:bearish) (:stagnated)))
     ("Used by INIT-PATTERNS. What timeframes are we going to consider
     for the database."
      *all-timeframes* '(:M1 :M5 :M15 :M30 :H1 :H4 :H12 :D))
     ("The timeframe we'll use for training."
      *train-tf* :M15)
     ("The timeframes we'll use to generate signals."
      *timeframes* `(,*train-tf*))
     ("The instruments we'll use to generate signals."
      *instruments* (if *is-production*
			(setf *instruments* ominp:*forex*)
			(setf *instruments* '(:AUD_USD :EUR_GBP :EUR_JPY :EUR_USD :GBP_USD :USD_CAD :USD_CHF :USD_CNH
 :USD_JPY))))))

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :overmind-agents))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

;; for predict module
(defparameter *plots-directory*   (merge-pathnames #P"plots/" *application-root*))
(defparameter *data-directory*   (merge-pathnames #P"data/" *application-root*))

(defconfig :common
    `(:error-log #P"~/predictus-error.log"
      :databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
