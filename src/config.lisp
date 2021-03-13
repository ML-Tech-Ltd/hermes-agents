(defpackage overmind-agents.config
  (:use :cl
	:random-state)
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
	   :*rand-gen*
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
	   #:*min-num-trades-training*
	   #:*optimize-p*
	   #:*random-lookahead-p*
	   #:*random-lookahead-min*
	   #:*random-lookahead-max*
	   #:*train-tf*
	   #:*initial-agent-count*
	   #:*test-most-activated-agents-p*
	   #:*test-size*
	   #:*all-timeframes*
	   #:*min-pips-sl*
	   #:*max-pips-sl*
	   #:*test-same-dataset*
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

     ;; Lookahead.
     ("How many datapoints to look ahead to determine TP/SL when creating
     agents."
      *lookahead* 40)
     ("Should we use a random value for `*lookahead*`?"
      *random-lookahead-p* nil)
     ("Minimum value of a random `*lookahead*`."
      *random-lookahead-min* 10)
     ("Maximum value of a random `*lookahead*`."
      *random-lookahead-max* 30)

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
      *seconds-to-optimize-per-pattern* 10)
     ("The maximum size for the creation dataset."
      *max-creation-dataset-size* 3000)
     ("The maximum size for the training dataset."
      *max-training-dataset-size* 3000)
     ("The maximum size for the testing dataset."
      *max-testing-dataset-size* 200)
     (""
      *min-num-trades-training* 7)
     (""
      *number-of-agent-rules* 20)
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
     ("Number of agents that must agree on direction in order to
     accept a trade."
      *consensus-threshold* 1)
     ("Minimum activation required for a trade to be added to the
     metrics generated during agent pool evaluation."
      *evaluate-agents-activation-threshold* 0.0)
     (""
      *test-most-activated-agents-p* t)
     (""
      *test-same-dataset* nil)

     (""
      *trade-every-dp-p* t)

     (""
      *test-size* 100)

     (""
      *train-slide-step* 50)
     (""
      *train-min-chunk-size* 300)
     (""
      *train-max-chunk-size* (+ (* *train-min-chunk-size* 2) 100))
     (""
      *creation-slide-step* 50)
     (""
      *creation-min-chunk-size* 300) ;; 400
     (""
      *creation-max-chunk-size* (+ (* *creation-min-chunk-size* 2) 100)) ;; 1200

     (""
      *all-timeframes* '(:M1 :M5 :M15 :M30 :H1 :H4 :H12 :D))
     (""
      *train-tf* :M15)
     (""
      *timeframes* `(,*train-tf*))
     (""
      *instruments* (if *is-production*
			(setf *instruments* ominp:*forex*)
			(setf *instruments* '(:AUD_USD :EUR_GBP))))))



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
