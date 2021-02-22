(defpackage overmind-agents.config
  (:use :cl
	:random-state)
  (:import-from :envy
                :config-env-var
                :defconfig)
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
	   )
  (:nicknames #:omage.config))
(in-package :overmind-agents.config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algorithm Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *is-production* t)
(defparameter *is-log* t)

;; Lookahead.
(defparameter *lookahead* 10)
(defparameter *random-lookahead-p* nil)
(defparameter *random-lookahead-min* 10)
(defparameter *random-lookahead-max* 30)

(defparameter *max-tp* 100 "In PIPs")
(defparameter *min-sl* 0 "In PIPs")
(defparameter *n-times-sl-for-max-sl* 2
  "If activation for an agent is 1, then we use the SL we read from the creation dataset, but determining
what SL to use if activation = 0 is not straightforward. We use this parameter to obtain the max SL as a factor of SL (we multiply *n-times-sl-for-max-sl* by the SL read from the creation dataset).")
(defparameter *min-n-times-spread-sl* 2
  "When creating a signal, the agent can potentially output 0 < SL < spread. This parameter is used to determine a minimum SL that is greater than the current spread.")
(defparameter *seconds-to-optimize-per-pattern* 100)
(defparameter *max-creation-dataset-size* 3000)
(defparameter *max-training-dataset-size* 3000)
(defparameter *max-testing-dataset-size* 200)
(defparameter *min-num-trades-training* 7)
(defparameter *number-of-agent-rules* 2)
(defparameter *number-of-agent-inputs* 5)
(defparameter *initial-agent-count* 100)
(defparameter *optimize-p* t)
(defparameter *min-agent-avg-return* 0.0)
(defparameter *stagnation-threshold* 0.5)
(defparameter *agents-min-rr-creation* 3
  "For example, if you want a R/R of 1:2, then 2 is the value you're looking for.")
(defparameter *agents-min-rr-trading* 2
  "For example, if you want a R/R of 1:2, then 2 is the value you're looking for.")
(defparameter *consensus-threshold* 1
  "Number of agents that must agree on direction in order to accept a trade.")
(defparameter *evaluate-agents-activation-threshold* 0.0
  "Minimum activation required for a trade to be added to the metrics generated during agent pool evaluation.")

(defparameter *trade-every-dp-p* t)

(defparameter *train-slide-step* 50)
(defparameter *train-min-chunk-size* 300)
(defparameter *train-max-chunk-size* (+ (* *train-min-chunk-size* 2) 100))
(defparameter *creation-slide-step* 50)
(defparameter *creation-min-chunk-size* 300) ;; 400
(defparameter *creation-max-chunk-size* (+ (* *creation-min-chunk-size* 2) 100)) ;; 1200

;; (defparameter *instruments* ominp:*forex*)
(defparameter *train-tf* :H1)
(defparameter *timeframes* `(,*train-tf*))
(defparameter *instruments* nil)
(if *is-production*
    (setf *instruments* ominp:*forex*)
    (setf *instruments* '(:AUD_USD :EUR_GBP :EUR_JPY :EUR_USD :GBP_USD :USD_CAD :USD_CHF :USD_CNH
 :USD_JPY)))

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
