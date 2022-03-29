;; (ql:quickload :cl-project)
;; (cl-project:make-project #p"/home/amherag/.roswell/local-projects/")

;; (ql:quickload :hermes-agents)
;; (time (loop-optimize-test))
;; (clerk:calendar)
;; (progn (hsage.db:drop-database) (hsage.db:init-database) (hsage.log:clear-logs) (when hscom.all:*is-production* (hsage::clear-jobs)))
;; (progn (drop-database) (init-database))
;; (ql-dist:disable (ql-dist:find-dist "ultralisp"))
;; (ql-dist:enable (ql-dist:find-dist "ultralisp"))

;; (conn (query (:select '* :from 'trades) :alists))
;; (conn (query (:select '* :from 'trades) :alists))

;; (bind ((trades (conn (query (:select '* :from 'trades) :alists)))
;;        (returns (remove 0.0 (loop for trade in trades collect (assoccess trade :test-avg-return))))
;;        (results (when returns `((:mean . ,(alexandria:mean returns))
;;                                 (:stdev . ,(alexandria:standard-deviation returns))
;;                                 (:n . ,(length returns)))))
;;        ((:values sign stat) (hsage.stat:t-test-two-sample -0.06639316003057408 0.14203489875313058 21
;;                                                           (assoccess results :mean)
;;                                                           (assoccess results :stdev)
;;                                                           (assoccess results :n)
;;                                                           :tails :negative)))
;;   (print (list sign stat))
;;   results)

(defpackage hermes-agents
  (:use #:cl
        #:ciel
        #:local-time
        #:access
        #:lparallel
        #:postmodern
        #:defenum
        #:fare-mop
        #:hermes-input
        #:hermes-perception
        #:hermes-intuition
        #:hermes-agents.db
        #:hermes-agents.km
        #:hermes-agents.utils
        #:hermes-agents.log
        #:hscom.log)
  (:import-from #:hu.dwim.def
                #:def)
  (:import-from #:hscom.utils
                #:assoccess
                #:dbg
                #:whole-reals-to-integers)
  (:import-from #:hscom.hsage
                #:*hybrid-maximize-p*
                #:*hybrid-population-size*
                #:*hybrid-iterations*
                #:*hybrid-mutation-rate*
                #:*cores-count*
                #:*iterations*
                #:*print-hypothesis-test*
                #:*test-size-human-strategies-signals*
                #:*test-size-human-strategies-metrics*
                #:*hybrid-fitness-metric*
                #:*run-hermes-p*
                #:*unique-point-p*
                #:*unique-count*
                #:*lookahead*
                #:*lookbehind*
                #:*instruments*
                #:*timeframes*
                #:*timeframes-being-used*
                #:*creation-dataset-size*
                #:*training-dataset-size*
                #:*testing-dataset-size*)
  (:import-from #:hsper
                #:get-human-strategies)
  (:import-from #:hsage.log
                #:log-stack
                #:clear-logs)
  (:import-from #:hsage.trading
                #:agent
                #:optimization
                #:gen-agent
                #:test-agents
                #:evaluate-agent
                #:evaluate-agents
                #:test-most-activated-agents
                #:add-agent
                #:sync-agents
                #:remove-agent
                #:get-agents-count
                #:wipe-agents
                #:update-agent-fitnesses
                #:update-agents-fitnesses
                #:validate-trades
                #:get-trade-result
                #:signal-strategy
                #:optimize-human-strategy
                #:best-individual
                #:cache-agents-from-db
                #:uncache-agents-from-db
                #:retire-agents
                #:update-unique-datasets
                #:get-dataset
                #:get-unique-dataset-idxs
                #:get-strategy)
  (:import-from #:hscom.db
                #:conn)
  (:import-from #:hsage.db
                #:init-database
                #:drop-database)
  (:import-from #:hsinp.rates
                #:fracdiff
                #:sync-datasets-to-database
                #:sync-datasets-from-database
                #:get-rates-count-big
                #:get-rates-random-count-big
                #:get-unique-dataset
                #:sync-rates)
  (:export #:loop-optimize-test)
  (:nicknames #:hsage))
(in-package :hermes-agents)

(ciel:enable-punch-syntax)

;; FARE-MEMOIZATION configuration.
(setf fare-memoization::*memoized* (make-hash-table :test #'equal :synchronized t))

;; (describe fare-memoization::*memoized*)
;; (hsage.utils:refresh-memory)

(setf lparallel:*kernel* (lparallel:make-kernel
                          (if (<= *cores-count* 0)
                              ;; (or (<= *cores-count* 0)
                              ;;     (>= *cores-count* (1- (cl-cpus:get-number-of-processors))))
                              (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
                                (if (/= ideal-cores-count 0)
                                    ideal-cores-count 1))
                              *cores-count*)))

(def (function d) clear-jobs ()
  (when (> (length clerk:*jobs*) 0)
    (ignore-errors
      (clerk:stop)
      (clerk:empty-jobs-queue))))

(defmacro -loop-human-strategies (&rest body)
  `(bind ((human-strategies (get-human-strategies)))
     (dolist (human-strategy human-strategies)
       (dolist (instrument (assoccess human-strategy :instruments))
         (dolist (timeframe (assoccess human-strategy :timeframes))
           (unless (is-market-close)
             ,@body
             ;; We don't want our data feed provider to ban us.
             ;; We also want to go easy on those database reads (`agents-count`).
             (when hscom.all:*is-production*
               (sleep 1))))))))

(def (function d) -loop-optimize-human-strategies (&key (testingp nil))
  (when (and (not (is-market-close))
             hscom.hsage:*run-human-and-hybrid-p*)
    (-loop-human-strategies
     testingp
     (optimize-human-strategy instrument timeframe human-strategy
                              :maximize *hybrid-maximize-p*
                              :population-size *hybrid-population-size*
                              :max-iterations *hybrid-iterations*
                              :mutation-rate *hybrid-mutation-rate*
                              :fitness-metric *hybrid-fitness-metric*))))

(def (function d) -loop-test-human-strategies ()
  "
-LOOP-TEST-HUMAN-STRATEGIES ...
"
  (when (and (not (is-market-close))
             hscom.hsage:*run-human-and-hybrid-p*)
    ($log $trace :-> :loop-test-human-strategies)
    (-loop-human-strategies
     ;; Testing hybrid strategy.
     (bind ((human (get-strategy instrument timeframe :human (assoccess human-strategy :name)))
            (hybrid (get-strategy instrument timeframe :hybrid (assoccess human-strategy :name))))
       (when (and hybrid (not (eq (best-individual hybrid) :null)))
         ($log $info "Trying to create signal for hybrid strategy" (assoccess human-strategy :name))
         (signal-strategy instrument timeframe
                          (slot-value hybrid 'hermes-agents.trading::id)
                          (lambda (input-dataset)
                            (funcall (assoccess human-strategy :model)
                                     input-dataset
                                     (whole-reals-to-integers (best-individual hybrid))))))
       ;; Testing human strategy.
       (when human
         ($log $info "Trying to create signal for human strategy" (assoccess human-strategy :name))
         (signal-strategy instrument timeframe
                          (slot-value human 'hermes-agents.trading::id)
                          (lambda (input-dataset)
                            (funcall (assoccess human-strategy :model)
                                     input-dataset
                                     (assoccess human-strategy :args-default)))))))
    ($log $trace :<- :loop-test-human-strategies)))

(defun -update-rates ()
  (loop for instrument in *instruments*
        do (loop for timeframe in *timeframes-being-used*
                 do (sync-rates instrument timeframe))))
;; (time (-update-rates))

(defun -loop-update-rates ()
  (loop while t
        do (-update-rates)))
;; (time (-loop-update-rates))

(def (function d) create-job-unique-datasets ()
  "
CREATE-JOB-UNIQUE-DATASETS creates a thread that calls
UPDATE-UNIQUE-DATASETS in an infinite loop.
"
  ($log $trace :-> :create-job-unique-datasets)
  ;; We run it once in main thread first. The whole algorithm depends
  ;; on having unique datasets on memory first.
  ($log $info "Creating initial unique datasets")
  (update-unique-datasets)
  ($log $info "Done creating initial unique datasets.")
  (bt:make-thread ^(loop while t do (update-unique-datasets)))
  ($log $trace :<- :create-job-unique-datasets))
;; (create-job-unique-datasets)

(def (function d) create-job-update-rates ()
  "CREATE-JOB-UPDATE-RATES creates a thread that is constantly updating our rates."
  ($log $info "Updating rates before loop that keeps updating them")
  (-update-rates)
  ($log $info "Done updating rates")
  ($log $trace :-> :create-job-update-rates)
  (bt:make-thread #'-loop-update-rates)
  ($log $trace :<- :create-job-update-rates))

(def (function d) create-job-human-strategies-metrics (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  (when hscom.hsage:*run-human-and-hybrid-p*
    ($log $trace :-> :create-job-human-strategies-metrics)
    ;; Let's run it once immediately.
    (eval `(clerk:job "Creating human strategies metrics" every ,(read-from-string (format nil "~a.seconds" seconds))
                      (-loop-test-human-strategies)))
    ($log $trace :<- :create-job-human-strategies-metrics)))
;; (create-human-strategies-metrics-job 10)

(def (function d) create-job-optimize-human-strategies (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  (when hscom.hsage:*run-human-and-hybrid-p*
    ($log $trace :-> :create-job-optimize-human-strategies)
    ;; Let's run it once immediately.
    (eval `(clerk:job "Optimizing human strategies to create hybrid strategies" every ,(read-from-string (format nil "~a.seconds" seconds))
                      (-loop-optimize-human-strategies)))
    ($log $trace :<- :create-job-optimize-human-strategies)))

(def (function d) create-job-signals (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing*` seconds."
  ($log $trace :-> :create-job-signals)
  (eval `(clerk:job "Creating signals" every ,(read-from-string (format nil "~a.seconds" seconds)) (-loop-test-all)))
  ($log $trace :<- :create-job-signals))

;; (require 'sb-sprof)
;; (sb-sprof:start-profiling)
;; (sb-sprof:reset)
;; (sb-sprof:report :type :flat)
;; (sb-sprof:report)
;; (sb-sprof:stop-profiling)

(def (function d) -loop-validate ()
  (when hscom.all:*is-production*
    ($log $info "Validating trades older than 24 hours.")
    (validate-trades)))

(def (function d) -loop-test (instrument timeframe)
  ($log $trace :-> :-loop-test instrument timeframe)
  ($log $info "Creating signal for" instrument timeframe)
  ($log $info "Beginning testing process")
  (test-agents instrument timeframe)
  ($log $trace :<- :-loop-test instrument timeframe))

(def (function d) -loop-optimize (instrument timeframe)
  ($log $trace :-> :-loop-optimize)
  (optimization instrument timeframe
                (lambda () (let ((beliefs (gen-random-perceptions hscom.hsage:*number-of-agent-inputs*)))
                             (gen-agent hscom.hsage:*number-of-agent-rules*
                                        instrument
                                        timeframe
                                        (assoccess beliefs :perception-fns)
                                        *lookahead*
                                        *lookbehind*)))
                (if (eq hscom.hsage:*stop-criteria* :time)
                    hscom.hsage:*seconds-to-optimize-per-pattern*
                    hscom.hsage:*optimization-agent-evaluations*)
                hscom.hsage:*stop-criteria*)
  ($log $info "Optimization process completed.")
  (sync-agents instrument timeframe)
  ($log $trace :<- :-loop-optimize))

(def (function d) -get-rates (instrument timeframe size)
  (if hscom.all:*is-production*
      (progn
        ;; We don't want our data feed provider to ban us.
        (sleep 1)
        (get-rates-count-big instrument timeframe size))
      (get-rates-random-count-big instrument timeframe size)))

(def (function d) -loop-test-all ()
  "We run this every `hscom.hsage:*seconds-interval-testing*` seconds."
  (dolist (instrument hscom.hsage:*instruments*)
    (dolist (timeframe hscom.hsage:*timeframes*)
      (unless (is-market-close)
        (cache-agents-from-db instrument timeframe)
        (let ((agents-count (get-agents-count instrument timeframe)))
          (when (> agents-count 0)
            (-loop-test instrument timeframe)))
        ;; We don't want our data feed provider to ban us.
        ;; We also want to go easy on those database reads (`agents-count`).
        (uncache-agents-from-db instrument timeframe)
        (sleep 1)))))

(def (function d) -loop-optimize-test-validate ()
  ($log $trace :-> :-loop-optimize-test-validate)
  ;; If development mode, we want to refresh with random datasets.
  (when (not hscom.all:*is-production*)
    (update-unique-datasets))
  ;; Gather  the human strategies metrics when in development mode.
  (when (and (not hscom.all:*is-production*)
             hscom.hsage:*run-human-and-hybrid-p*)
    (-loop-optimize-human-strategies)
    (-loop-test-human-strategies))
  ;; Human strategies signals. Let's evaluate human strategies first regardless of development or production mode.
  (when hscom.hsage:*run-human-and-hybrid-p*
    (-loop-test-human-strategies))
  (when *run-hermes-p*
    (pmap nil (lambda (instrument)
                (dolist (timeframe hscom.hsage:*timeframes*)
                  (unless (is-market-close)
                    (retire-agents instrument timeframe)
                    ($log $info "Caching agents for pattern" (list instrument timeframe))
                    (cache-agents-from-db instrument timeframe t)
                    ($log $info "Retrieving datasets for agents.")
                    ($log $info "Beginning agent optimization process.")
                    ;; Optimization.
                    (-loop-optimize instrument timeframe)
                    ;; Signal creation. Development.
                    (when (not hscom.all:*is-production*)
                      (-loop-test instrument timeframe))
                    ($log $info "Done agent optimization process.")
                    (-loop-validate)
                    (uncache-agents-from-db instrument timeframe t))
                  (sync-datasets-to-database)
                  (when *print-hypothesis-test*
                    (hsage.trading::hypothesis-test))))
          hscom.hsage:*instruments*)
    (unless hscom.all:*is-production*
      (wipe-agents)))
  ($log $trace :<- :-loop-optimize-test-validate))

(def (function d) loop-optimize-test ()
  (handler-bind ((error (lambda (c)
                          (log-stack c))))
    (when (is-market-close)
      (format t "~%==========================================================~%MARKET IS CLOSED~%SWITCH TO DEVELOPMENT MODE IF YOU WANT TO RUN EXPERIMENTS.~%==========================================================~%"))
    (hsage.utils:refresh-memory)
    ;; If market's closed, just update unique datasets once.
    (if (or (is-market-close)
            (not hscom.all:*is-production*))
        (update-unique-datasets)
        (progn
          (create-job-unique-datasets)
          (create-job-update-rates)))
    ;; Signal creation. Production. We create a cron job for this to be
    ;; run every `hscom.hsage:*seconds-interval-testing*` seconds.
    (when hscom.all:*is-production*
      (when *run-hermes-p*
        (create-job-signals hscom.hsage:*seconds-interval-testing*))
      (create-job-update-rates)
      (create-job-human-strategies-metrics hscom.hsage:*seconds-interval-testing-human-strategies-metrics*)
      (create-job-optimize-human-strategies hscom.hsage:*seconds-interval-optimizing-human-strategies*)
      (clerk:start))
    (if *run-hermes-p*
        (if (< *iterations* 0)
            (loop unless (is-market-close)
                    do (-loop-optimize-test-validate))
            (loop repeat *iterations*
                  do (-loop-optimize-test-validate)))
        (loop))))
