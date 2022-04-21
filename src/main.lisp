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
        #:hscom.log)
  (:import-from #:hu.dwim.def
                #:def)
  (:import-from #:hscom.utils
                #:assoccess
                #:whole-reals-to-integers)
  (:import-from #:hscom.hsage
                #:*cores-count*
                #:*iterations*
                #:*print-hypothesis-test*
                #:*run-hermes-p*
                #:*instruments*
                #:*timeframes*)
  (:import-from #:hsper
                #:get-human-strategies)
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

(setf lparallel:*kernel* (lparallel:make-kernel
                          (if (<= *cores-count* 0)
                              ;; (or (<= *cores-count* 0)
                              ;;     (>= *cores-count* (1- (cl-cpus:get-number-of-processors))))
                              (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
                                (if (/= ideal-cores-count 0)
                                    ideal-cores-count 1))
                              *cores-count*)))

(def (function d) log-stack (c)
  (with-open-file (str (merge-pathnames #P"hsage-stack.log" #P"~/")
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "==============~%~%~a~%" (local-time:now))
    (format str "~%~a~%~%" c)
    (loop for obj in (cdr (dissect:stack))
          when (dissect:line obj)
          do (format str "~a:~a ~a~%"
                     (dissect:file obj)
                     (dissect:line obj)
                     (dissect:call obj)))))

(def (function d) get-clerk-jobs ()
  (with-open-stream (s (make-string-output-stream))
    (clerk:calendar s)
    (get-output-stream-string s)))
;; (get-clerk-jobs)

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
                              :maximize hscom.hsage:*hybrid-maximize-p*
                              :population-size hscom.hsage:*hybrid-population-size*
                              :max-iterations hscom.hsage:*hybrid-iterations*
                              :mutation-rate hscom.hsage:*hybrid-mutation-rate*
                              :fitness-metric hscom.hsage:*hybrid-fitness-metric*))))

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

(def (function d) -update-rates ()
  (loop for instrument in *instruments*
        do (loop for timeframe in hscom.hsage:*timeframes-being-used*
                 do (sync-rates instrument timeframe))))
;; (time (-update-rates))

(def (function d) -loop-update-rates ()
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
  (dex:clear-connection-pool)
  (-update-rates)
  ($log $info "Done updating rates")
  ($log $trace :-> :create-job-update-rates)
  (dex:clear-connection-pool)
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
                                        hscom.hsage:*lookahead*
                                        hscom.hsage:*lookbehind*)))
                (if (eq hscom.hsage:*stop-criteria* :time)
                    hscom.hsage:*seconds-to-optimize-per-pattern*
                    hscom.hsage:*optimization-agent-evaluations*)
                hscom.hsage:*stop-criteria*)
  ($log $info "Optimization process completed.")
  (sync-agents instrument timeframe)
  ($log $trace :<- :-loop-optimize))

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
        (create-job-unique-datasets))
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
