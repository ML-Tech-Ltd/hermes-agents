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
                #:dbg
                #:assoccess
                #:whole-reals-to-integers
                #:sans-default
                #:getenv)
  (:import-from #:hscom.config
                #:cfg<
                #:cfg>
                #:cfg>>)
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
                          (if (<= (cfg>> :hsage :cores-count) 0)
                              ;; (or (<= (cfg>> :hsage :cores-count) 0)
                              ;;     (>= (cfg>> :hsage :cores-count) (1- (cl-cpus:get-number-of-processors))))
                              (let ((ideal-cores-count (1- (cl-cpus:get-number-of-processors))))
                                (if (/= ideal-cores-count 0)
                                    ideal-cores-count 1))
                              (cfg>> :hsage :cores-count))))

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
             (when (cfg>> :hscom :is-production)
               (sleep 1))))))))

(def (function d) -loop-optimize-human-strategies (&key (testingp nil))
  (when (and (not (is-market-close))
             (cfg>> :hsage :run-human-and-hybrid))
    (-loop-human-strategies
     testingp
     (optimize-human-strategy instrument timeframe human-strategy
                              :maximize (cfg>> :hsage :hybrid-maximize)
                              :population-size (cfg>> :hsage :hybrid-population-size)
                              :max-iterations (cfg>> :hsage :hybrid-iterations)
                              :mutation-rate (cfg>> :hsage :hybrid-mutation-rate)
                              :fitness-metric (cfg>> :hsage :hybrid-fitness-metric)))))

(def (function d) -loop-test-human-strategies ()
  "
-LOOP-TEST-HUMAN-STRATEGIES ...
"
  (when (and (not (is-market-close))
             (cfg>> :hsage :run-human-and-hybrid))
    ($log $trace :-> :loop-test-human-strategies)
    (-loop-human-strategies
     ;; Testing hybrid strategy.
     (bind ((human (get-strategy instrument timeframe :human (assoccess human-strategy :name)))
            (hybrid (get-strategy instrument timeframe :hybrid (assoccess human-strategy :name))))
       (when (and hybrid (not (eq (best-individual hybrid) :null)))
         ($log $info "Trying to create signal for hybrid strategy" (assoccess human-strategy :name))
         (signal-strategy instrument timeframe
                          (slot-value hybrid 'hermes-agents.trading::id)
                          (lambda (input-dataset idx)
                            (funcall (assoccess human-strategy :model)
                                     input-dataset idx
                                     (whole-reals-to-integers (best-individual hybrid))))))
       ;; Testing human strategy.
       (when human
         ($log $info "Trying to create signal for human strategy" (assoccess human-strategy :name))
         (signal-strategy instrument timeframe
                          (slot-value human 'hermes-agents.trading::id)
                          (lambda (input-dataset idx)
                            (funcall (assoccess human-strategy :model)
                                     input-dataset idx
                                     (assoccess human-strategy :args-default)))))))
    ($log $trace :<- :loop-test-human-strategies)))

(def (function d) -update-rates ()
  (loop for instrument across (sans-default (cfg>> :hsage :instruments))
        do (loop for timeframe across (cfg>> :hsage :timeframes-being-used)
                 do (sync-rates instrument timeframe))))
;; (time (-update-rates))
;; (sync-rates :AUD_USD :M15)
;; (sync-rates "EUR_USD" "M15")

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
  (when (cfg>> :hsage :run-human-and-hybrid)
    ($log $trace :-> :create-job-human-strategies-metrics)
    ;; Let's run it once immediately.
    (eval `(clerk:job "Creating human strategies metrics" every ,(read-from-string (format nil "~a.seconds" seconds))
                      (-loop-test-human-strategies)))
    ($log $trace :<- :create-job-human-strategies-metrics)))
;; (create-human-strategies-metrics-job 10)

(def (function d) create-job-optimize-human-strategies (seconds)
  "We run this every `hscom.hsage#:*seconds-interval-testing-human-strategies-metrics*` seconds."
  (when (cfg>> :hsage :run-human-and-hybrid)
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
  (when (cfg>> :hscom :is-production)
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
                (lambda () (let ((beliefs (gen-random-perceptions (cfg>> :hsage :number-of-agent-inputs instrument timeframe))))
                             (gen-agent (cfg>> :hsage :number-of-agent-rules instrument timeframe)
                                        instrument
                                        timeframe
                                        (assoccess beliefs :perception-fns)
                                        (cfg>> :hsage :lookahead instrument timeframe)
                                        (cfg>> :hsage :lookbehind instrument timeframe))))
                (if (string= (cfg>> :hsage :stop-criteria instrument timeframe) "TIME")
                    (cfg>> :hsage :seconds-to-optimize-per-pattern instrument timeframe)
                    (cfg>> :hsage :optimization-agent-evaluations instrument timeframe))
                (cfg>> :hsage :stop-criteria instrument timeframe))
  ($log $info "Optimization process completed.")
  (sync-agents instrument timeframe)
  ($log $trace :<- :-loop-optimize))

(def (function d) -loop-test-all ()
  "We run this every `hscom.hsage:*seconds-interval-testing*` seconds."
  (loop for instrument across (sans-default (cfg>> :hsage :instruments))
        do (loop for timeframe across (sans-default (cfg>> :hsage :timeframes))
                 do (unless (is-market-close)
                      (cache-agents-from-db instrument timeframe)
                      (let ((agents-count (get-agents-count instrument timeframe)))
                        (when (> agents-count 0)
                          (-loop-test instrument timeframe)))
                      ;; We don't want our data feed provider to ban us.
                      ;; We also want to go easy on those database reads (`agents-count`).
                      (uncache-agents-from-db instrument timeframe)
                      (sleep 1)))))

;; (cfg< "DEVELOP" :EUR_USD :M15)
;; (cfg< "DEFAULT" :EUR_USD :M15)
;; (cfg> :hsage :EUR_USD :M15)

(def (function d) coco (profile)
  "Gets the latest N optimized profiles and performs a mutation to the best of
them."
  (conn (query (:select '* :from 'config@hsage :where (:like 'profile (format "~a%" profile)))))
  )

;; (cfg< )

;; (bind ((profile "DEVELOP")
;;        (instrument :AUD_USD)
;;        (instrument :AUD_USD))
;;   (conn (query (:select 'metrics_id :from 'config@hsage :where (:like 'profile (format nil "~a%" profile)))
;;                :alist)))

(def (function d) -loop-optimize-test-validate ()
  ($log $trace :-> :-loop-optimize-test-validate)
  ;; If development mode, we want to refresh with random datasets.
  (when (not (cfg>> :hscom :is-production))
    (update-unique-datasets))
  ;; Gather  the human strategies metrics when in development mode.
  (when (and (not (cfg>> :hscom :is-production))
             (cfg>> :hsage :run-human-and-hybrid))
    (-loop-optimize-human-strategies)
    (-loop-test-human-strategies))
  ;; Human strategies signals. Let's evaluate human strategies first regardless of development or production mode.
  (when (cfg>> :hsage :run-human-and-hybrid)
    (-loop-test-human-strategies))
  (when (cfg>> :hsage :run-hermes)
    (pmap nil (lambda (instrument)
                (loop for timeframe across (sans-default (cfg>> :hsage :timeframes))
                      do (unless (is-market-close)
                           (cfg< (json:decode-json-from-string (getenv "HERMES_PROFILES" "\"DEFAULT\""))
                                 instrument
                                 timeframe)
                           (retire-agents instrument timeframe)
                           ($log $info "Caching agents for pattern" (list instrument timeframe))
                           (cache-agents-from-db instrument timeframe t)
                           ($log $info "Retrieving datasets for agents.")
                           ($log $info "Beginning agent optimization process.")
                           ;; Optimization.
                           (-loop-optimize instrument timeframe)
                           ;; Signal creation. Development.
                           (when (not (cfg>> :hscom :is-production))
                             (-loop-test instrument timeframe))
                           ($log $info "Done agent optimization process.")
                           (-loop-validate)
                           (uncache-agents-from-db instrument timeframe t))
                         (sync-datasets-to-database)
                         (when (cfg>> :hsage :print-hypothesis-test)
                           (hsage.trading::hypothesis-test))))
          (sans-default (cfg>> :hsage :instruments)))
    (unless (cfg>> :hscom :is-production)
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
            (not (cfg>> :hscom :is-production)))
        (update-unique-datasets)
        (create-job-unique-datasets))
    ;; Signal creation. Production. We create a cron job for this to be
    ;; run every `hscom.hsage:*seconds-interval-testing*` seconds.
    (when (cfg>> :hscom :is-production)
      (when (cfg>> :hsage :run-hermes)
        (create-job-signals (cfg>> :hsage :seconds-interval-testing)))
      (create-job-update-rates)
      (create-job-human-strategies-metrics (cfg>> :hsage :seconds-interval-testing-human-strategies-metrics))
      (create-job-optimize-human-strategies (cfg>> :hsage :seconds-interval-optimizing-human-strategies))
      (clerk:start))
    (if (cfg>> :hsage :run-hermes)
        (if (< (cfg>> :hsage :iterations) 0)
            (loop unless (is-market-close)
                    do (-loop-optimize-test-validate))
            (loop repeat (cfg>> :hsage :iterations)
                  do (-loop-optimize-test-validate)))
        (loop))))
