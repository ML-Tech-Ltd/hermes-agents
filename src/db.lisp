(defpackage hermes-agents.db
  (:use :cl :postmodern)
  (:import-from #:hscom.db
                #:conn)
  (:import-from #:hscom.utils
                #:assoccess)
  (:import-from #:hscom.hsage
                #:*all-timeframes*
                #:*timeframes*
                #:*forex*
                #:*methods*
                #:*environments*)
  (:import-from #:hsper
                #:get-human-strategies)
  (:import-from #:hsage.trading
                #:insert-strategy)
  (:export #:init-database
           #:drop-database)
  (:nicknames :hsage.db))
(in-package hermes-agents.db)

(defun init-database ()
  "Creates all the necessary tables for Hermes Agents."
  (conn
   (unless (table-exists-p 'instruments)
     (query (:create-table 'instruments
                ((id :type int8 :identity-always t)
                 (name :type string))
              (:primary-key id)))
     (loop for instrument in *forex*
           do (query (:insert-into 'instruments :set 'name (format nil "~a" instrument)))))

   (unless (table-exists-p 'timeframes)
     (query (:create-table 'timeframes
                ((id :type int8 :identity-always t)
                 (name :type string))
              (:primary-key id)))
     (loop for timeframe in *all-timeframes*
           do (query (:insert-into 'timeframes :set 'name (format nil "~a" timeframe)))))

   (unless (table-exists-p 'methods)
     (query (:create-table 'methods
                ((id :type int8 :identity-always t)
                 (name :type string))
              (:primary-key id)))
     (loop for method in *methods*
           do (query (:insert-into 'methods :set 'name (format nil "~a" method)))))

   (unless (table-exists-p 'environments)
     (query (:create-table 'environments
                ((id :type int8 :identity-always t)
                 (name :type string))
              (:primary-key id)))
     (loop for environment in *environments*
           do (query (:insert-into 'environments :set 'name (format nil "~a" environment)))))

   (unless (table-exists-p 'strategies)
     (query (:create-table 'strategies
                ((id :type int8 :identity-always t)
                 (instrument-id :type int8)
                 (timeframe-id :type int8)
                 (method-id :type int8)
                 (name :type (or db-null string))
                 (parameters :type (or db-null string))
                 (population :type (or db-null float[]))
                 (best-individual :type (or db-null float[]))
                 (iterations :type int8 :default 0))
              (:primary-key id)
              (:foreign-key (instrument-id) (instruments id))
              (:foreign-key (timeframe-id) (timeframes id))
              (:foreign-key (method-id) (methods id))))
     ;; Humans and hybrids.
     (loop for strategy in (get-human-strategies)
           do (loop for instrument in *forex*
                    do (loop for timeframe in *timeframes*
                             do (progn
                                  ;; Human.
                                  (insert-strategy instrument timeframe :human
                                                   :name (assoccess strategy :name))
                                  ;; Hybrid.
                                  (insert-strategy instrument timeframe :hybrid
                                                   :name (assoccess strategy :name))))))
     ;; Hermes.
     (loop for instrument in *forex*
           do (loop for timeframe in *timeframes*
                    do (insert-strategy instrument timeframe :hermes))))

   (unless (table-exists-p 'metrics)
     (query (:create-table 'metrics
                ((id :type string)
                 (begin-time :type int8)
                 (end-time :type int8)
                 (dataset-size :type integer)
                 (avg-revenue :type double-float)
                 (avg-return :type (or db-null double-float))
                 (total-return :type (or db-null double-float))
                 (stdev-revenue :type double-float)
                 (total-revenue :type double-float)
                 (avg-max-pos :type double-float)
                 (stdev-max-pos :type double-float)
                 (avg-max-neg :type double-float)
                 (stdev-max-neg :type double-float)
                 (avg-tp :type double-float)
                 (stdev-tp :type double-float)
                 (avg-sl :type double-float)
                 (avg-activation :type double-float)
                 (stdev-activation :type double-float)
                 (stdev-sl :type double-float)
                 (max-tp :type double-float)
                 (min-tp :type double-float)
                 (max-sl :type double-float)
                 (min-sl :type double-float)
                 (trades-won :type integer)
                 (trades-lost :type integer))
              (:primary-key id))))

   (unless (table-exists-p 'metrics-strategies)
     (query (:create-table 'metrics-strategies
                ((metrics-id :type string)
                 (strategy-id :type int8)
                 (environment-id :type int8)
                 (timestamp :type int8))
              (:primary-key metrics-id strategy-id environment-id)
              (:foreign-key (metrics-id) (metrics id))
              (:foreign-key (strategy-id) (strategies id))
              (:foreign-key (environment-id) (environments id)))))

   (unless (table-exists-p 'trades)
     (query (:create-table 'trades
                ((id :type string)
                 (metrics-id :type (or db-null string))
                 (decision :type string)
                 (return :type (or db-null double-float))
                 (revenue :type (or db-null double-float))
                 (tp :type double-float)
                 (sl :type double-float)
                 (activation :type double-float)
                 (entry-price :type double-float)
                 (exit-price :type (or db-null double-float))
                 (entry-time :type int8)
                 (exit-time :type (or db-null int8)))
              (:primary-key id)
              (:foreign-key (metrics-id) (metrics id)))))

   (unless (table-exists-p 'signals)
     (query (:create-table 'signals
                ((id :type string)
                 (trade-id :type string)
                 (strategy-id :type int8)
                 (training-metrics-id :type string)
                 (testing-metrics-id :type string)
                 (timestamp :type int8))
              (:primary-key id)
              (:foreign-key (trade-id) (trades id))
              (:foreign-key (strategy-id) (strategies id))
              (:foreign-key (training-metrics-id) (metrics id))
              (:foreign-key (testing-metrics-id) (metrics id)))))

   (unless (table-exists-p 'agents)
     (query (:create-table 'agents
                ((id :type string)
                 (strategy-id :type int8)
                 (metrics-id :type (or db-null string))
                 (retired :type boolean :default nil)
                 (lookahead :type integer)
                 (lookbehind :type integer)
                 (perceptions-count :type integer)
                 (rules-count :type integer)
                 (perception-fns :type float[])
                 (antecedents :type float[])
                 (consequents :type float[])
                 (creation-begin-time :type (or db-null int8))
                 (creation-end-time :type (or db-null int8))
                 (timestamp :type int8))
              (:primary-key id)
              (:foreign-key (strategy-id) (strategies id))
              (:foreign-key (metrics-id) (metrics id)))))

   (unless (table-exists-p 'rates)
     (query (:create-table 'rates
                ((time :type int8)
                 (instrument :type string)
                 (timeframe :type string)
                 (complete :type boolean)
                 (open-bid :type double-float)
                 (open-ask :type double-float)
                 (high-bid :type double-float)
                 (high-ask :type double-float)
                 (low-bid :type double-float)
                 (low-ask :type double-float)
                 (close-bid :type double-float)
                 (close-ask :type double-float)
                 (volume :type int))
              (:primary-key time instrument timeframe))))))
;; (init-database)

(defun drop-database ()
  (conn
   (drop-table 'signals)
   (drop-table 'trades)
   (drop-table 'agents)
   (drop-table 'metrics-strategies)
   (drop-table 'strategies)
   (drop-table 'metrics)
   (drop-table 'instruments)
   (drop-table 'timeframes)
   (drop-table 'methods)
   (drop-table 'environments)))
;; (drop-database)
