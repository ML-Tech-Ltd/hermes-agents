(defpackage hermes-agents.db
  (:use :cl :postmodern)
  (:import-from #:hscom.db
		#:conn)
  (:export #:init-database
	   #:drop-database)
  (:nicknames :hsage.db))
(in-package hermes-agents.db)

(defun init-database ()
  "Creates all the necessary tables for Hermes Agents."
  (conn
   (unless (table-exists-p 'datasets)
     (query (:create-table 'datasets
			   ((id :type string)
			    (begin-time :type int8)
			    (end-time :type int8))
			   (:primary-key id))))
   
   (unless (table-exists-p 'indicators)
     ;; Used only for reports, not by our code.
     (query (:create-table 'indicators
			   ((id :type string)
			    (name :type string)
			    (parameters :type (or db-null text[])))
			   (:primary-key id))))
   (unless (table-exists-p 'strategies)
     ;; Used only for reports, not by our code.
     (query (:create-table 'strategies
			   ((owner-id :type string)
			    (indicator-id :type string))
			   (:primary-key owner-id indicator-id))))
   (unless (table-exists-p 'humans)
     ;; Used only for reports, not by our code.
     (query (:create-table 'humans
			   ((id :type string)
			    (name :type string)
			    (arguments :type (or db-null float[])))
			   (:primary-key id))))
   
   (unless (table-exists-p 'metricses)
     (query (:create-table 'metricses
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
			    (trades-lost :type integer)
			    (revenues :type float[])
			    (entry-times :type int8[])
			    (exit-times :type int8[])
			    (entry-prices :type float[])
			    (exit-prices :type float[])
			    (tps :type float[])
			    (sls :type float[])
			    (activations :type (or db-null float[]))
			    (returns :type (or db-null float[])))
			   (:primary-key id))))
   (unless (table-exists-p 'evaluations)
     (query (:create-table 'evaluations
			   ((metrics-id :type string)
			    (owner-id :type string)
			    (iterations :type bigint)
			    (label :type string))
			   (:primary-key metrics-id owner-id))))
   (unless (table-exists-p 'hybrids)
     (query (:create-table 'hybrids
			   ((id :type string)
			    (instrument :type string)
			    (timeframe :type string)
			    (name :type string)
			    (population :type (or db-null float[]))
			    (best-individual :type (or db-null float[])))
			   (:primary-key id))))
   (unless (table-exists-p 'rates)
     (query (:create-table 'rates
			   ((time :type string)
			    (instrument :type string)
			    (timeframe :type string)
			    (open-bid :type double-float)
			    (open-ask :type double-float)
			    (high-bid :type double-float)
			    (high-ask :type double-float)
			    (low-bid :type double-float)
			    (low-ask :type double-float)
			    (close-bid :type double-float)
			    (close-ask :type double-float)
			    (volume :type int))
			   (:primary-key time instrument timeframe))))
   (unless (table-exists-p 'agents)
     (query (:create-table 'agents
			   ((id :type string)
			    (lookahead-count :type integer)
			    (lookbehind-count :type integer)
			    (perceptions-count :type integer)
			    (rules-count :type integer)
			    (perception-fns :type float[])
			    (antecedents :type float[])
			    (consequents :type float[])
			    (creation-begin-time :type (or db-null int8))
			    (creation-end-time :type (or db-null int8))
			    (begin-time :type (or db-null int8))
			    (end-time :type (or db-null int8))
			    (dataset-size :type (or db-null integer))
			    (avg-revenue :type (or db-null double-float))
			    (stdev-revenue :type (or db-null double-float))
			    (total-revenue :type (or db-null double-float))
			    (avg-return :type (or db-null double-float))
			    (total-return :type (or db-null double-float))
			    (avg-max-pos :type (or db-null double-float))
			    (stdev-max-pos :type (or db-null double-float))
			    (avg-max-neg :type (or db-null double-float))
			    (stdev-max-neg :type (or db-null double-float))
			    (avg-tp :type (or db-null double-float))
			    (stdev-tp :type (or db-null double-float))
			    (avg-sl :type (or db-null double-float))
			    (stdev-sl :type (or db-null double-float))
			    (avg-activation :type (or db-null double-float))
			    (stdev-activation :type (or db-null double-float))
			    (max-tp :type (or db-null double-float))
			    (min-tp :type (or db-null double-float))
			    (max-sl :type (or db-null double-float))
			    (min-sl :type (or db-null double-float))
			    (trades-won :type (or db-null integer))
			    (trades-lost :type (or db-null integer))
			    (revenues :type (or db-null float[]))
			    (entry-times :type (or db-null int8[]))
			    (exit-times :type (or db-null int8[]))
			    (entry-prices :type (or db-null float[]))
			    (exit-prices :type (or db-null float[]))
			    (tps :type (or db-null float[]))
			    (sls :type (or db-null float[]))
			    (activations :type (or db-null float[]))
			    (returns :type (or db-null float[])))
			   (:primary-key id))))
   (unless (table-exists-p 'agents-patterns)
     (query (:create-table 'agents-patterns
			   ((agent-id :type string)
			    (pattern-id :type string))
			   (:primary-key agent-id pattern-id))))
   (unless (table-exists-p 'patterns)
     (query (:create-table 'patterns
			   ((id :type string)
			    (type :type string)
			    (instrument :type string)
			    (timeframe :type string))
			   (:primary-key id)
			   (:unique type instrument timeframe))))
   (unless (table-exists-p 'patterns-trades)
     (query (:create-table 'patterns-trades
			   ((pattern-id :type string)
			    (trade-id :type string))
			   (:primary-key pattern-id trade-id))))
   (unless (table-exists-p 'trades)
     (query (:create-table 'trades
			   ((id :type string)
			    (label :type string)
			    (owner-id :type (or db-null string))
			    (creation-time :type int8)
			    (decision :type string)
			    (result :type (or db-null double-float))
			    (tp :type double-float)
			    (sl :type double-float)
			    (activation :type double-float)
			    (entry-price :type double-float)
			    (entry-time :type (or db-null int8))
			    (exit-time :type (or db-null int8))

			    (train-begin-time :type int8)
			    (train-end-time :type int8)
			    (train-dataset-size :type integer)
			    (train-avg-revenue :type double-float)
			    (train-avg-return :type (or db-null double-float))
			    (train-total-return :type (or db-null double-float))
			    (train-stdev-revenue :type double-float)
			    (train-total-revenue :type double-float)
			    (train-avg-max-pos :type double-float)
			    (train-stdev-max-pos :type double-float)
			    (train-avg-max-neg :type double-float)
			    (train-stdev-max-neg :type double-float)
			    (train-avg-tp :type double-float)
			    (train-stdev-tp :type double-float)
			    (train-avg-sl :type double-float)
			    (train-avg-activation :type double-float)
			    (train-stdev-activation :type double-float)
			    (train-stdev-sl :type double-float)
			    (train-max-tp :type double-float)
			    (train-min-tp :type double-float)
			    (train-max-sl :type double-float)
			    (train-min-sl :type double-float)
			    (train-trades-won :type integer)
			    (train-trades-lost :type integer)
			    (train-revenues :type float[])
			    (train-entry-times :type int8[])
			    (train-exit-times :type int8[])
			    (train-entry-prices :type float[])
			    (train-exit-prices :type float[])
			    (train-tps :type float[])
			    (train-sls :type float[])
			    (train-activations :type (or db-null float[]))
			    (train-returns :type (or db-null float[]))
			    
			    (test-begin-time :type int8)
			    (test-end-time :type int8)
			    (test-dataset-size :type integer)
			    (test-avg-revenue :type double-float)
			    (test-avg-return :type (or db-null double-float))
			    (test-total-return :type (or db-null double-float))
			    (test-stdev-revenue :type double-float)
			    (test-total-revenue :type double-float)
			    (test-avg-max-pos :type double-float)
			    (test-stdev-max-pos :type double-float)
			    (test-avg-max-neg :type double-float)
			    (test-stdev-max-neg :type double-float)
			    (test-avg-tp :type double-float)
			    (test-stdev-tp :type double-float)
			    (test-avg-sl :type double-float)
			    (test-avg-activation :type double-float)
			    (test-stdev-activation :type double-float)
			    (test-stdev-sl :type double-float)
			    (test-max-tp :type double-float)
			    (test-min-tp :type double-float)
			    (test-max-sl :type double-float)
			    (test-min-sl :type double-float)
			    (test-trades-won :type integer)
			    (test-trades-lost :type integer)
			    (test-revenues :type float[])
			    (test-entry-times :type int8[])
			    (test-exit-times :type int8[])
			    (test-entry-prices :type float[])
			    (test-exit-prices :type float[])
			    (test-tps :type float[])
			    (test-sls :type float[])
			    (test-activations :type (or db-null float[]))
			    (test-returns :type (or db-null float[])))
			   (:primary-key id))))))
;; (init-database)

(defun drop-database ()
  (conn
   ;; (drop-table 'datasets)
   (drop-table 'metricses)
   (drop-table 'evaluations)
   (drop-table 'hybrids)
   (drop-table 'agents)
   (drop-table 'agents-patterns)
   (drop-table 'patterns)
   (drop-table 'patterns-trades)
   (drop-table 'trades)))
;; (drop-database)
