(defpackage hermes-agents.trading
  (:use #:cl #:ciel #:postmodern #:hsage.log #:hscom.log #:hu.dwim.def)
  (:import-from #:defenum
                #:defenum)
  (:import-from #:alexandria
                #:copy-sequence
                #:make-keyword
                #:standard-deviation
                #:if-let)
  (:import-from #:hscom.db
                #:conn)
  (:import-from #:hscom.utils
                #:format-table
                #:comment
                #:assoccess
                #:assoccess-default
                #:random-float
                #:dbg
                #:stringify
                #:whole-reals-to-integers)
  (:import-from #:hscom.hsage
                #:*fis-method*
                #:*min-pips-sl*
                #:*exhaust-rules-in-creation-dataset-p*
                #:*type-groups*
                #:*ignore-test-conditions-p*
                #:*lookahead*
                #:*lookbehind*
                #:*unique-point-p*
                #:*unique-count*
                #:*max-agents-count*
                #:*kill-agents-count*)
  (:import-from #:hsper
                #:get-perceptions
                #:nth-perception
                #:get-perceptions-count
                #:get-human-strategies
                #:=>strategy-rsi-stoch-macd)
  (:import-from #:hsinp.rates
                #:to-pips
                #:get-tp-sl
                #:from-pips
                #:get-rates-range-big
                #:get-input-dataset
                #:get-output-dataset
                #:get-tp-sl
                #:get-unique-dataset)
  (:import-from #:hsage.log
                #:push-to-log)
  (:import-from #:hsage.utils
                #:prepare-agents-properties
                #:format-rr)
  (:import-from #:hsint
                #:eval-ifis-gen
                #:eval-ifis-idx)
  (:import-from #:genetic-algorithm
                #:get-gens
                #:select-the-best
                #:get-fit
                #:make-being
                #:-population-
                #:-best-being-)
  (:export #:agent
           #:log-agent
           #:optimization
           #:evaluate-trade
           #:insert-trade
           #:get-same-direction-outputs-idxs
           #:make-ifis
           #:evaluate-agent
           #:evaluate-agents
           #:test-agents
           #:init-patterns
           #:add-agent
           #:sync-agents
           #:remove-agent
           #:eval-agent
           #:get-agent
           #:get-agents-some
           #:get-agents-all
           #:get-agent-by-id
           #:get-agents-count
           #:agents-to-alists
           #:test-most-activated-agents
           #:gen-agent
           #:gen-agents
           #:update-agent-fitnesses
           #:update-agents-fitnesses
           #:wipe-agents
           #:get-patterns
           #:insert-pattern
           #:get-agent-ids-from-patterns
           #:get-trades-grouped
           #:get-trades-flat
           #:get-trades-nested
           #:get-trades-by-ids
           #:describe-trades
           #:describe-agents
           #:get-trade-result
           #:get-global-revenue
           #:validate-trades
           #:re-validate-trades
           #:delete-trades
           #:get-hybrid
           #:get-human-name
           #:get-hybrid-name
           #:best-individual
           #:test-hybrid-strategy
           #:get-hybrid-id
           #:cache-agents-from-db
           #:uncache-agents-from-db
           #:retire-agents-from-db)
  (:nicknames #:hsage.trading))
(in-package :hermes-agents.trading)

(ciel:enable-punch-syntax)

(defenum perceptions
  (decompress
   fuzzy-rule
   perception-function
   ))

;; (defparameter *rates* (hsinp.rates:fracdiff (hsinp.rates:get-rates-count-big :AUD_USD :H1 20000)))
(defparameter *agents-cache* (make-hash-table :test 'equal :synchronized t))
;; (maphash (lambda (k v) (dbg k v)) *agents-cache*)
;; (cache-agents-from-db :EUR_USD :M15)
;; (setf (gethash (list :EUR_USD :M15 '(:single)) *agents-cache*) 5)

(defclass agent ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id)
   (retired :col-type (or db-null boolean) :initarg :retired :initform nil)
   (lookahead-count :col-type integer :initarg :lookahead-count :accessor lookahead-count)
   (lookbehind-count :col-type integer :initarg :lookbehind-count :accessor lookbehind-count)
   (perceptions-count :col-type integer :initarg :perceptions-count :accessor perceptions-count)
   (rules-count :col-type integer :initarg :rules-count :accessor rules-count)
   (perception-fns :col-type string :initarg :perception-fns :accessor perception-fns)
   (antecedents :col-type string :initarg :antecedents :accessor antecedents)
   (consequents :col-type string :initarg :consequents :accessor consequents)
   (creation-begin-time :col-type (or db-null int8) :initarg :creation-begin-time :initform :null)
   (creation-end-time :col-type (or db-null int8) :initarg :creation-end-time :initform :null)
   (begin-time :col-type (or db-null int8) :initarg :begin-time :initform :null)
   (end-time :col-type (or db-null int8) :initarg :end-time :initform :null)
   (dataset-size :col-type (or db-null integer) :initarg :dataset-size :initform :null)
   (avg-revenue :col-type (or db-null double-float) :initarg :avg-revenue :initform :null)
   (stdev-revenue :col-type (or db-null double-float) :initarg :stdev-revenue :initform :null)
   (total-revenue :col-type (or db-null double-float) :initarg :total-revenue :initform :null)
   (avg-return :col-type (or db-null double-float) :initarg :avg-return :initform :null)
   (total-return :col-type (or db-null double-float) :initarg :total-return :initform :null)
   (avg-max-pos :col-type (or db-null double-float) :initarg :avg-max-pos :initform :null)
   (stdev-max-pos :col-type (or db-null double-float) :initarg :stdev-max-pos :initform :null)
   (avg-max-neg :col-type (or db-null double-float) :initarg :avg-max-neg :initform :null)
   (stdev-max-neg :col-type (or db-null double-float) :initarg :stdev-max-neg :initform :null)
   (avg-tp :col-type (or db-null double-float) :initarg :avg-tp :initform :null)
   (stdev-tp :col-type (or db-null double-float) :initarg :stdev-tp :initform :null)
   (avg-sl :col-type (or db-null double-float) :initarg :avg-sl :initform :null)
   (stdev-sl :col-type (or db-null double-float) :initarg :stdev-sl :initform :null)
   (avg-activation :col-type (or db-null double-float) :initarg :avg-activation :initform :null)
   (stdev-activation :col-type (or db-null double-float) :initarg :stdev-activation :initform :null)
   (max-tp :col-type (or db-null double-float) :initarg :max-tp :initform :null)
   (min-tp :col-type (or db-null double-float) :initarg :min-tp :initform :null)
   (max-sl :col-type (or db-null double-float) :initarg :max-sl :initform :null)
   (min-sl :col-type (or db-null double-float) :initarg :min-sl :initform :null)
   (trades-won :col-type (or db-null integer) :initarg :trades-won :initform :null)
   (trades-lost :col-type (or db-null integer) :initarg :trades-lost :initform :null)
   (revenues :col-type (or db-null float[]) :initarg :revenues :initform :null)
   (entry-times :col-type (or db-null int8[]) :initarg :entry-times :initform :null)
   (exit-times :col-type (or db-null int8[]) :initarg :exit-times :initform :null)
   (entry-prices :col-type (or db-null float[]) :initarg :entry-prices :initform :null)
   (exit-prices :col-type (or db-null float[]) :initarg :exit-prices :initform :null)
   (tps :col-type (or db-null float[]) :initarg :tps :initform :null)
   (sls :col-type (or db-null float[]) :initarg :sls :initform :null)
   (activations :col-type (or db-null float[]) :initarg :activations :initform :null)
   (returns :col-type (or db-null float[]) :initarg :returns :initform :null))
  (:metaclass postmodern:dao-class)
  (:table-name agents)
  (:keys id))

(defclass agent-pattern ()
  ((agent-id :col-type string :initarg :agent-id)
   (pattern-id :col-type string :initarg :pattern-id))
  (:metaclass postmodern:dao-class)
  (:table-name agents-patterns)
  (:keys agent-id pattern-id))

(defclass pattern-trade ()
  ((pattern-id :col-type string :initarg :pattern-id)
   (trade-id :col-type string :initarg :trade-id))
  (:metaclass postmodern:dao-class)
  (:table-name patterns-trades)
  (:keys pattern-id trade-id))

(defclass pattern ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (type :col-type string :initarg :type)
   (instrument :col-type string :initarg :instrument)
   (timeframe :col-type string :initarg :timeframe))
  (:metaclass postmodern:dao-class)
  (:table-name patterns)
  (:keys id))

(defclass trade ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (label :col-type string :initform "" :initarg :label)
   (owner-id :col-type (or db-null string) :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :owner-id)
   (creation-time :col-type int8 :initarg :creation-time)
   (decision :col-type string :initarg :decision)
   (result :col-type (or db-null double-float) :initarg :result)
   (tp :col-type double-float :initarg :tp)
   (sl :col-type double-float :initarg :sl)
   (activation :col-type double-float :initarg :activation)
   (entry-price :col-type double-float :initarg :entry-price)
   (entry-time :col-type int8 :initarg :entry-time)
   (exit-time :col-type int8 :initarg :exit-time)
   (train-begin-time :col-type int8 :initarg :train-begin-time :accessor train-begin-time)
   (train-end-time :col-type int8 :initarg :train-end-time)
   (test-begin-time :col-type int8 :initarg :test-begin-time)
   (test-end-time :col-type int8 :initarg :test-end-time)
   (train-dataset-size :col-type integer :initarg :train-dataset-size)
   (test-dataset-size :col-type integer :initarg :test-dataset-size)
   (train-avg-revenue :col-type double-float :initarg :train-avg-revenue)
   (test-avg-revenue :col-type double-float :initarg :test-avg-revenue)
   (train-stdev-revenue :col-type double-float :initarg :train-stdev-revenue)
   (test-stdev-revenue :col-type double-float :initarg :test-stdev-revenue)
   (train-total-revenue :col-type double-float :initarg :train-total-revenue)
   (test-total-revenue :col-type double-float :initarg :test-total-revenue)
   (train-avg-return :col-type double-float :initarg :train-avg-return)
   (test-avg-return :col-type double-float :initarg :test-avg-return)
   (train-total-return :col-type double-float :initarg :train-total-return)
   (test-total-return :col-type double-float :initarg :test-total-return)
   (train-avg-max-pos :col-type double-float :initarg :train-avg-max-pos)
   (test-avg-max-pos :col-type double-float :initarg :test-avg-max-pos)
   (train-stdev-max-pos :col-type double-float :initarg :train-stdev-max-pos)
   (test-stdev-max-pos :col-type double-float :initarg :test-stdev-max-pos)
   (train-avg-max-neg :col-type double-float :initarg :train-avg-max-neg)
   (test-avg-max-neg :col-type double-float :initarg :test-avg-max-neg)
   (train-stdev-max-neg :col-type double-float :initarg :train-stdev-max-neg)
   (test-stdev-max-neg :col-type double-float :initarg :test-stdev-max-neg)
   (train-avg-tp :col-type double-float :initarg :train-avg-tp)
   (test-avg-tp :col-type double-float :initarg :test-avg-tp)
   (train-stdev-tp :col-type double-float :initarg :train-stdev-tp)
   (test-stdev-tp :col-type double-float :initarg :test-stdev-tp)
   (train-avg-sl :col-type double-float :initarg :train-avg-sl)
   (test-avg-sl :col-type double-float :initarg :test-avg-sl)
   (train-stdev-sl :col-type double-float :initarg :train-stdev-sl)
   (test-stdev-sl :col-type double-float :initarg :test-stdev-sl)
   (train-avg-activation :col-type double-float :initarg :train-avg-activation)
   (test-avg-activation :col-type double-float :initarg :test-avg-activation)
   (train-stdev-activation :col-type double-float :initarg :train-stdev-activation)
   (test-stdev-activation :col-type double-float :initarg :test-stdev-activation)
   (train-max-tp :col-type double-float :initarg :train-max-tp)
   (test-max-tp :col-type double-float :initarg :test-max-tp)
   (train-min-tp :col-type double-float :initarg :train-min-tp)
   (test-min-tp :col-type double-float :initarg :test-min-tp)
   (train-max-sl :col-type double-float :initarg :train-max-sl)
   (test-max-sl :col-type double-float :initarg :test-max-sl)
   (train-min-sl :col-type double-float :initarg :train-min-sl)
   (test-min-sl :col-type double-float :initarg :test-min-sl)
   (train-trades-won :col-type double-float :initarg :train-trades-won)
   (test-trades-won :col-type double-float :initarg :test-trades-won)
   (train-trades-lost :col-type double-float :initarg :train-trades-lost)
   (test-trades-lost :col-type double-float :initarg :test-trades-lost)
   (train-revenues :col-type float[] :initarg :train-revenues)
   (test-revenues :col-type float[] :initarg :test-revenues)
   (train-entry-times :col-type int8[] :initarg :train-entry-times)
   (test-entry-times :col-type int8[] :initarg :test-entry-times)
   (train-exit-times :col-type int8[] :initarg :train-exit-times)
   (test-exit-times :col-type int8[] :initarg :test-exit-times)
   (train-entry-prices :col-type float[] :initarg :train-entry-prices)
   (test-entry-prices :col-type float[] :initarg :test-entry-prices)
   (train-exit-prices :col-type float[] :initarg :train-exit-prices)
   (test-exit-prices :col-type float[] :initarg :test-exit-prices)
   (train-tps :col-type float[] :initarg :train-tps)
   (test-tps :col-type float[] :initarg :test-tps)
   (train-sls :col-type float[] :initarg :train-sls)
   (test-sls :col-type float[] :initarg :test-sls)
   (train-activations :col-type float[] :initarg :train-activations)
   (test-activations :col-type float[] :initarg :test-activations)
   (train-returns :col-type float[] :initarg :train-returns)
   (test-returns :col-type float[] :initarg :test-returns))
  (:metaclass postmodern:dao-class)
  (:table-name trades)
  (:keys id))

(defclass metrics ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (begin-time :col-type int8 :initarg :begin-time :accessor begin-time)
   (end-time :col-type int8 :initarg :end-time :accessor end-time)
   (dataset-size :col-type integer :initarg :dataset-size :accessor dataset-size)
   (avg-revenue :col-type double-float :initarg :avg-revenue :accessor avg-revenue)
   (stdev-revenue :col-type double-float :initarg :stdev-revenue :accessor stdev-revenue)
   (total-revenue :col-type double-float :initarg :total-revenue :accessor total-revenue)
   (avg-return :col-type double-float :initarg :avg-return :accessor avg-return)
   (total-return :col-type double-float :initarg :total-return :accessor total-return)
   (avg-max-pos :col-type double-float :initarg :avg-max-pos :accessor avg-max-pos)
   (stdev-max-pos :col-type double-float :initarg :stdev-max-pos :accessor stdev-max-pos)
   (avg-max-neg :col-type double-float :initarg :avg-max-neg :accessor avg-max-neg)
   (stdev-max-neg :col-type double-float :initarg :stdev-max-neg :accessor stdev-max-neg)
   (avg-tp :col-type double-float :initarg :avg-tp :accessor avg-tp)
   (stdev-tp :col-type double-float :initarg :stdev-tp :accessor stdev-tp)
   (avg-sl :col-type double-float :initarg :avg-sl :accessor avg-sl)
   (stdev-sl :col-type double-float :initarg :stdev-sl :accessor stdev-sl)
   (avg-activation :col-type double-float :initarg :avg-activation :accessor avg-activation)
   (stdev-activation :col-type double-float :initarg :stdev-activation :accessor stdev-activation)
   (max-tp :col-type double-float :initarg :max-tp :accessor max-tp)
   (min-tp :col-type double-float :initarg :min-tp :accessor min-tp)
   (max-sl :col-type double-float :initarg :max-sl :accessor max-sl)
   (min-sl :col-type double-float :initarg :min-sl :accessor min-sl)
   (trades-won :col-type double-float :initarg :trades-won :accessor trades-won)
   (trades-lost :col-type double-float :initarg :trades-lost :accessor trades-lost)
   (revenues :col-type float[] :initarg :revenues :accessor revenues)
   (entry-times :col-type int8[] :initarg :entry-times :accessor entry-times)
   (exit-times :col-type int8[] :initarg :exit-times :accessor exit-times)
   (entry-prices :col-type float[] :initarg :entry-prices :accessor entry-prices)
   (exit-prices :col-type float[] :initarg :exit-prices :accessor exit-prices)
   (tps :col-type float[] :initarg :tps :accessor tps)
   (sls :col-type float[] :initarg :sls :accessor sls)
   (activations :col-type float[] :initarg :activations :accessor activations)
   (returns :col-type float[] :initarg :returns :accessor returns))
  (:metaclass postmodern:dao-class)
  (:table-name metricses)
  (:keys id))

(defclass evaluation ()
  ((metrics-id :col-type string :initarg :metrics-id :accessor metrics-id)
   (owner-id :col-type string :initarg :owner-id :accessor owner-id)
   (label :col-type string :initform "" :initarg :label :accessor label)
   (iterations :col-type bigint :initarg :iterations :accessor iterations))
  (:metaclass postmodern:dao-class)
  (:table-name evaluations)
  (:keys metrics-id owner-id label))

(defclass hybrid ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (instrument :col-type string :initarg :instrument :accessor instrument)
   (timeframe :col-type string :initarg :timeframe :accessor timeframe)
   (name :col-type string :initform "" :initarg :name :accessor name)
   (population :col-type float[] :initarg :population :accessor population)
   (best-individual :col-type float[] :initarg :best-individual :accessor best-individual))
  (:metaclass postmodern:dao-class)
  (:table-name hybrids)
  (:keys id))

(def (function d) get-hybrid-id (hybrid)
  (id hybrid))

(def (function d) insert-trade-human (owner-id instrument timeframe types train-fitnesses test-fitnesses tp sl activation rates creation-time label &key (metric-or-signal :signal))
  (conn (let* ((trade-id (assoccess (query (:select 'patterns.*
                                            (:as 'trades.id 'tid)
                                            :distinct-on 'trades.id
                                            :from 'trades
                                            :inner-join 'patterns-trades
                                            :on (:= 'trades.id 'patterns-trades.trade-id)
                                            :inner-join 'patterns
                                            :on (:= 'patterns-trades.pattern-id 'patterns.id)
                                            :where (:and (:= 'label label)
                                                    (:= 'instrument (format nil "~a" instrument))
                                                    (:= 'timeframe (format nil "~a" timeframe))))
                                           :alist)
                                    :tid))
               (trade (when trade-id (get-dao 'trade trade-id))))
          (if trade
              (if (eq metric-or-signal :signal)
                  (progn
                    (setf (slot-value trade 'creation-time) creation-time)
                    (setf (slot-value trade 'decision)
                          (if (or (= sl 0) (= tp 0))
                              "HOLD"
                              (if (> tp 0)
                                  "BUY"
                                  "SELL")))
                    (setf (slot-value trade 'tp) tp)
                    (setf (slot-value trade 'sl) sl)
                    (setf (slot-value trade 'activation) activation)
                    (setf (slot-value trade 'entry-price)
                          (if (> tp 0)
                              (hsinp.rates:->close-ask (last-elt rates))
                              (hsinp.rates:->close-bid (last-elt rates))))
                    (setf (slot-value trade 'entry-time)
                          (/ (read-from-string (assoccess (last-elt rates) :time)) 1000000))
                    (update-dao trade))
                  (progn
                    ;; TODO: Holy... we need a better way of doing this.
                    ;; Training.
                    (setf (slot-value trade 'train-begin-time) (assoccess-default train-fitnesses :begin-time -1))
                    (setf (slot-value trade 'train-end-time) (assoccess-default train-fitnesses :end-time -1))
                    (setf (slot-value trade 'test-begin-time) (assoccess-default train-fitnesses :begin-time -1))
                    (setf (slot-value trade 'test-end-time) (assoccess-default train-fitnesses :end-time -1))

                    (setf (slot-value trade 'train-dataset-size) (assoccess-default train-fitnesses :dataset-size -1))
                    (setf (slot-value trade 'train-avg-revenue) (assoccess-default train-fitnesses :avg-revenue 0))
                    (setf (slot-value trade 'train-stdev-revenue) (assoccess-default train-fitnesses :stdev-revenue -1))
                    (setf (slot-value trade 'train-total-revenue) (assoccess-default train-fitnesses :total-revenue 0))
                    (setf (slot-value trade 'train-avg-return) (assoccess-default train-fitnesses :avg-return 0))
                    (setf (slot-value trade 'train-total-return) (assoccess-default train-fitnesses :total-return 0))
                    (setf (slot-value trade 'train-avg-max-pos) (assoccess-default train-fitnesses :avg-max-pos 0))
                    (setf (slot-value trade 'train-stdev-max-pos) (assoccess-default train-fitnesses :stdev-max-pos -1))
                    (setf (slot-value trade 'train-avg-max-neg) (assoccess-default train-fitnesses :avg-max-neg 0))
                    (setf (slot-value trade 'train-stdev-max-neg) (assoccess-default train-fitnesses :stdev-max-neg -1))
                    (setf (slot-value trade 'train-avg-tp) (assoccess-default train-fitnesses :avg-tp 0))
                    (setf (slot-value trade 'train-stdev-tp) (assoccess-default train-fitnesses :stdev-tp -1))
                    (setf (slot-value trade 'train-avg-sl) (assoccess-default train-fitnesses :avg-sl 0))
                    (setf (slot-value trade 'train-stdev-sl) (assoccess-default train-fitnesses :stdev-sl -1))
                    (setf (slot-value trade 'train-avg-activation) (assoccess-default train-fitnesses :avg-activation 0))
                    (setf (slot-value trade 'train-stdev-activation) (assoccess-default train-fitnesses :stdev-activation -1))
                    (setf (slot-value trade 'train-max-tp) (assoccess-default train-fitnesses :max-tp 0))
                    (setf (slot-value trade 'train-min-tp) (assoccess-default train-fitnesses :min-tp 0))
                    (setf (slot-value trade 'train-max-sl) (assoccess-default train-fitnesses :max-sl 0))
                    (setf (slot-value trade 'train-min-sl) (assoccess-default train-fitnesses :min-sl 0))
                    (setf (slot-value trade 'train-trades-won) (assoccess-default train-fitnesses :trades-won -1))
                    (setf (slot-value trade 'train-trades-lost) (assoccess-default train-fitnesses :trades-lost -1))
                    (setf (slot-value trade 'train-revenues) (apply #'vector (assoccess-default train-fitnesses :revenues '(0.0))))
                    (setf (slot-value trade 'train-entry-times) (apply #'vector (assoccess-default train-fitnesses :entry-times '(0.0))))
                    (setf (slot-value trade 'train-exit-times) (apply #'vector (assoccess-default train-fitnesses :exit-times '(0.0))))
                    (setf (slot-value trade 'train-entry-prices) (apply #'vector (assoccess-default train-fitnesses :entry-prices '(0.0))))
                    (setf (slot-value trade 'train-exit-prices) (apply #'vector (assoccess-default train-fitnesses :exit-prices '(0.0))))
                    (setf (slot-value trade 'train-tps) (apply #'vector (assoccess-default train-fitnesses :tps '(0.0))))
                    (setf (slot-value trade 'train-sls) (apply #'vector (assoccess-default train-fitnesses :sls '(0.0))))
                    (setf (slot-value trade 'train-activations) (apply #'vector (assoccess-default train-fitnesses :activations '(0.0))))
                    (setf (slot-value trade 'train-returns) (apply #'vector (assoccess-default train-fitnesses :returns '(0.0))))

                    ;; Testing.
                    (setf (slot-value trade 'test-dataset-size) (assoccess-default test-fitnesses :dataset-size -1))
                    (setf (slot-value trade 'test-avg-revenue) (assoccess-default test-fitnesses :avg-revenue 0))
                    (setf (slot-value trade 'test-stdev-revenue) (assoccess-default test-fitnesses :stdev-revenue -1))
                    (setf (slot-value trade 'test-total-revenue) (assoccess-default test-fitnesses :total-revenue 0))
                    (setf (slot-value trade 'test-avg-return) (assoccess-default test-fitnesses :avg-return 0))
                    (setf (slot-value trade 'test-total-return) (assoccess-default test-fitnesses :total-return 0))
                    (setf (slot-value trade 'test-avg-max-pos) (assoccess-default test-fitnesses :avg-max-pos 0))
                    (setf (slot-value trade 'test-stdev-max-pos) (assoccess-default test-fitnesses :stdev-max-pos -1))
                    (setf (slot-value trade 'test-avg-max-neg) (assoccess-default test-fitnesses :avg-max-neg 0))
                    (setf (slot-value trade 'test-stdev-max-neg) (assoccess-default test-fitnesses :stdev-max-neg -1))
                    (setf (slot-value trade 'test-avg-tp) (assoccess-default test-fitnesses :avg-tp 0))
                    (setf (slot-value trade 'test-stdev-tp) (assoccess-default test-fitnesses :stdev-tp -1))
                    (setf (slot-value trade 'test-avg-sl) (assoccess-default test-fitnesses :avg-sl 0))
                    (setf (slot-value trade 'test-stdev-sl) (assoccess-default test-fitnesses :stdev-sl -1))
                    (setf (slot-value trade 'test-avg-activation) (assoccess-default test-fitnesses :avg-activation 0))
                    (setf (slot-value trade 'test-stdev-activation) (assoccess-default test-fitnesses :stdev-activation -1))
                    (setf (slot-value trade 'test-max-tp) (assoccess-default test-fitnesses :max-tp 0))
                    (setf (slot-value trade 'test-min-tp) (assoccess-default test-fitnesses :min-tp 0))
                    (setf (slot-value trade 'test-max-sl) (assoccess-default test-fitnesses :max-sl 0))
                    (setf (slot-value trade 'test-min-sl) (assoccess-default test-fitnesses :min-sl 0))
                    (setf (slot-value trade 'test-trades-won) (assoccess-default test-fitnesses :trades-won -1))
                    (setf (slot-value trade 'test-trades-lost) (assoccess-default test-fitnesses :trades-lost -1))
                    (setf (slot-value trade 'test-revenues) (apply #'vector (assoccess-default test-fitnesses :revenues '(0.0))))
                    (setf (slot-value trade 'test-entry-times) (apply #'vector (assoccess-default test-fitnesses :entry-times '(0.0))))
                    (setf (slot-value trade 'test-exit-times) (apply #'vector (assoccess-default test-fitnesses :exit-times '(0.0))))
                    (setf (slot-value trade 'test-entry-prices) (apply #'vector (assoccess-default test-fitnesses :entry-prices '(0.0))))
                    (setf (slot-value trade 'test-exit-prices) (apply #'vector (assoccess-default test-fitnesses :exit-prices '(0.0))))
                    (setf (slot-value trade 'test-tps) (apply #'vector (assoccess-default test-fitnesses :tps '(0.0))))
                    (setf (slot-value trade 'test-sls) (apply #'vector (assoccess-default test-fitnesses :sls '(0.0))))
                    (setf (slot-value trade 'test-activations) (apply #'vector (assoccess-default test-fitnesses :activations '(0.0))))
                    (setf (slot-value trade 'test-returns) (apply #'vector (assoccess-default test-fitnesses :returns '(0.0))))

                    (update-dao trade)))
              (insert-trade owner-id instrument timeframe types train-fitnesses test-fitnesses tp sl activation rates creation-time label)))))

(def (function d) insert-trade (owner-id instrument timeframe types train-fitnesses test-fitnesses tp sl activation rates creation-time label)
  (conn (let ((patterns (get-patterns instrument timeframe types))
              (trade (make-dao 'trade
                               :owner-id owner-id
                               :label label
                               :creation-time creation-time
                               :decision (if (or (= sl 0) (= tp 0))
                                             "HOLD"
                                             (if (> tp 0)
                                                 "BUY"
                                                 "SELL"))
                               :tp tp
                               :sl sl
                               :activation activation
                               :entry-price (if (> tp 0)
                                                (hsinp.rates:->close-ask (last-elt rates))
                                                (hsinp.rates:->close-bid (last-elt rates)))
                               :entry-time (/ (read-from-string (assoccess (last-elt rates) :time)) 1000000)
                               :train-begin-time (assoccess-default train-fitnesses :begin-time -1)
                               :test-begin-time (assoccess-default test-fitnesses :begin-time -1)
                               :train-end-time (assoccess-default train-fitnesses :end-time -1)
                               :test-end-time (assoccess-default test-fitnesses :end-time -1)
                               :train-dataset-size (assoccess-default train-fitnesses :dataset-size -1)
                               :test-dataset-size (assoccess-default test-fitnesses :dataset-size -1)
                               :train-avg-revenue (assoccess-default train-fitnesses :avg-revenue 0)
                               :test-avg-revenue (assoccess-default test-fitnesses :avg-revenue 0)
                               :train-stdev-revenue (assoccess-default train-fitnesses :stdev-revenue -1)
                               :test-stdev-revenue (assoccess-default test-fitnesses :stdev-revenue -1)
                               :train-total-revenue (assoccess-default train-fitnesses :total-revenue 0)
                               :test-total-revenue (assoccess-default test-fitnesses :total-revenue 0)
                               :train-avg-return (assoccess-default train-fitnesses :avg-return 0)
                               :test-avg-return (assoccess-default test-fitnesses :avg-return 0)
                               :train-total-return (assoccess-default train-fitnesses :total-return 0)
                               :test-total-return (assoccess-default test-fitnesses :total-return 0)
                               :train-avg-max-pos (assoccess-default train-fitnesses :avg-max-pos 0)
                               :test-avg-max-pos (assoccess-default test-fitnesses :avg-max-pos 0)
                               :train-stdev-max-pos (assoccess-default train-fitnesses :stdev-max-pos -1)
                               :test-stdev-max-pos (assoccess-default test-fitnesses :stdev-max-pos -1)
                               :train-avg-max-neg (assoccess-default train-fitnesses :avg-max-neg 0)
                               :test-avg-max-neg (assoccess-default test-fitnesses :avg-max-neg 0)
                               :train-stdev-max-neg (assoccess-default train-fitnesses :stdev-max-neg -1)
                               :test-stdev-max-neg (assoccess-default test-fitnesses :stdev-max-neg -1)
                               :train-avg-tp (assoccess-default train-fitnesses :avg-tp 0)
                               :test-avg-tp (assoccess-default test-fitnesses :avg-tp 0)
                               :train-stdev-tp (assoccess-default train-fitnesses :stdev-tp -1)
                               :test-stdev-tp (assoccess-default test-fitnesses :stdev-tp -1)
                               :train-avg-sl (assoccess-default train-fitnesses :avg-sl 0)
                               :test-avg-sl (assoccess-default test-fitnesses :avg-sl 0)
                               :train-stdev-sl (assoccess-default train-fitnesses :stdev-sl -1)
                               :test-stdev-sl (assoccess-default test-fitnesses :stdev-sl -1)
                               :train-avg-activation (assoccess-default train-fitnesses :avg-activation 0)
                               :test-avg-activation (assoccess-default test-fitnesses :avg-activation 0)
                               :train-stdev-activation (assoccess-default train-fitnesses :stdev-activation -1)
                               :test-stdev-activation (assoccess-default test-fitnesses :stdev-activation -1)
                               :train-max-tp (assoccess-default train-fitnesses :max-tp 0)
                               :test-max-tp (assoccess-default test-fitnesses :max-tp 0)
                               :train-min-tp (assoccess-default train-fitnesses :min-tp 0)
                               :test-min-tp (assoccess-default test-fitnesses :min-tp 0)
                               :train-max-sl (assoccess-default train-fitnesses :max-sl 0)
                               :test-max-sl (assoccess-default test-fitnesses :max-sl 0)
                               :train-min-sl (assoccess-default train-fitnesses :min-sl 0)
                               :test-min-sl (assoccess-default test-fitnesses :min-sl 0)
                               :train-trades-won (assoccess-default train-fitnesses :trades-won -1)
                               :test-trades-won (assoccess-default test-fitnesses :trades-won -1)
                               :train-trades-lost (assoccess-default train-fitnesses :trades-lost -1)
                               :test-trades-lost (assoccess-default test-fitnesses :trades-lost -1)
                               :train-revenues (apply #'vector (assoccess-default train-fitnesses :revenues '(0.0)))
                               :test-revenues (apply #'vector (assoccess-default test-fitnesses :revenues '(0.0)))
                               :train-entry-times (apply #'vector (assoccess-default train-fitnesses :entry-times '(0.0)))
                               :test-entry-times (apply #'vector (assoccess-default test-fitnesses :entry-times '(0.0)))
                               :train-exit-times (apply #'vector (assoccess-default train-fitnesses :exit-times '(0.0)))
                               :test-exit-times (apply #'vector (assoccess-default test-fitnesses :exit-times '(0.0)))
                               :train-entry-prices (apply #'vector (assoccess-default train-fitnesses :entry-prices '(0.0)))
                               :test-entry-prices (apply #'vector (assoccess-default test-fitnesses :entry-prices '(0.0)))
                               :train-exit-prices (apply #'vector (assoccess-default train-fitnesses :exit-prices '(0.0)))
                               :test-exit-prices (apply #'vector (assoccess-default test-fitnesses :exit-prices '(0.0)))
                               :train-tps (apply #'vector (assoccess-default train-fitnesses :tps '(0.0)))
                               :test-tps (apply #'vector (assoccess-default test-fitnesses :tps '(0.0)))
                               :train-sls (apply #'vector (assoccess-default train-fitnesses :sls '(0.0)))
                               :test-sls (apply #'vector (assoccess-default test-fitnesses :sls '(0.0)))
                               :train-activations (apply #'vector (assoccess-default train-fitnesses :activations '(0.0)))
                               :test-activations (apply #'vector (assoccess-default test-fitnesses :activations '(0.0)))
                               :train-returns (apply #'vector (assoccess-default train-fitnesses :returns '(0.0)))
                               :test-returns (apply #'vector (assoccess-default test-fitnesses :returns '(0.0)))
                               )))
          (loop for pattern in patterns
                do (make-dao 'pattern-trade
                             :pattern-id (assoccess pattern :id)
                             :trade-id (slot-value trade 'id))))))

(def (function d) buy-and-hold (rates)
  (- (hsinp.rates:->close (last-elt rates))
     (hsinp.rates:->close (first rates))))

(def (function d) evaluate-trade (tp sl rates)
  "Refactorize this."
  (let ((starting-rate (if (plusp tp)
                           ;; We need to use `open` because it's when we start.
                           (hsinp.rates:->open-ask (first rates))
                           (hsinp.rates:->open-bid (first rates))))
        (revenue 0)
        (max-pos 0)
        (max-neg 0)
        (exit-time)
        ;; Needs to be (length rates) in case the trade never finishes.
        (finish-idx (length rates)))
    (when (or (= tp 0) (= sl 0))
      ;; We move to the next price.
      (setf finish-idx 1))
    ;; We use the full `rates` dataset because we're starting at open.
    ;; We need to check the starting candle's low and high.
    (unless (or (= tp 0) (= sl 0))
      (loop for rate in rates
            for idx from 1 below finish-idx ;; (length (rest rates))
            do (let ((low (if (plusp tp) ;; Used to exit a trade, so buy -> bid, sell -> ask.
                              (hsinp.rates:->low-bid rate)
                              (hsinp.rates:->low-ask rate)))
                     (high (if (plusp tp)
                               (hsinp.rates:->high-bid rate)
                               (hsinp.rates:->high-ask rate)))
                     (time (assoccess rate :time)))
                 (if (> tp 0)
                     ;; Then it's bullish.
                     (if (< (- low starting-rate) sl)
                         ;; Then we lost.
                         (progn
                           (setf revenue sl)
                           (setf finish-idx idx)
                           (setf exit-time time)
                           (return))
                         (when (>= (- high starting-rate) tp)
                           ;; Then we won.
                           (setf revenue tp)
                           (setf finish-idx idx)
                           (setf exit-time time)
                           (return)))
                     ;; Then it's bearish.
                     (if (> (- high starting-rate) sl)
                         ;; Then we lost.
                         (progn
                           (setf revenue (- sl))
                           (setf finish-idx idx)
                           (setf exit-time time)
                           (return))
                         (when (<= (- low starting-rate) tp)
                           ;; Then we won.
                           (setf revenue (- tp))
                           (setf finish-idx idx)
                           (setf exit-time time)
                           (return))))
                 ;; These need to be done after determining if we won or lost.
                 ;; Otherwise, we'd be reporting max-neg or max-pos that are greater
                 ;; than the TP or SL.
                 ;; Updating max-pos.
                 (when (> (- high starting-rate) max-pos)
                   (setf max-pos (- high starting-rate)))
                 ;; Updating max-neg.
                 (when (< (- low starting-rate) max-neg)
                   (setf max-neg (- low starting-rate))))))
    `((:revenue . ,revenue)
      (:max-pos . ,max-pos)
      (:max-neg . ,max-neg)
      (:exit-time . ,exit-time)
      (:finish-idx . ,finish-idx))))
;; (evaluate-trade 0.0015 -0.0020 (get-output-dataset *rates* 3))
;; (get-tp-sl (get-output-dataset *rates* 188))
;; (defparameter *rates* (hsinp.rates:get-rates-count :EUR_USD :M1 5000))
;; (defparameter *rates2* (hsinp.rates:get-rates-count-big :EUR_USD :M1 5000))
;; (evaluate-trade 0.00211 -0.00141 (get-output-dataset *rates* 3))

(comment
  (read-from-string (assoccess (last-elt *rates*) :time))
  (read-from-string (assoccess (first *rates*) :time))
  (local-time:unix-to-timestamp 1639532220)
  (local-time:unix-to-timestamp 1630488600)
  (local-time:unix-to-timestamp 1639396130)

  (let ((creation-time 1639396130000000))
    (loop for rate in *rates2*
          for idx from 0 
          when (>= (read-from-string (assoccess rate :time)) creation-time)
            do (return idx)))

  (loop for rate in (get-output-dataset *rates* 387)
        do (format t "~$2~%" (* 10000 (- (assoccess rate :close-bid)
                                          (assoccess (first (get-output-dataset *rates* 387)) :close-bid)))))

  (evaluate-trade 0.00208 -0.00088 (get-output-dataset *rates* 2795))
  (evaluate-trade 0.00208 -0.00088 (get-output-dataset *rates2* 4868))

  (- (assoccess (first (get-output-dataset *rates* 387)) :close-bid)
      (assoccess (last-elt (get-output-dataset *rates* 387)) :close-bid))

  (position "1639396130000000" *rates* :test #'string= :key (lambda (rate) (assoccess rate :time)))
  1636109460000000
  1639396130000000
  1639396130000)

(def (function d) -test-conditions (instrument tp sl test-fitnesses &key (hybridp nil))
  (or *ignore-test-conditions-p*
      (and
       (/= tp 0)
       ;; (if (not (eq instrument :USD_CNH)) (< (assoccess prediction :tp) 100) t)
       (> (abs tp) (abs sl))
       (/= sl 0)
       (< (* tp sl) 0)
       (if hybridp
           t
           (and
            (> (abs (/ tp sl))
               hscom.hsage:*agents-min-rr-signal*)
            (> (abs (to-pips instrument sl)) *min-pips-sl*)
            (/= (assoccess test-fitnesses :trades-won) 0)))
       ;; (< (to-pips instrument (abs sl)) 20)
       )))

;;
;; (get-hybrid :EUR_USD :M15 "")

(def (function d) insert-metrics (metrics)
  (conn (make-dao 'metrics
                  :begin-time (assoccess-default metrics :begin-time -1)
                  :end-time (assoccess-default metrics :end-time -1)
                  :dataset-size (assoccess-default metrics :dataset-size -1)
                  :avg-revenue (assoccess-default metrics :avg-revenue 0)
                  :stdev-revenue (assoccess-default metrics :stdev-revenue -1)
                  :total-revenue (assoccess-default metrics :total-revenue 0)
                  :avg-return (assoccess-default metrics :avg-return 0)
                  :total-return (assoccess-default metrics :total-return 0)
                  :avg-max-pos (assoccess-default metrics :avg-max-pos 0)
                  :stdev-max-pos (assoccess-default metrics :stdev-max-pos -1)
                  :avg-max-neg (assoccess-default metrics :avg-max-neg 0)
                  :stdev-max-neg (assoccess-default metrics :stdev-max-neg -1)
                  :avg-tp (assoccess-default metrics :avg-tp 0)
                  :stdev-tp (assoccess-default metrics :stdev-tp -1)
                  :avg-sl (assoccess-default metrics :avg-sl 0)
                  :stdev-sl (assoccess-default metrics :stdev-sl -1)
                  :avg-activation (assoccess-default metrics :avg-activation 0)
                  :stdev-activation (assoccess-default metrics :stdev-activation -1)
                  :max-tp (assoccess-default metrics :max-tp 0)
                  :min-tp (assoccess-default metrics :min-tp 0)
                  :max-sl (assoccess-default metrics :max-sl 0)
                  :min-sl (assoccess-default metrics :min-sl 0)
                  :trades-won (assoccess-default metrics :trades-won -1)
                  :trades-lost (assoccess-default metrics :trades-lost -1)
                  :revenues (apply #'vector (assoccess metrics :revenues))
                  :entry-times (apply #'vector (assoccess metrics :entry-times))
                  :exit-times (apply #'vector (assoccess metrics :exit-times))
                  :entry-prices (apply #'vector (assoccess metrics :entry-prices))
                  :exit-prices (apply #'vector (assoccess metrics :exit-prices))
                  :tps (apply #'vector (assoccess metrics :tps))
                  :sls (apply #'vector (assoccess metrics :sls))
                  :activations (apply #'vector (assoccess metrics :activations))
                  :returns (apply #'vector (assoccess metrics :returns)))))

(def (function d) insert-evaluation (metrics-id owner-id label iterations)
  (conn (make-dao 'evaluation
                  :metrics-id metrics-id
                  :owner-id owner-id
                  :label label
                  :iterations iterations)))

(def (function d) get-metricses (owner-id label)
  (conn (query (:select '* :from 'metricses
                :where (:= 'id
                        (:select 'metrics-id :from 'evaluations
                         :where (:and (:= 'owner-id owner-id)
                                 (:= 'label label)))))
               (:dao metrics))))
;; (avg-revenue (get-metricses "66D36D97-EE28-4C3B-8239-9D096542DE2B" "train"))

(def (function d) get-hybrid (instrument timeframe name &optional (ret-type :dao))
  (let ((q `(:select '* :from 'hybrids :where (:and (:= 'instrument ,(stringify instrument))
                                               (:= 'timeframe ,(stringify timeframe))
                                               (:= 'name ,name)))))
    (first (conn (cond ((eq ret-type :alist) (query (sql-compile q) :alist))
                       ((eq ret-type :alists) (query (sql-compile q) :alists))
                       (t (query (sql-compile q) (:dao hybrid))))))))
;; (best-individual (get-hybrid :AUD_USD :M15 "hybrid.rsi-stoch-macd"))

(def (function d) get-hybrid-by-id (id)
  (conn (get-dao 'hybrid id)))
;; (name (get-hybrid-by-id "66D36D97-EE28-4C3B-8239-9D096542DE2B"))

(def (function d) get-hybrid-iterations (instrument timeframe name)
  (let ((hybrid (get-hybrid instrument timeframe name)))
    (assoccess (conn (query (:order-by (:select 'iterations :from 'evaluations
                                        :where (:and (:= 'owner-id (id hybrid))))
                             (:desc 'iterations))
                            :alist))
               :iterations)))
;; (get-hybrid-iterations :EUR_USD :M15 "human.rsi-stoch-macd")

(def (function d) insert-hybrid (instrument timeframe name label iterations population best-individual metrics)
  (conn (let* ((existing-hybrid (get-hybrid instrument timeframe name))
               (hybrid (if existing-hybrid
                           (progn
                             (setf (population existing-hybrid)
                                   (if (vectorp population) population (apply #'vector population)))
                             (setf (best-individual existing-hybrid)
                                   (if (vectorp best-individual) best-individual (apply #'vector best-individual)))
                             (update-dao existing-hybrid))
                           (make-dao 'hybrid
                                     :instrument instrument
                                     :timeframe timeframe
                                     :iterations iterations
                                     :name name
                                     :population (if (vectorp population) population (apply #'vector population))
                                     :best-individual (if (vectorp best-individual) best-individual (apply #'vector best-individual)))))
               (metrics-dao (insert-metrics metrics)))
          (insert-evaluation (id metrics-dao)
                             (id hybrid)
                             label
                             (if existing-hybrid
                                 (if-let ((iter (get-hybrid-iterations instrument timeframe name)))
                                   (+ iterations iter) 0)
                                 iterations))
          hybrid)))

(def (function d) get-human-name (human-strategy)
  (format nil "human.~a" (assoccess human-strategy :name)))
(def (function d) get-hybrid-name (human-strategy)
  (format nil "hybrid.~a" (assoccess human-strategy :name)))

(def (function d) gen-search-space (parameters ranges)
  (mapcar (lambda (parameter range)
            (cons parameter range))
          parameters
          ranges))

;; (gen-search-space (first (hsper:get-human-strategies)))
;; (gen-search-space (assoccess (first (hsper:get-human-strategies)) :parameters)
;; 		  (whole-reals-to-integers (best-individual (get-hybrid :EUR_USD :M15 "human.rsi-stoch-macd"))))

(def (function d) gen-population-from-genomes (search-space genomes genome-size parameters)
  (loop for i from 0 below (length genomes) by genome-size
        collect (let* ((values (whole-reals-to-integers (subseq genomes i (+ i genome-size))))
                       (gen-values (flatten (mapcar (lambda (gen value)
                                                      (list (make-keyword gen) value))
                                                    parameters
                                                    values))))
                  (apply #'make-being search-space gen-values))))
;; (gen-population-from-genomes )

(def (function d) optimize-human-strategy (instrument timeframe types
                                                      input-dataset train-size
                                                      human-strategy
                                                      &key (maximize nil) (population-size 100)
                                                      (max-iterations 100) (mutation-rate 0.1)
                                                      (test-size 1000) (fitness-metric :total-return))
  (bind ((train-dataset (subseq input-dataset 0 train-size))
         (test-dataset (subseq input-dataset train-size))
         (train-idxs (when *unique-point-p* (get-unique-dataset train-dataset *unique-count* *lookahead* *lookbehind*)))
         (test-idxs (when *unique-point-p* (get-unique-dataset test-dataset *unique-count* *lookahead* *lookbehind*)))
         (search-space (gen-search-space
                        (assoccess human-strategy :parameters)
                        (assoccess human-strategy :args-ranges)))
         (get-fit-fn (lambda (fit)
                       (assoccess (get-fit fit) fitness-metric)))
         (existing-hybrid (get-hybrid instrument
                                      timeframe
                                      (get-hybrid-name human-strategy))))
    (eval `(genetic-algorithm:run-ga (,search-space
                                 :maximize ,maximize
                                 :population-size ,population-size
                                 :max-iterations ,max-iterations
                                 :mutation-rate ,mutation-rate
                                 :first-generation (when ,(when existing-hybrid t)
                                                     (let* ((hybrid (get-hybrid ,instrument
                                                                                ,timeframe
                                                                                ,(get-hybrid-name human-strategy)))
                                                            (genomes (population hybrid))
                                                            (best (best-individual hybrid)))
                                                       (gen-population-from-genomes
                                                        ',search-space
                                                        genomes
                                                        (length best)
                                                        ',(assoccess human-strategy :parameters))
                                                       ))
                                 :get-fit-fn (lambda (fit)
                                               (assoccess (genetic-algorithm:get-fit fit) ,fitness-metric))
                                 ;; :after-each-iteration (describe (first genetic-algorithm:-population-))
                                 :finally
                                 (let* ((best-individual (select-the-best -population-
                                                                          :get-fit-fn ,get-fit-fn))
                                        (best-genome (get-gens best-individual))
                                        (best-fitness (get-fit best-individual))
                                        (population-genomes (flatten (mapcar (lambda (ind)
                                                                               (get-gens ind))
                                                                             -population-)))
                                        (test-metrics (-evaluate-model
                                                       :instrument ,instrument
                                                       :timeframe ,timeframe
                                                       :types ',types
                                                       :rates ',test-dataset
                                                       :model (lambda (input-dataset)
                                                                (apply #'funcall ,(assoccess human-strategy :fn)
                                                                       input-dataset
                                                                       best-genome))
                                                       :idx ,(assoccess human-strategy :lookbehind-count)
                                                       :idxs ',test-idxs
                                                       :test-size ,test-size)))
                                   (insert-hybrid ,(stringify instrument)
                                                  ,(stringify timeframe)
                                                  ,(get-hybrid-name human-strategy)
                                                  "train"
                                                  ,max-iterations
                                                  population-genomes
                                                  best-genome
                                                  best-fitness)
                                   (insert-hybrid ,(stringify instrument)
                                                  ,(stringify timeframe)
                                                  ,(get-hybrid-name human-strategy)
                                                  "test"
                                                  ,max-iterations
                                                  population-genomes
                                                  best-genome
                                                  test-metrics)))
             (-evaluate-model
              :instrument ,instrument
              :timeframe ,timeframe
              :types ',types
              :rates ',train-dataset
              :model (lambda (input-dataset)
                       (funcall ,(assoccess human-strategy :fn)
                                input-dataset
                                ,@(assoccess human-strategy :parameters)))
              :idx ,(assoccess human-strategy :lookbehind-count)
              :idxs ',train-idxs
              :test-size ,train-size)))))

(comment
  (let ((hybrids (conn (query (:select 'hybrids.instrument 'hybrids.timeframe 'metricses.trades-won 'metricses.trades-lost
                                :from 'hybrids
                                :join 'evaluations
                                :on (:= 'evaluations.owner_id 'hybrids.id)
                                :join 'metricses
                                :on (:= 'evaluations.metrics_id 'metricses.id)
                                ;; :where (:= 'evaluations.label "test")
                                ))))
        (results (make-hash-table :test 'equal)))
    (print hybrids)
    (loop for hybrid in hybrids do (if (gethash (car hybrid) results)
                                       (incf (gethash (car hybrid) results))
                                       (setf (gethash (car hybrid) results) 1)))
    results)
  ;; trades
  (let ((hybrids (conn (query (:select 'trades.*
                                'patterns.instrument
                                'patterns.timeframe
                                :from 'trades
                                :join 'patterns-trades
                                :on (:= 'trades.id 'patterns-trades.trade-id)
                                :join 'patterns
                                :on (:= 'patterns-trades.pattern-id 'patterns.id)
                                :where (:and (:like 'trades.label "hybrid%")
                                             ;; (:not (:= 'trades.decision "HOLD"))
                                             ))
                              :alists))
                 )
        (results (make-hash-table :test 'equal)))
    (loop for hybrid in hybrids do (if (gethash (assoccess hybrid :instrument) results)
                                       (incf (gethash (assoccess hybrid :instrument) results))
                                       (setf (gethash (assoccess hybrid :instrument) results) 1)))
    results)
  )

(def (function d) sigmoid (x a b k)
  (/ k (+ 1 (exp (+ a (* b x))))))
;; (sigmoid -3 2 -30 1)


(comment
  ;; (sb-int:with-float-traps-masked)
  (sb-int:set-floating-point-modes :traps nil)
  (sb-int:set-floating-point-modes :fast-mode t)
  (let ((x 2)
        (a 0)
        (b 1000)
        (k 1))
    (* x
       (sigmoid x a (* -1 b) k)
       (sigmoid x (* -1 b) b k)))
  )

;; (genetic-algorithm:get-gens *coco*)
;; (genetic-algorithm:get-name (first (genetic-algorithm:get-genome *coco*)))

(comment
 ;; (defparameter *rates* (hsinp.rates:fracdiff (hsinp.rates:get-rates-count-big :AUD_USD :H1 60000)))
 (defparameter *coco*
   (time (optimize-human-strategy :EUR_USD :M15 '((:single))
                                  (get-input-dataset *rates* 6000)
                                  ;; *rates*
                                  1000
                                  (first (hsper:get-human-strategies))
                                  :maximize t
                                  :population-size 2
                                  :max-iterations 1
                                  :mutation-rate 0.1
                                  :test-size 5000
                                  :fitness-metric :total-revenue))
   )
 )
;; pop - 2, iter - 1
;; rates 1000/1000 - 31.755
;; rates 2000/2000 - 90.626
;; rates 3000/3000 - 240.154
;; rates 1000/3000 - 77.727
;; rates 3000/1000 - 26.196
;; rates 5000/1000 - 20.617
;; rates 1000/5000 - 192.314

(comment
 (-evaluate-model
  :instrument :EUR_USD
  :timeframe :M15
  :types '((:single))
  :rates (get-input-dataset *rates* 1500)
  :model (lambda (input-dataset)
           (apply #'=>STRATEGY-RSI-STOCH-MACD
                  input-dataset
                  ;; (assoccess (first (hsper:get-human-strategies)) :args-default)
                  '(0 17 17 19 9 28 38 13 18 5)
                  ))
  :idx (assoccess (first (hsper:get-human-strategies)) :lookbehind-count)
  :test-size 1000))

(def (function d) test-hybrid-strategy (instrument timeframe types hybrid-id testing-dataset model lookbehind-count &key (test-size 50) (label "") (testingp t))
  (multiple-value-bind (tp sl activation)
      (funcall model testing-dataset)
    (bind ((idxs (when *unique-point-p* (get-unique-dataset testing-dataset *unique-count* *lookahead* *lookbehind*)))
           (test-fitnesses (-evaluate-model
                            :instrument instrument
                            :timeframe timeframe
                            :types types
                            :rates testing-dataset
                            :model model
                            :idx lookbehind-count
                            :idxs idxs
                            :test-size test-size)))
      (when test-fitnesses
        (push-to-log "Tested hybrid strategy successfully."))
      (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
      ;; We're going to allow any trade to pass (not using -TEST-CONDITIONS).
      (when (-test-conditions instrument tp sl test-fitnesses :hybridp t)
        (insert-trade hybrid-id instrument timeframe types test-fitnesses test-fitnesses tp sl activation testing-dataset (local-time:timestamp-to-unix (local-time:now)) label))
      (push-to-log "Trade created successfully."))))

(def (function d) test-human-strategy (instrument timeframe types testing-dataset model lookbehind-count &key (test-size 50) (label "") (testingp t))
  (multiple-value-bind (tp sl activation)
      (funcall model testing-dataset)
    (bind ((idxs (when *unique-point-p* (get-unique-dataset testing-dataset *unique-count* *lookahead* *lookbehind*)))
           (test-fitnesses (-evaluate-model
                            :instrument instrument
                            :timeframe timeframe
                            :types types
                            :rates testing-dataset
                            :model model
                            :idx lookbehind-count
                            :idxs idxs
                            :test-size test-size)))
      (when test-fitnesses
        (push-to-log "Tested human strategy successfully."))
      (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
      ;; We're going to allow any trade to pass (not using -TEST-CONDITIONS).
      (insert-trade-human "" instrument timeframe types test-fitnesses test-fitnesses tp sl activation testing-dataset (local-time:timestamp-to-unix (local-time:now)) label :metric-or-signal (if testingp :metric :signal))
      (push-to-log "Trade created successfully."))))
;; (test-human-strategy :EUR_USD :M15 '((:bullish)) *rates* (assoccess (first (hsper:get-human-strategies)) :model) (assoccess (first (hsper:get-human-strategies)) :lookbehind-count) :test-size 500 :label "human")

(comment
  (defparameter *rates* (hsinp.rates:get-rates-random-count-big :EUR_USD :M15 10000))
  (test-human-strategy :EUR_USD :M15 '((:rsi :macd :stoch)) (subseq *rates* 0 ) (assoccess (first (hsper:get-human-strategies)) :model) (assoccess (first (hsper:get-human-strategies)) :lookbehind-count) :test-size 200 :label "human" :testingp t)
  (slot-value (first (conn (query (:select '* :from 'trades :where (:= 'label "human")) (:dao trade)))) 'tp)
  ;; (slot-value (first (conn (query (:select '* :from 'trades :where (:= 'label "human")) (:dao trade)))) 'test-begin-time)
  )

;; (funcall (assoccess (first (hsper:get-human-strategies)) :model) *rates*)

(comment
  (length *rates*)
  (loop for i from 1500 below 3000
        do (progn
             (format t ".~%")
             (print (test-human-strategy :EUR_USD :M15 '((:single))
                                         (get-input-dataset *rates* i)
                                         (lambda (input-dataset)
                                           (funcall (assoccess (first (hsper:get-human-strategies)) :model)
                                                    input-dataset
                                                    (assoccess (first (hsper:get-human-strategies)) :args-default)))
                                         (assoccess (first (hsper:get-human-strategies)) :lookbehind-count)
                                         :test-size 500
                                         :label (assoccess (first (hsper:get-human-strategies)) :label)))))
  )

(def (function d) test-agents (instrument timeframe types testing-dataset idxs &key (test-size 50) (label ""))
  (multiple-value-bind (tp sl activation agent-ids)
      ;; This one gets the final TP and SL.
      (eval-agents instrument timeframe types testing-dataset)
    (let* (;; (train-fitnesses (evaluate-agents instrument timeframe types training-dataset))
           (test-fitnesses (evaluate-agents instrument timeframe types testing-dataset idxs :test-size test-size)))
      ($log $info (format nil "Tested ~a from ~a data points for ~a." test-size (length testing-dataset) instrument))
      ;; (when train-fitnesses
      ;; 	(push-to-log "Training process successful."))
      (when test-fitnesses
        (push-to-log "Testing process successful."))
      (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
      (when (-test-conditions instrument tp sl test-fitnesses)
        (push-to-log (format nil "Trying to create trade. Agents IDs: ~a" agent-ids))
        (insert-trade (first agent-ids) instrument timeframe (first (get-agent-ids-patterns agent-ids)) test-fitnesses test-fitnesses tp sl activation testing-dataset (local-time:timestamp-to-unix (local-time:now)) label)
        (push-to-log "Trade created successfully.")))))

(def (function d) get-max-lookbehind (instrument timeframe types)
  (bind ((types (flatten types)))
    (loop for agent in (get-agents-some instrument timeframe types) maximize (slot-value agent 'lookbehind-count))))

(def (macro d) -evaluate-model-loop (body)
    "Used by -EVALUATE-MODEL."
  (if *unique-point-p*
      `(loop for idx in idxs
             do (progn
                  ,body))
      `(loop while (< idx (length rates))
          do ,body)))

(def (function d) -evaluate-model (&key instrument timeframe types rates model idx idxs test-size)
  "Used by EVALUATE-AGENT and EVALUATE-AGENTS."
  (bind ((idx-p (when idx t))
         (max-lookbehind (if idx idx (get-max-lookbehind instrument timeframe types)))
         (idx (if idx idx max-lookbehind))
         (rates (last rates (+ (if test-size test-size (length rates)) max-lookbehind 1)))
         (revenues)
         (max-poses)
         (max-negses)
         (trades-won 0)
         (trades-lost 0)
         (entry-prices)
         (exit-prices)
         (tps)
         (sls)
         (activations)
         (entry-times)
         (exit-times)
         (num-datapoints 0)
         (num-datapoints-traded 0))
    (when (>= (length rates) max-lookbehind)
      (-evaluate-model-loop
       (let* ((input-dataset (hsinp.rates:get-input-dataset rates idx))
              (output-dataset (hsinp.rates:get-output-dataset rates idx)))
         (multiple-value-bind (tp sl activation)
             (funcall model input-dataset)
           (if (< activation hscom.hsage:*evaluate-agents-activation-threshold*)
               (progn
                 (incf num-datapoints)
                 (incf idx))
               (let* ((trade (evaluate-trade tp sl output-dataset))
                      (revenue (assoccess trade :revenue))
                      (max-pos (assoccess trade :max-pos))
                      (max-neg (assoccess trade :max-neg))
                      (exit-time (assoccess trade :exit-time))
                      (finish-idx (assoccess trade :finish-idx)))
                 (if (or (= revenue 0)
                         (> (abs sl) (abs tp))
                         (> (* tp sl) 0)
                         (< (abs (/ tp sl))
                            hscom.hsage:*agents-min-rr-trading*)
                         (= tp 0)
                         (= sl 0)
                         (unless idx-p
                           (< (abs (to-pips instrument sl)) hscom.hsage:*min-pips-sl*))
                         (> (abs (to-pips instrument sl)) hscom.hsage:*max-pips-sl*)
                         )
                     (incf num-datapoints)
                     (progn
                       (incf num-datapoints-traded)
                       (incf num-datapoints)
                       (if (> revenue 0)
                           (incf trades-won)
                           (incf trades-lost))
                       (push tp tps)
                       (push sl sls)
                       (push activation activations)
                       (push (read-from-string (assoccess (nth idx rates) :time)) entry-times)
                       (push (read-from-string exit-time) exit-times)
                       (push (if (plusp tp)
                                 (hsinp.rates:->close-ask (nth idx rates))
                                 (hsinp.rates:->close-bid (nth idx rates)))
                             entry-prices)
                       (push (if (plusp tp)
                                 (hsinp.rates:->close-bid (nth finish-idx output-dataset))
                                 (hsinp.rates:->close-ask (nth finish-idx output-dataset)))
                             exit-prices)
                       (push max-pos max-poses)
                       (push max-neg max-negses)
                       (push revenue revenues)))
                 (if (and hscom.hsage:*trade-every-dp-p*
                          *unique-point-p*)
                     (incf idx)
                     (incf idx finish-idx))
                 ))
           ))))
    ;; ACTPROOF
    ;; (unless agent
    ;;   (setf *activations* activations)
    ;;   (setf *revenues* revenues)
    ;;   (coco))
    ($log $info (format nil "Traded ~a out of ~a datapoints." num-datapoints-traded num-datapoints))
    (let* ((returns (loop for revenue in revenues
                          for tp in tps
                          for sl in sls
                          collect (if (or (= tp 0)
                                          (= sl 0))
                                      0
                                      (if (> revenue 0)
                                          (* (/ tp sl) -1) ;; To always get positive number.
                                          -1))))
           (total-return (reduce #'+ returns))
           (abs-tps (mapcar #'abs tps))
           (abs-sls (mapcar #'abs sls)))
      `((:begin-time . ,(read-from-string (assoccess (first rates) :time)))
        (:end-time . ,(read-from-string (assoccess (last-elt rates) :time)))
        (:dataset-size . ,(length rates))
        (:avg-revenue . ,(if (> (length revenues) 0) (mean revenues) 0))
        (:stdev-revenue . ,(if (> (length revenues) 0) (standard-deviation revenues) 0))
        (:total-revenue . ,(if (> (length revenues) 0) (reduce #'+ revenues) 0))
        (:avg-max-pos . ,(if (> (length max-poses) 0) (mean max-poses) 0))
        (:stdev-max-pos . ,(if (> (length max-poses) 0) (standard-deviation max-poses) 0))
        (:avg-max-neg . ,(if (> (length max-negses) 0) (mean max-negses) 0))
        (:stdev-max-neg . ,(if (> (length max-negses) 0) (standard-deviation max-negses) 0))
        (:avg-tp . ,(if (> (length tps) 0) (mean abs-tps) 0))
        (:stdev-tp . ,(if (> (length tps) 0) (standard-deviation abs-tps) 0))
        (:avg-sl . ,(if (> (length sls) 0) (mean abs-sls) 0))
        (:stdev-sl . ,(if (> (length sls) 0) (standard-deviation abs-sls) 0))
        (:avg-activation . ,(if (> (length activations) 0) (mean activations) 0))
        (:stdev-activation . ,(if (> (length activations) 0) (standard-deviation activations) 0))
        (:avg-return . ,(if (> (length tps) 0) (/ total-return (length tps)) 0))
        (:total-return . ,total-return)
        (:max-tp . ,(if (> (length max-negses) 0) (apply #'max abs-tps) 0))
        (:min-tp . ,(if (> (length max-negses) 0) (apply #'min abs-tps) 0))
        (:max-sl . ,(if (> (length max-negses) 0) (apply #'max abs-sls) 0))
        (:min-sl . ,(if (> (length max-negses) 0) (apply #'min abs-sls) 0))
        (:trades-won . ,trades-won)
        (:trades-lost . ,trades-lost)
        (:revenues . ,(reverse revenues))
        (:entry-times . ,(reverse entry-times))
        (:exit-times . ,(reverse exit-times))
        (:entry-prices . ,(reverse entry-prices))
        (:exit-prices . ,(reverse exit-prices))
        (:tps . ,(reverse tps))
        (:sls . ,(reverse sls))
        (:activations . ,(reverse activations))
        (:returns . ,(reverse returns))))))
;; (evaluate-agents :EUR_USD hscom.hsage:*train-tf* '(:BULLISH) *rates*)

(def (function d) evaluate-agent (instrument timeframe agent rates idxs &key test-size (return-fitnesses-p nil))
  (let ((fitnesses (-evaluate-model :instrument instrument
                                    :timeframe timeframe
                                    ;; :agent agent
                                    :model (lambda (input-dataset)
                                             (eval-agent agent input-dataset))
                                    :rates rates
                                    :idx (slot-value agent 'lookbehind-count)
                                    :idxs idxs
                                    :test-size test-size)))
    (loop for fitness in fitnesses
          ;; TODO: We should be returning symbols (not kws) from -evaluate-model.
          ;;; and then remove this format + read-from-string.
          do (setf (slot-value agent (read-from-string (format nil "hermes-agents.trading::~a" (car fitness))))
                   (if (listp (cdr fitness)) (apply #'vector (cdr fitness)) (cdr fitness))))
    (if return-fitnesses-p
        fitnesses
        agent)))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(def (function d) evaluate-agents (instrument timeframe types rates idxs &key (test-size 50))
  (-evaluate-model :instrument instrument
                   :timeframe timeframe
                   :types types
                   :rates rates
                   :model (lambda (input-dataset)
                            (eval-agents instrument timeframe types input-dataset))
                   :idxs idxs
                   :test-size test-size))
;; (time (evaluate-agents :EUR_USD hscom.hsage:*train-tf* '(:BULLISH) (subseq *rates* 0 200)))

(def (function d) insert-pattern (instrument timeframe type)
  (conn (make-dao 'pattern :type (format nil "~a" type)
                  :instrument (format nil "~a" instrument)
                  :timeframe (format nil "~a" timeframe))))

(def (function d) wipe-agents ()
  (when hscom.hsage:*wipe-agents-p*
    ($log $trace :-> :wipe-agents)
    (conn (query (:delete-from 'agents :where (:= 1 1)))
          (query (:delete-from 'agents-patterns :where (:= 1 1))))
    ($log $trace :<- :wipe-agents)))
;; (wipe-agents)

(def (function d) get-patterns (instrument timeframe types)
  (let* ((types (flatten types)))
    (conn (query (:select '* :from 'patterns :where (:and (:= 'instrument (format nil "~a" instrument))
                                                     (:= 'timeframe (format nil "~a" timeframe))
                                                     (:in 'type (:set (loop for type in types collect (format nil "~a" type))))))
                 :alists))))
;; (get-patterns :EUR_JPY hscom.hsage:*train-tf* '(:BULLISH (:BEARISH) :STAGNATED))

(def (function d) get-agent-ids-from-patterns (instrument timeframe types)
  (let* ((types (flatten types))
         (patterns (get-patterns instrument timeframe types))
         )
    ;; (loop for agent in (get-agents-some instrument timeframe types)
    ;; 	  collect (slot-value agent 'id))
    (conn (query (:select 'agent-id :from 'agents-patterns :where (:in 'pattern-id (:set (loop for pattern in patterns collect (assoccess pattern :id))))) :column))))
;; (get-agent-ids-from-patterns :AUD_USD hscom.hsage:*train-tf* '(:bullish))
;; (get-agent-ids-from-patterns :AUD_USD hscom.hsage:*train-tf* '((:bullish) (:stagnated)))

(def (function d) get-agents-from-cache (instrument timeframe types)
  (gethash (list instrument timeframe types) *agents-cache*))

;; (get-agents-some :EUR_JPY hscom.hsage:*train-tf* '(:BULLISH))
;; (get-agents-from-cache :EUR_JPY hscom.hsage:*train-tf* '(:BULLISH))

(def (function d) sync-agents (instrument timeframe types)
  ;; Get agents from database (A1)
  ;; Get agents from cache (A2)
  ;; Update or add agents from A1 using A2
  ;; Delete agents found in A1 but not in A2
  ($log $trace :-> :sync-agents)
  (let ((A1 (get-agents-some instrument timeframe types))
        (A2 (get-agents-from-cache instrument timeframe types)))
    ($log $debug "Database agents:" (length A1))
    ($log $debug "Cache agents:" (length A2))
    (conn
     ;; First we update existing agents on database and insert the new ones.
     ;; If the algorithm crashes, we at least keep some of the agents updated and new ones.
     (loop for agent in A2
           do (if (get-dao 'agent (slot-value agent 'id))
                  (update-dao agent)
                  (insert-agent agent instrument timeframe types)))
     ;; Now we delete the agents that are in A1 (database) but not in A2 (cache).
     (let ((ids (mapcar (lambda (agent) (slot-value agent 'id)) A2)))
       (loop for agent in A1
             do (let* ((id (slot-value agent 'id))
                       (foundp (find id ids :test #'string=)))
                  (unless foundp
                    (delete-dao agent)))))))
  ($log $trace :<- :sync-agents))
;; (time (sync-agents :AUD_USD hscom.hsage:*train-tf* '(:BULLISH)))

(def (function d) limit-seq (seq limit offset)
  (let ((lseq (length seq)))
    (if (plusp limit)
        (if (>= offset lseq)
            nil
            (if (> (+ offset limit) lseq)
                (subseq seq offset)
                (subseq seq offset (+ offset limit))))
        seq)))

(def (function d) agent-correct-perception-fns (agent)
  "TODO: Add some error handling, as we're not returning anything."
  (let ((perception-fns (slot-value agent 'perception-fns)))
    (loop for idx from 0 below (length perception-fns)
          do (multiple-value-bind (i r)
                 (round (aref perception-fns idx))
               (when (= r 0)
                 (setf (aref perception-fns idx) i)))))
  (values))

(def (function d) retire-agents-from-db (instrument timeframe)
  ($log $trace :-> :retire-agents-from-db)
  (conn
   (bind ((instrument (format nil "~a" instrument))
          (timeframe (format nil "~a" timeframe))
          (agents-count (get-agents-count instrument timeframe '((:SINGLE))))
          (to-retire-count (- agents-count (- *max-agents-count* *kill-agents-count*))))
     (when (> agents-count *max-agents-count*)
       ($log $info (format nil "Retiring ~a agents out of ~a in ~a ~a"
                           to-retire-count
                           agents-count
                           instrument
                           timeframe
                           ))
       (query
        (:update 'agents
          :set 'retired t
          :where (:in 'id
                      (:limit (:order-by (:select 'agents.id
                                           :from 'agents
                                           :join 'agents-patterns
                                           :on (:= 'agent-id 'agents.id)
                                           :join 'patterns
                                           :on (:= 'agents-patterns.pattern-id 'patterns.id)
                                           :where (:and (:= 'instrument instrument)
                                                        (:= 'timeframe timeframe)
                                                        (:= 'agents.retired nil)))
                                         (:asc 'avg-return))
                              to-retire-count
                              )))))))
  ($log $trace :<- :retire-agents-from-db))
;; (retire-agents-from-db :EUR_GBP :M15)
;; (get-agents-count :EUR_GBP :M15 '())
;; (retire-agents-from-db :EUR_GBP :M15)

(let ((sync-table (make-hash-table :test 'equal :synchronized t)))
  (def (function d) cache-agents-from-db (instrument timeframe &optional (safe-cache-p nil))
    "Caches agents from `instrument` and `timeframe` from the database to *AGENTS-CACHE*."
    ($log $trace :-> :cache-agents-from-db)
    (when safe-cache-p
      ($log $info "Adding cache safety for" instrument timeframe)
      (setf (gethash (list instrument timeframe '(:single)) sync-table) t)
      ($log $info "Caching agents for" instrument timeframe)
      (get-agents-some instrument timeframe '(:single) -1 0))
    (unless (gethash (list instrument timeframe '(:single)) sync-table)
      ($log $info "Caching agents for" instrument timeframe)
      (get-agents-some instrument timeframe '(:single) -1 0))
    ($log $trace :<- :cache-agents-from-db))
  ;; (cache-agents-from-db)

  (def (function d) uncache-agents-from-db (instrument timeframe &optional (safe-cache-p nil))
    "Restarts the agents cache for `instrument` and `timeframe`."
    ($log $trace :-> :uncache-agents-from-db)
    (when safe-cache-p
      ($log $info "Removing cache safety for" instrument timeframe)
      (setf (gethash (list instrument timeframe '(:single)) sync-table) nil))

    (unless (gethash (list instrument timeframe '(:single)) sync-table)
      ($log $info "Releasing cache for" instrument timeframe)
      (setf (gethash (list instrument timeframe '(:single)) *agents-cache*) nil))

    (hsage.utils:refresh-memory)
    ($log $trace :<- :uncache-agents-from-db)))

(def (function d) get-agents-some (instrument timeframe types &optional (limit -1) (offset 0))
  (let (result)
    (loop for type in (flatten types)
          do (let ((type (list type)))
               (if-let ((agents (gethash (list instrument timeframe type) *agents-cache*)))
                 (loop for agent in (limit-seq agents limit offset)
                       do (push agent result))
                 (let* ((agent-ids (get-agent-ids-from-patterns instrument timeframe type))
                        (agents (conn (query (:select '* :from 'agents :where (:and (:in 'id (:set agent-ids))
                                                                                    (:= 'retired nil)))
                                             (:dao agent)))))
                   (when agents
                     (map nil #'agent-correct-perception-fns agents)
                     (setf (gethash (list instrument timeframe type) *agents-cache*) agents)
                     (loop for agent in (limit-seq agents limit offset)
                           do (push agent result)))))))
    result))
;; (time (get-agents-some :AUD_USD hscom.hsage:*train-tf* '(:single) -1 0))
;; (time (get-agents-some :EUR_USD hscom.hsage:*train-tf* '(:single :stagnated :bearish) 3 10))
;; (agents-to-alists (get-agents-some :AUD_USD hscom.hsage:*train-tf* '(:SINGLE)))

(def (function d) add-agent (agent instrument timeframe types)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe types) *agents-cache*)
        (append (gethash (list instrument timeframe types) *agents-cache*)
                (list agent))))

(def (function d) remove-agent (agent instrument timeframe types)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe types) *agents-cache*)
        (remove agent
                (gethash (list instrument timeframe types) *agents-cache*)
                :test (lambda (elt1 elt2)
                        (string= (slot-value elt1 'id)
                                 (slot-value elt2 'id))))))

(def (function d) insert-agent (agent instrument timeframe types)
  "Works with database."
  (let ((agent-id (slot-value agent 'id))
        (patterns (get-patterns instrument timeframe types)))
    (conn
     (unless (get-dao 'agent agent-id)
       (insert-dao agent)
       (loop for pattern in patterns
             do (make-dao 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id)))))
    agent))

(def (function d) delete-agent (agent instrument timeframe types)
  "Works with database."
  (let ((agent-id (slot-value agent 'id))
        (patterns (get-patterns instrument timeframe types)))
    (conn
     ;; First we remove agent-pattern.
     (loop for pattern in patterns
           do (delete-dao (make-instance 'agent-pattern :agent-id agent-id :pattern-id (assoccess pattern :id))))
     ;; Then we delete the agent only if there are no agent-patterns related to this agent.
     (unless (query (:select 'agent-id :from 'agents-patterns :where (:= 'agent-id agent-id)))
       (delete-dao agent)))))

(def (function d) get-agent-ids-patterns (agent-ids)
  "AGENT-IDS are the IDs of agents that participated in the creation of a signal. We retrieve a list of patterns associated to these AGENT-IDS."
  (loop for key being each hash-key of *agents-cache*
        for value being each hash-value of *agents-cache*
        when (find agent-ids value
                   :key (lambda (agent) (slot-value agent 'id))
                   :test (lambda (ids id) (find id ids :test #'string=)))
        collect (car (third key))))
;; (get-agent-ids-patterns (list (slot-value (first (get-agents-some :AUD_USD hscom.hsage:*train-tf* '(:bearish))) 'id) (slot-value (first (get-agents-some :AUD_USD hscom.hsage:*train-tf* '(:stagnated))) 'id)))

(def (function d) get-agents-count (instrument timeframe types)
  (declare (ignore types))
  (bind ((instrument (format nil "~a" instrument))
         (timeframe (format nil "~a" timeframe)))
    (conn (query (:select (:count 'agents.id)
                 :from 'agents
                 :join 'agents-patterns
                 :on (:= 'agents.id 'agents-patterns.agent-id)
                 :join 'patterns
                 :on (:= 'agents-patterns.pattern-id 'patterns.id)
                 :where (:and (:= 'instrument instrument)
                              (:= 'timeframe timeframe)
                              (:= 'retired nil)))
               :single))))
;; (get-agents-count :AUD_USD :M15 '((:SINGLE)))

(def (function d) eval-agent (agent rates)
  (let ((perception-fn (gen-perception-fn (perception-fns agent))))
    (cond ((eq *fis-method* :index)
           (eval-ifis-idx (funcall perception-fn rates)
                          (slot-value agent 'antecedents)
                          (slot-value agent 'consequents)))
          (t
           (eval-ifis-gen (funcall perception-fn rates)
                          (slot-value agent 'antecedents)
                          (slot-value agent 'consequents)
                          (slot-value agent 'perceptions-count)
                          (slot-value agent 'rules-count)))
          )))

(def (function d) gen-perception-fn (perception-params)
  (hsper:gen-perception-fn perception-params))

(def (function d) update-agent-fitnesses (instrument timeframe types agent rates idxs)
  (let* ((agents (gethash (list instrument timeframe types) *agents-cache*))
         (agent-idx (position agent agents :test (lambda (agent1 agent2) (string= (slot-value agent1 'id)
                                                                                  (slot-value agent2 'id))))))
    (setf (nth agent-idx agents) (evaluate-agent instrument timeframe agent rates idxs))))

(def (function d) update-agents-fitnesses (instrument timeframe types agents rates idxs)
  (loop for agent in agents
        do (update-agent-fitnesses instrument timeframe types agent rates idxs))
  agents)

(def (function d) -base-reject (agent)
  "Used by AGENT-DOMINATED?-XXX."
  (or (= (length (slot-value agent 'tps)) 0)
      (<= (slot-value agent 'avg-return)
          hscom.hsage:*min-agent-avg-return*)
      (< (length (slot-value agent 'tps))
         hscom.hsage:*min-num-trades-training*)))

(def (function d) agent-dominated?-pareto (agent agents &optional (logp nil))
  (if (-base-reject agent)
      ;; AGENT is dominated.
      (progn
        (when logp
          (log-agent :crap agent))
        t)
      (let* ((agent-id-0 (slot-value agent 'id))
             (avg-revenue-0 (slot-value agent 'avg-revenue))
             (trades-won-0 (slot-value agent 'trades-won))
             (trades-lost-0 (slot-value agent 'trades-lost))
             (agent-direction-0 (aref (slot-value agent 'tps) 0))
             (avg-return-0 (slot-value agent 'avg-return))
             (total-return-0 (slot-value agent 'total-return))
             (activations-0 (slot-value agent 'activations))
             (returns-0 (slot-value agent 'returns))
             ;; (agent-directions (slot-value agent 'tps))
             (stdev-revenue-0 (slot-value agent 'stdev-revenue))
             ;; (entry-times-0 (slot-value agent 'entry-times))
             (is-dominated? nil))
        ;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
        (loop for agent in agents
              do (when (> (length (slot-value agent 'tps)) 0)
                   (let* ((agent-id (slot-value agent 'id))
                          (avg-revenue (slot-value agent 'avg-revenue))
                          (trades-won (slot-value agent 'trades-won))
                          (trades-lost (slot-value agent 'trades-lost))
                          (agent-direction (aref (slot-value agent 'tps) 0))
                          (avg-return (slot-value agent 'avg-return))
                          (total-return (slot-value agent 'total-return))
                          (activations (slot-value agent 'activations))
                          (returns (slot-value agent 'returns))
                          ;; (agent-directions (slot-value agent 'tps))
                          (stdev-revenue (slot-value agent 'stdev-revenue))
                          ;; (entry-times (slot-value agent 'entry-times))
                          )
                     ;; Fitnesses currently being used.
                     (when (or
                            ;; (<= total-return-0 0)
                            (and
                             ;; (> (* agent-direction-0 agent-direction) 0)
                             ;; (>= avg-revenue avg-revenue-0)
                             ;; (< stdev-revenue stdev-revenue-0)

                             (>= trades-won trades-won-0)
                             (<= trades-lost trades-lost-0)
                             (>= avg-return avg-return-0)
                             (>= total-return total-return-0)
                             (>= (/ trades-won
                                    (+ trades-won trades-lost))
                                 (/ trades-won-0
                                    (+ trades-won-0 trades-lost-0)))
                             ;; (vector-1-similarity entry-times entry-times-0)
                             ))
                       ;; Candidate agent was dominated.
                       (when logp
                         (let ((metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST" "AVG-RETURN" "TOTAL-RETURN")))
                           (with-open-stream (s (make-string-output-stream))
                             (format s "<pre><b>(BETA) </b>Agent ID ~a~%" agent-id-0)
                             (format-table s `((,(format nil "~6$" avg-revenue-0) ,trades-won-0 ,trades-lost-0 ,(format nil "~2$" avg-return-0) ,(format nil "~2$" total-return-0))) :column-label metric-labels)
                             (format s "</pre>")
                             (format s "<pre><b>(ALPHA) </b>Agent ID ~a~%" agent-id)
                             (format-table s `((,(format nil "~6$" avg-revenue) ,trades-won ,trades-lost ,(format nil "~2$" avg-return) ,(format nil "~2$" total-return))) :column-label metric-labels)
                             (format s "</pre><hr/>"))))
                       (setf is-dominated? t)
                       (return)))))
        is-dominated?)))

(def (function d) agent-dominated?-mactavator (agent agents &optional (logp nil))
  (if (-base-reject agent)
      ;; AGENT is dominated.
      (progn
        (when logp
          (log-agent :crap agent))
        t)
      (let* ((total-return-0 (slot-value agent 'total-return))
             (avg-return-0 (slot-value agent 'avg-return))
             (activations-0 (slot-value agent 'activations))
             (returns-0 (slot-value agent 'returns))
             (entry-times-0 (slot-value agent 'entry-times)))
        ;; `data`'s going to hold the max activations, returns and entry times per DP.
        (let ((data (make-hash-table)))
          ;; Determining max activations and returns.
          (loop for agent in agents
                for agent-idx from 0
                do (loop for act across (slot-value agent 'activations)
                         for ret across (slot-value agent 'returns)
                         for time across (slot-value agent 'entry-times)
                         do (progn
                              ;; Checking if internal hash-table doesn't exist.
                              (unless (gethash time data)
                                (setf (gethash time data) (make-hash-table :size 3)))
                              (let ((datum (gethash time data)))
                                ;; Updating internal hash-table.
                                (when (or (not (gethash :activation datum))
                                          (> act (gethash :activation datum)))
                                  (setf (gethash :agent-idx datum) agent-idx)
                                  (setf (gethash :activation datum) act)
                                  ;; (setf (gethash :return datum) ret) ;; Keeping in case we want to compare against DPret.
                                  (setf (gethash :total-return datum)
                                        (slot-value agent 'total-return)))))))
          ;; Comparing agent activations, returns, etc. to see if it doesn't get dominated.
          (let ((dominatedp t)
                (dominated-idx -1))
            ;; First checking if our candidate AGENT wins by default because no other agent is trading at a particular DP.
            ;; Also checking if candidate has a positive total return and return at that DP.
            ;; Checking agent data against max values.
            (let ((foundp t))
              (loop for time across entry-times-0
                    for ret across returns-0
                    do (when (and (not (gethash time data))
                                  (> ret 0)
                                  (> avg-return-0 hscom.hsage:*min-agent-avg-return*)
                                  (> total-return-0 0)
                                  )
                         (setf foundp nil)
                         (return)))
              (unless foundp
                ;; Not dominated.
                (setf dominatedp nil)))

            ;; Checking if we already know that it wasn't dominated.
            (when dominatedp
              ;; Checking each DP if agent dominates at that DP.
              (loop for time across entry-times-0
                    for act across activations-0
                    for ret across returns-0
                    do (let ((datum (gethash time data)))
                         ;; We're going to add an agent to the agent pool that has a positive DPret (`ret`),
                         ;; a greater total-return on that DP and a greater than or equal activation than that DP's (and threshold).
                         (when (and datum
                                    (> ret 0)
                                    (> total-return-0 (gethash :total-return datum))
                                    (>= act
                                        (gethash :activation datum)
                                        hscom.hsage:*evaluate-agents-activation-threshold*))
                           ;; Storing what agent got dominated, mainly for logging purposes.
                           (setf dominated-idx (gethash :agent-idx datum))
                           ;; Not dominated. Returning from loop.
                           (setf dominatedp nil)
                           (return)))))
            ;; TODO: Refactor to its own function.
            (when logp
              (if (>= dominated-idx 0)
                  ;; Logging what BETA agent our ALPHA dominated (if any).
                  (let ((dominated-agent (nth dominated-idx agents)))
                    (log-agent :beta dominated-agent)
                    (log-agent :alpha agent))
                  ;; Logging crappy agent who couldn't beat anyone.
                  (if dominatedp
                      (log-agent :crap agent)
                      (log-agent :default agent))))
            ;; Returning result.
            dominatedp)))))

(def (function d) get-agent-by-id (agent-id &key (ret-type :dao))
  (let (result)
    (loop for key being each hash-key of *agents-cache*
          for agents being each hash-value of *agents-cache*
          do (loop for agent in agents
                   do (progn
                        ;; TODO: We can't be certain `key`s will always have this structure.
                        (when (string= (slot-value agent 'id) agent-id)
                          ;; (print (type-of (json:encode-json-to-string agent)))
                          (if (eq ret-type :dao)
                              (setf result agent)
                              (let ((pre-result (json:decode-json-from-string (json:encode-json-to-string agent))))
                                (push `(:instrument . ,(first key)) pre-result)
                                (push `(:timeframe . ,(second key)) pre-result)
                                (push `(:type . ,(third key)) pre-result)
                                (push `(:r/r . ,(format-rr (assoccess pre-result :avg-sl) (assoccess pre-result :avg-tp))) pre-result)
                                (setf result pre-result))
                              )))))
    result))
;; (get-agent-by-id "9B394F8B-EA70-441E-907E-FEA02F035E0F" :ret-type :alist)

;; for key being each hash-key of *agents-cache*
;; for value being each hash-value of *agents-cache*

;; (get-agent-by-id "5530FA06-85AD-4D95-AFF1-0F8220702E6D" :ret-type :alist)
;; (slot-value (get-agent-by-id "F9E434C2-1C4B-4C85-9E29-973A26399B3F") 'perception-fns)

(def (function d) get-agent (instrument timeframe types agent-id)
  (find agent-id (gethash (list instrument timeframe types) *agents-cache*)
        :key (lambda (agent) (slot-value agent 'id))
        :test #'string=))
;; (get-agent :EUR_USD hscom.hsage:*train-tf* '(:BULLISH) "48F3970F-36C1-4A49-9E54-95746CFEA9FE")
;; (slot-value (first (get-agents-some :EUR_USD hscom.hsage:*train-tf* '(:BULLISH))) 'id)

(def (function d) eval-agents (instrument timeframe types rates)
  (let (tps sls activations ids)
    (let ((agents (get-agents-some instrument timeframe types)))
      (loop for agent in agents
            do (multiple-value-bind (tp sl activation)
                   (eval-agent agent rates)
                 (let* ((last-rate (last-elt rates))
                        ;; Checking if calculated SL is greater than Nx the current spread.
                        ;; If not, we set Nx the current spread as the SL.
                        (corrected-sl (let ((nx-spread (* hscom.hsage:*min-n-times-spread-sl*
                                                          (abs (- (hsinp.rates:->close-bid last-rate)
                                                                  (hsinp.rates:->close-ask last-rate))))))
                                        (if (>= (abs sl) nx-spread)
                                            sl
                                            (if (plusp sl)
                                                nx-spread
                                                (* -1 nx-spread))))))
                   (push tp tps)
                   (push corrected-sl sls)
                   (push activation activations)
                   (push (slot-value agent 'id) ids)))))
    ;; (format t "~a, ~a~%" (apply #'min activations) (apply #'max activations))
    (let* ((idxs (hsage.utils:sorted-indexes activations #'>))
           (tp (nth (position 0 idxs) tps))
           (sl (nth (position 0 idxs) sls))
           (activation (nth (position 0 idxs) activations))
           ;; (dir (if (plusp tp)
           ;; 	    activation
           ;; 	    (- activation)))
           (len (min hscom.hsage:*consensus-threshold* (length activations))))

      ;; Majority of agents must agree on direction.
      ;; (let ((score (loop for idx from 0 below len
      ;; 			 with score = 0
      ;; 			 when (let ((nth-tp (nth (position idx idxs) tps)))
      ;; 				(> (* nth-tp tp) 0))
      ;; 			   do (incf score)
      ;; 			 finally (return score))))
      ;; 	(setf tp (* tp (/ score len)))
      ;; 	(setf sl (* sl (/ len score)))
      ;; 	;; (when (< score
      ;; 	;; 	 (/ len 2))
      ;; 	;;   (setf tp 0)
      ;; 	;;   (setf sl 0)
      ;; 	;;   (setf activation 0)
      ;; 	;;   )
      ;; 	)

      (unless (< len 3)
        (let* ((bullish-acts (loop for idx from 0 below len
                                   when (plusp (nth (position idx idxs) tps))
                                   collect (nth (position idx idxs) activations)))
               (bearish-acts (loop for idx from 0 below len
                                   when (minusp (nth (position idx idxs) tps))
                                   collect (nth (position idx idxs) activations)))
               (idx-top-bullish (loop for idx from 0 below len
                                      do (when (plusp (nth (position idx idxs) tps))
                                           (return idx))))
               (idx-top-bearish (loop for idx from 0 below len
                                      do (when (minusp (nth (position idx idxs) tps))
                                           (return idx))))
               (bullish-act (when bullish-acts (mean bullish-acts)))
               (bearish-act (when bearish-acts (mean bearish-acts))))
          (when (and bullish-act bearish-act)
            (let ((pos (if (> bullish-act bearish-act)
                           (position idx-top-bullish idxs)
                           (position idx-top-bearish idxs))))
              (setf tp (nth pos tps))
              (setf sl (nth pos sls))
              (setf activation (nth pos activations))))))

      ;; Cancel trade only if N-1 consensus agents disaggree with top agent.
      ;; (let ((score (loop for idx from 1 below len
      ;; 			 with score = 0
      ;; 			 when (let ((nth-tp (nth (position idx idxs) tps)))
      ;; 				;; Disagrees.
      ;; 				(< (* nth-tp tp) 0))
      ;; 			   do (incf score)
      ;; 			 finally (return score))))
      ;; 	(when (and (not (= score 0))
      ;; 		   (= (1- hscom.hsage:*consensus-threshold*) score))
      ;; 	  (setf tp 0)
      ;; 	  (setf sl 0)
      ;; 	  (setf activation 0)))

      ;; Using activation as weight to determine direction.
      ;; (loop for idx from 1 below len
      ;; 	    do (let* ((pos (position idx idxs))
      ;; 		      (nth-tp (nth pos tps))
      ;; 		      (nth-act (nth pos activations)))
      ;; 		 (incf dir (if (plusp nth-tp)
      ;; 			       nth-act
      ;; 			       (- nth-act)))))

      ;; (loop for idx from 1 below len
      ;; 	    do (let* ((pos (position idx idxs))
      ;; 		      (nth-tp (nth pos tps))
      ;; 		      ;; (nth-sl (nth pos sls))
      ;; 		      ;; (nth-act (nth idx activations))
      ;; 		      )

      ;; 		 ;; (incf dir (if (plusp nth-tp)
      ;; 		 ;; 	       nth-act
      ;; 		 ;; 	       (- nth-act)))

      ;; 		 (when (< (* nth-tp tp) 0)
      ;; 		   (setf tp 0)
      ;; 		   (setf sl 0)
      ;; 		   (return))

      ;; 		 ;; (incf tp (abs nth-tp))
      ;; 		 ;; (incf sl (abs nth-sl))
      ;; 		 ))

      (values (/ tp 1)
              (/ sl 1)
              activation
              (list (nth (position 0 idxs) ids)))

      ;; (values
      ;;  (/ (* (abs tp) (if (/= dir 0) (/ dir (abs dir)) 0)) 1)
      ;;  (/ (* (abs tp) (if (/= dir 0) (- (/ dir (abs dir))) 0)) 1)
      ;;  activation
      ;;  (list (nth (position 0 idxs) ids))
      ;;  )
      )))

(def (function d) get-most-activated-agents (instrument timeframe types &optional (n 10))
  ;; We're ordering the agents in ascending order to avoid
  ;; reversing them after `push`ing them to `bullish-agents` and `bearish-agents`.
  (let ((agents (sort (get-agents-some instrument timeframe types) #'< :key (lambda (agent) (slot-value agent 'avg-activation))))
        (bullish-agents)
        (bearish-agents))
    (loop for agent in agents
          ;; Checking if bullish or bearish agent.
          do (if (plusp (slot-value agent 'avg-tp))
                 (push agent bullish-agents)
                 (push agent bearish-agents)))
    (values
     ;; n bullish.
     (loop for agent in bullish-agents
           for i from 0 below n
           collect agent)
     (loop for agent in bearish-agents
           for i from 0 below n
           collect agent))))
;; (get-most-activated-agents :AUD_USD :H1 '(:BULLISH :BEARISH) 10)

(def (function d) trade-most-activated-agents (instrument timeframe types agents testing-dataset creation-time &key (test-size 50))
  "Used in `test-most-activated-agents`."
  (loop for agent in agents
        do (let ((test-fitnesses (evaluate-agent instrument timeframe agent testing-dataset :test-size test-size :return-fitnesses-p t))
                 (agent-id (slot-value agent 'id)))
             (when test-fitnesses
               (push-to-log "Testing process successful."))
             (multiple-value-bind (tp sl activation)
                 (eval-agent agent testing-dataset)
               (push-to-log (format nil "Prediction. TP: ~a, SL: ~a." tp sl))
               (when (and (/= tp 0)
                          ;; (if (not (eq instrument :USD_CNH)) (< (assoccess prediction :tp) 100) t)
                          (> (abs tp) (abs sl))
                          (/= sl 0)
                          (< (* tp sl) 0)
                          (> (abs (/ tp sl))
                             hscom.hsage:*agents-min-rr-signal*)
                          (> (abs (to-pips instrument sl)) *min-pips-sl*)
                          ;; (< (to-pips instrument (abs sl)) 20)
                          (/= (assoccess test-fitnesses :trades-won) 0)
                          (/= (+ (assoccess test-fitnesses :trades-won)
                                 (assoccess test-fitnesses :trades-lost))
                              0))
                 (push-to-log (format nil "Trying to create trade. Agents ID: ~a" agent-id))
                 (insert-trade agent-id instrument timeframe types test-fitnesses test-fitnesses tp sl activation testing-dataset creation-time)
                 (push-to-log "Trade created successfully."))))))

(def (function d) test-most-activated-agents (instrument timeframe types testing-dataset &key (test-size 50))
  (let ((creation-time (local-time:timestamp-to-unix (local-time:now))))
    (multiple-value-bind (bullish-agents bearish-agents)
        (get-most-activated-agents instrument timeframe types)
      (trade-most-activated-agents instrument timeframe '(:BULLISH) bullish-agents testing-dataset creation-time :test-size test-size)
      (trade-most-activated-agents instrument timeframe '(:BEARISH) bearish-agents testing-dataset creation-time :test-size test-size))))

(def (function d) -init-patterns (instrument timeframe)
  (unless (get-patterns instrument timeframe '(:BULLISH))
    (insert-pattern instrument timeframe :BULLISH))
  (unless (get-patterns instrument timeframe '(:BEARISH))
    (insert-pattern instrument timeframe :BEARISH))
  (unless (get-patterns instrument timeframe '(:STAGNATED))
    (insert-pattern instrument timeframe :STAGNATED))
  (unless (get-patterns instrument timeframe '(:SINGLE))
    (insert-pattern instrument timeframe :SINGLE)))

(def (function d) init-patterns ()
  (loop for instrument in hscom.hsage:*instruments*
        do (loop for timeframe in hscom.hsage:*all-timeframes*
                 do (-init-patterns instrument timeframe))))
;; (init-patterns)

(def (function d) make-agent (inputs outputs perception-fns lookahead-count lookbehind-count)
  "Used for manual agent creation."
  (let ((agent (make-instance 'agent)))
    (setf (slot-value agent 'perception-fns) (format nil "~s" perception-fns))
    (setf (slot-value agent 'lookahead-count) lookahead-count)
    (setf (slot-value agent 'lookbehind-count) lookbehind-count)
    (multiple-value-bind (antecedents consequents)
        (make-ant-con inputs outputs)
      (setf (slot-value agent 'antecedents) (format nil "~s" antecedents))
      (setf (slot-value agent 'consequents) (format nil "~s" consequents)))
    agent))

(comment
 ;; Testing MAKE-AGENT
 (let ((perc (hsper:gen-random-perceptions 3)))
   (multiple-value-bind (inp out)
       (get-inputs-outputs 2 :AUD_USD *rates*
                           (hsper:gen-perception-fn (hscom.utils:assoccess perc :perception-fns))
                           (hscom.utils:assoccess perc :lookahead-count)
                           (hscom.utils:assoccess perc :lookbehind-count)
                           :direction-fn #'minusp)
     (let ((agent (make-agent inp out (hscom.utils:assoccess perc :perception-fns) 10 10)))
       (list (antecedents agent)
             (consequents agent)))
     )))

;; (defparameter *agent* (gen-agent 3 :AUD_USD *rates* (hscom.utils:assoccess (hsper:gen-random-perceptions 2) :perception-fns) 10 100))
;; (antecedents *agent*)

(def (function d) interactive-make-agent (&key (lookahead-count 10) (rules-count 3))
  "Asks the user what perception function to generate for an agent."
  (block all
    (let ((options (get-perceptions))
          (p-rows)
          (percs)
          (instrument :AUD_USD)
          (lookbehind-count -1)
          (direction-fn)
          ;; (final-agent)
          )
      ;; Asking for instrument.
      (loop for instrument in hscom.hsage:*instruments*
            for i from 0
            do (format t "(~a) ~a~%" (1+ i) instrument))
      (format t "~%Choose a market (default = ~a):~%" instrument)
      (setf instrument (let ((ans (read-line)))
                         (unless (string= ans "") (nth (1- (read-from-string ans)) hscom.hsage:*instruments*))))
      ;; Capturing table elements.
      (loop for perc in options
            for i from 1
            do (progn
                 (push (list (format nil "(~a)" i)
                             (assoccess perc :name)
                             (assoccess perc :documentation))
                       p-rows)))
      (let ((table (with-open-stream (s (make-string-output-stream))
                     (format-table s (reverse p-rows)
                                   :column-label '("#" "Name" "Documentation"))
                     (get-output-stream-string s))))
        ;; Collecting perception functions.
        (loop
          do (let ((answer -1)
                   (args '()))
               (format t "~%~a~%" table)
               (when (> (length percs) 0)
                 (format t "~%These perception functions have been chosen so far:~%")
                 (loop for pnum in percs
                       do (progn
                            (format t "~%~a~%" (assoccess (nth (aref pnum 0) options) :name))
                            (loop for param in (reverse (assoccess (nth (aref pnum 0) options) :params))
                                  for i from 1
                                  do (format t "~a = ~a~%"
                                             (assoccess param :name)
                                             (aref pnum i)))))
                 (format t "~%"))

               (format t "Choose a perception function by entering their associated numbers (1 - ~a) or just press ENTER to finish:~%" (length options))

               (setf answer (let ((ans (read-line))) (if (string= ans "") -1 (1- (read-from-string ans)))))

               (if (< answer 0)
                   (progn
                     (format t "Finished capturing perception functions.~%~%")
                     (if (= (length percs) 0)
                         (return-from all)
                         (return)))
                   (progn
                     (format t "Please give values to the perception function's parameters:~%")
                     (loop for param in (assoccess (nth answer options) :params)
                           do (progn
                                (format t "~%~a~%~a (default = ~a): "
                                        (assoccess param :documentation)
                                        (assoccess param :name)
                                        (assoccess param :default))
                                (let ((ans (read-line)))
                                  (if (string= ans "")
                                      (push (assoccess param :default) args)
                                      (push (read-from-string ans) args)))))
                     (multiple-value-bind (perc-vector lookbehind)
                         (apply #'funcall (assoccess (nth answer options) :array-fn) args)
                       (when (> lookbehind lookbehind-count)
                         (setf lookbehind-count lookbehind))
                       (push perc-vector percs)))))))
      ;; Asking for direction function.
      (format t "Should the agent be BULLISH (input any positive number) or BEARISH (input any negative number)? (default = random)" )
      (setf direction-fn (let ((ans (read-line)))
                           (if (string= ans "")
                               (if (> (random-float 0 1) 0.5)
                                   #'plusp
                                   #'minusp)
                               (if (> (read-from-string ans) 0)
                                   #'plusp
                                   #'minusp))))
      ;; Asking for rules count.
      (format t "How many rules should your agent have (default = ~a):~%" rules-count)
      (setf rules-count (let ((ans (read-line))) (if (string= ans "") rules-count (read-from-string ans))))
      ;; Asking for lookahead.
      (format t "How many datapoints in the future do you want to consider for calculating TP & SL (default = ~a):~%" lookahead-count)
      (setf lookahead-count (let ((ans (read-line))) (if (string= ans "") lookahead-count (read-from-string ans))))
      ;; Creating fuzzy system.
      (let ((percs (make-array (length percs) :initial-contents percs)))
        (loop
          do (multiple-value-bind (inputs outputs idxs)
                 (get-inputs-outputs rules-count
                                     instrument
                                     *rates*
                                     (hsper:gen-perception-fn percs)
                                     lookahead-count
                                     lookbehind-count
                                     :direction-fn direction-fn)
               ;; Getting rate plots
               (let ((agent (make-agent inputs outputs percs lookahead-count lookbehind-count))
                     (rate-plots (apply #'concatenate 'string
                                        (loop for idx in idxs
                                              collect (plot-rates (subseq *rates* (- idx lookbehind-count) (+ idx lookahead-count)))))))
                 (multiple-value-bind (antecedents consequents)
                     (plot-agent-rules agent)
                   (print-in-columns (list ;; rate-plots
                                      antecedents consequents)))
                 (format t "Are you satisfied with this configuration (Y or N)? (default = N):~%")
                 (let ((ans (read-line)))
                   (when (or (string= ans "y")
                             (string= ans "Y"))
                     ;; (setf final-agent agent)
                     (return-from all agent)
                     )))
               )))
      )))
;; (interactive-make-agent)

(def (function d) -fill-string-with-spaces (string width)
  "Used by PRINT-IN-COLUMNS."
  (if (> (- width (length string)) 0)
      (format nil "~a~a"
              (string-trim '(#\Newline) string)
              (format nil "~v@{~A~:*~}" (- width (length string)) " ")))
  )
;; (-fill-string-with-spaces "meow meow" 40)

(def (function d) -fill-split-string-with-newlines (split-string height)
  "Used by PRINT-IN-COLUMNS."
  (let ((row-width (length (first split-string))))
    (if (> height (length split-string))
        (append split-string
                (make-list (- height (length split-string))
                           :initial-element (format nil "~v@{~A~:*~}" row-width " ")))
        split-string)))

(def (function d) -split-string (string)
  "Used by PRINT-IN-COLUMNS."
  (let ((split (cl-ppcre:split "\\n" string)))
    (values
     (if split split "")
     ;; width
     (+ 2
        (if (= (length string) 0)
            0
            (apply #'max (mapcar #'length split))))
     ;; height
     (length split))))

(def (function d) print-in-columns (strings)
  (let* ((max-height 0)
         (splits (mapcar (lambda (string)
                           (multiple-value-bind (split width height)
                               (-split-string string)
                             (when (> height max-height)
                               (setf max-height height))
                             (loop for s in split
                                   collect (-fill-string-with-spaces s width))))
                         strings)))
    (apply #'mapcar (lambda (&rest rows)
                      (format t "~{|~a |~}~%" rows))
           (mapcar (lambda (split-string)
                     (-fill-split-string-with-newlines split-string max-height))
                   splits)
           ))
  (values))
(comment
 (print-in-columns '("hello
how are you
I hope you're good" "good
bye
it was nice
to see
you"))
 )

(comment
 (time (let ((perc (hsper:gen-random-perceptions 3)))
         (get-inputs-outputs 10 :AUD_USD *rates*
                             (hsper:gen-perception-fn (hscom.utils:assoccess perc :perception-fns))
                             (hscom.utils:assoccess perc :lookahead-count)
                             (hscom.utils:assoccess perc :lookbehind-count)
                             :direction-fn #'minusp)))
 )

;; (get-perceptions)
;; (hsper:fixed=>sma-close 5 10)
;; (gen-perception-fn (get-perceptions))
;; (gen-perception-fn (format nil "~a" (hsper:random=>sma-close)))

;; (ql:quickload :cl-charms)
;; (cl-charms:)
;; (ql:system-apropos "menu")

;; Perception fns (build a list of arrays of arrays)
;; lookahead-count
;; lookbehind-count (automatic)
;; test agent before adding to pool to avoid errors
;; Steps:
;; Choose perception functions
;; Choose input-outputs

(def (function d) plot-xy (xs ys)
  (let ((output #P"/tmp/hermes-plot-xy.txt"))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :xlabel "X"
                             :ylabel "Y"
                             :output output
                             :terminal :dumb)
      (loop for x in xs
            for y in ys
            do (eazy-gnuplot:plot (lambda ()
                                    (loop for x in x
                                          for y in y
                                          do (format t "~&~a ~a" x y)))
                                  :using '(1 2)
                                  :title ""
                                  :with '(:linespoint))))
    (string-trim '(#\Page #\Newline) (hscom.utils:file-get-contents output))))
;; (plot-xy (list (iota 10) (iota 10 :start 1)) (list (iota 10) (iota 10 :start 5)))

(def (function d) plot-rates (rates &optional (type :close-frac))
  (let ((output #P"/tmp/hermes-plot-rates.txt"))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :xlabel "X"
                             :ylabel "GM"
                             :output output
                             :terminal :dumb)
      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xdata time")
      (format t "~%set timefmt \"%s\"")
      ;; (format t "~%set format x \"%d/%m/%Y\t%H:%M\"")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")
      (eazy-gnuplot:plot (lambda ()
                           (loop for rate in rates
                                 do (format t "~&~a ~a"
                                            (/ (read-from-string (assoccess rate :time)) 1000000)
                                            (assoccess rate type))))
                         :using '(1 2)
                         :title ""
                         :with '(:linespoint))
      (string-trim '(#\Page #\Newline) (hscom.utils:file-get-contents output))
      )))
;; (plot-rates (subseq *rates* 0 10) :close-bid)

(def (function d) plot-agent-rules (agent)
  (let ((output #P"/tmp/hermes-plot-fuzzy.txt"))
    (let (antecedents consequents)
      (loop for antecedent across (antecedents agent)
            for perc-fn across (perception-fns agent)
            do (progn
                 (eazy-gnuplot:with-plots (*standard-output* :debug nil)
                   (eazy-gnuplot:gp-setup :xlabel (string-upcase (format nil "~a" (nth-perception (aref perc-fn 0))))
                                          :ylabel "GM"
                                          :output output
                                          :terminal :dumb)
                   (loop for ant across antecedent
                         do (eazy-gnuplot:plot (lambda ()
                                                 (format t "~&~a ~a" (aref ant 0) 0)
                                                 (format t "~&~a ~a" (aref ant 1) 1))
                                               :using '(1 2)
                                               :title ""
                                               :with '(:linespoint)
                                               )))
                 (push (string-trim '(#\Page #\Newline) (hscom.utils:file-get-contents output)) antecedents)))
      (setf consequents (make-list (length antecedents) :initial-element ""))
      ;; Consequents
      ;; TODO: These consequents follow the structure of "stay restricted to each observed rule", i.e.
      ;; we're not "mixing" antecedents nor consequents from different observed rules.
      (let ((tps)
            (sls))
        ;; Collecting all TPs and SLs first.
        ;; They're all the same (constrained to rules).
        (let ((cons (aref (consequents agent) 0)))
          (loop
            ;; We need to ignore the repeated line.
            for i from 0 below (length cons) by 2
            do (progn
                 (push (aref (aref cons i) 0) tps)
                 (push (aref (aref cons i) 1) sls))))
        ;; TP
        (eazy-gnuplot:with-plots (*standard-output* :debug nil)
          (eazy-gnuplot:gp-setup :xlabel "Take Profit"
                                 :ylabel "GM"
                                 :output output
                                 :terminal :dumb)
          (loop for tp in tps
                do (eazy-gnuplot:plot (lambda ()
                                        (format t "~&~a ~a" (aref tp 0) 0)
                                        (format t "~&~a ~a" (aref tp 1) 1))
                                      :using '(1 2)
                                      :title ""
                                      :with '(:linespoint)
                                      )))
        ;; Adding TPs.
        ;; (cl-ppcre:regex-replace "\r" "h")
        (setf (nth 0 consequents) (string-trim '(#\Page #\Newline) (hscom.utils:file-get-contents output)))
        ;; SL
        (eazy-gnuplot:with-plots (*standard-output* :debug nil)
          (eazy-gnuplot:gp-setup :xlabel "Stop Loss"
                                 :ylabel "GM"
                                 :output output
                                 :terminal :dumb)
          (loop for sl in sls
                do (eazy-gnuplot:plot (lambda ()
                                        (format t "~&~a ~a" (aref sl 0) 0)
                                        (format t "~&~a ~a" (aref sl 1) 1))
                                      :using '(1 2)
                                      :title ""
                                      :with '(:linespoint)
                                      )))
        (setf (nth 0 consequents)
              (format nil "~a~%~%~a"
                      (nth 0 consequents)
                      (string-trim '(#\Page #\Newline) (hscom.utils:file-get-contents output)))))
      ;; (format t "~%Antecedents:~%~{~a~}" antecedents)
      ;; (format t "~%Consequents:~%~{~a~}" consequents)
      (values (concatenate 'string
                           (format nil "  Antecedents:~%~%")
                           (apply #'concatenate 'string antecedents))
              (concatenate 'string
                           (format nil "  Consequents:~%~%")
                           (apply #'concatenate 'string consequents))))))
;; (plot-agent-rules *agent*)
;; (defparameter *agent* (gen-agent 2 :AUD_USD *rates* (hscom.utils:assoccess (hsper:gen-random-perceptions 3) :perception-fns) 10 100))
;; (antecedents *agent*)
;; (consequents *agent*)

;; (aref (consequents *agent*) 0)
;; (hsint:eval-ifis-gen '(1.1744353952569169 1.1847066666666666 1.17137625)
;; 		     (antecedents *agent*)
;; 		     (consequents *agent*)
;; 		     (perceptions-count *agent*)
;; 		     (rules-count *agent*))
;; (make-ifis *agent* 3 :AUD_USD *rates*)

;; (conn (query (:select (:count 'antecedents) :from 'agents) :alist))
;; (length (assoccess (conn (query (:select 'antecedents :from 'agents) :alist)) :antecedents))
;; (get-perceptions-count (assoccess (conn (query (:select 'antecedents :from 'agents) :alist)) :antecedents))

(def (function d) gen-agent (num-rules instrument rates idxs perception-fns lookahead-count lookbehind-count)
  (let ((agent (make-instance 'agent)))
    (setf (slot-value agent 'creation-begin-time) (read-from-string (assoccess (first rates) :time)))
    (setf (slot-value agent 'creation-end-time) (read-from-string (assoccess (last-elt rates) :time)))
    (setf (slot-value agent 'perception-fns) perception-fns)
    (setf (slot-value agent 'lookahead-count) lookahead-count)
    (setf (slot-value agent 'lookbehind-count) lookbehind-count)
    (setf (slot-value agent 'rules-count) num-rules)
    (setf (slot-value agent 'perceptions-count)
          (get-perceptions-count perception-fns))
    (multiple-value-bind (antecedents consequents)
        (make-ifis agent num-rules instrument rates idxs)
      (setf (slot-value agent 'antecedents) antecedents)
      (setf (slot-value agent 'consequents) consequents))
    agent))

(def (function d) gen-agents (num-agents num-rules instrument rates perception-fns lookahead-count lookbehind-count)
  (loop repeat num-agents collect (gen-agent instrument num-rules rates perception-fns lookahead-count lookbehind-count)))
;; (gen-agents 2 3 *rates* (assoccess *beliefs* :perception-fns) 10 55)

(def (function d) get-same-direction-outputs-idxs (instrument rates count &key (lookahead-count 10) (lookbehind-count 10) direction-fn)
  (let* ((r (random-float 0 1))
         (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
         (opposite-pred (if (> r 0.5) #'minusp #'plusp))
         (idxs (shuffle (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count)))
         (result))
    (loop for idx in idxs
          do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
               (when (and (< (length result) count)
                          (funcall pred (assoccess tp-sl :tp))
                          (/= (assoccess tp-sl :sl) 0)
                          (> (abs (assoccess tp-sl :sl)) (from-pips instrument hscom.hsage:*min-sl*))
                          (> (abs (/ (assoccess tp-sl :tp)
                                     (assoccess tp-sl :sl)))
                             hscom.hsage:*agents-min-rr-creation*)
                          (or (eq instrument :USD_CNH)
                              (< (abs (assoccess tp-sl :tp)) (from-pips instrument hscom.hsage:*max-tp*))))
                 (push idx result))))
    (if (> (length result) 1)
        result
        (get-same-direction-outputs-idxs instrument rates count
                                         :lookahead-count lookahead-count
                                         :lookbehind-count lookbehind-count
                                         :direction-fn opposite-pred))))
;; (get-same-direction-outputs-idxs *rates* :lookahead-count 5)

(def (function d) get-same-direction-outputs-idxs-all (instrument rates &key (lookahead-count 10) (lookbehind-count 10) direction-fn)
  (let* ((r (random-float 0 1))
         (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
         (opposite-pred (if (> r 0.5) #'minusp #'plusp))
         (idxs (iota (- (length rates) lookahead-count lookbehind-count) :start lookbehind-count))
         (result))
    (loop for idx in idxs
          do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead-count)))
               (when (and (funcall pred (assoccess tp-sl :tp))
                          (/= (assoccess tp-sl :sl) 0)
                          (> (abs (assoccess tp-sl :sl)) (from-pips instrument hscom.hsage:*min-sl*))
                          (> (abs (/ (assoccess tp-sl :tp)
                                     (assoccess tp-sl :sl)))
                             hscom.hsage:*agents-min-rr-creation*)
                          (or (eq instrument :USD_CNH)
                              (< (abs (assoccess tp-sl :tp)) (from-pips instrument hscom.hsage:*max-tp*))))
                 (push idx result))))
    (if (> (length result) 1)
        result
        (get-same-direction-outputs-idxs-all instrument rates
                                             :lookahead-count lookahead-count
                                             :lookbehind-count lookbehind-count
                                             :direction-fn opposite-pred))))
;; (length (get-same-direction-outputs-idxs-all :AUD_USD *rates* :lookahead-count 10))

(def (function d) get-inputs-outputs (num-rules instrument rates idxs perception-fn lookahead-count lookbehind-count &key direction-fn)
  (let* ((idxs (if idxs (^(sort (subseq (shuffle (alexandria:copy-sequence 'list idxs)) 0 num-rules) #'<))
                   (^(sort _ #'<)
                     (cond (*exhaust-rules-in-creation-dataset-p*
                            (^(remove-duplicates _)
                              (get-same-direction-outputs-idxs-all
                               instrument rates
                               :lookahead-count lookahead-count
                               :lookbehind-count lookbehind-count
                               :direction-fn direction-fn)))
                           (t
                            (^(remove-duplicates _)
                              (get-same-direction-outputs-idxs
                               instrument rates num-rules
                               :lookahead-count lookahead-count
                               :lookbehind-count lookbehind-count
                               :direction-fn direction-fn)))))))
         (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
         (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead-count))))
    (values chosen-inputs chosen-outputs idxs)))

(comment
  (time (bind ((perc (hsper:gen-random-perceptions 5))
               ((:values inputs outputs)
                (get-inputs-outputs 10 :EUR_USD *rates*
                                    (hsper:gen-perception-fn (hscom.utils:assoccess perc :perception-fns))
                                    (hscom.utils:assoccess perc :lookahead-count)
                                    (hscom.utils:assoccess perc :lookbehind-count))))
          (list (length inputs)
                (length outputs)))))

(def (function d) make-ant-con (inputs outputs)
  (values
   (let* ((v (flatten
              (loop
                for inputs in (apply #'mapcar #'list inputs)
                for idx from 0
                collect (let ((inputs (sort (copy-sequence 'list inputs) #'<)))
                          (loop
                            for i from 0
                            for input in inputs
                            with max-inp-idx = (1- (length inputs))
                            collect (cond ((= i 0)
                                           (list input input (nth (1+ i) inputs) input))
                                          ((= i max-inp-idx)
                                           (list (nth (1- i) inputs) input input input))
                                          (t (list (nth (1- i) inputs) input (nth (1+ i) inputs) input)))))))))
     (make-array (length v) :initial-contents v))
   (let* ((v (flatten
              (loop for output in outputs
                    collect (let* ((tp (assoccess output :tp))
                                   (sl (assoccess output :sl)))
                              ;; Consequent creation.
                              (list 0 tp
                                    (* hscom.hsage:*n-times-sl-for-max-sl* sl) sl))))))
     (make-array (length v) :initial-contents v))))
;; (make-ifis *agent* 3 :AUD_USD *rates*)

(def (function d) make-ifis (agent num-rules instrument rates idxs)
  "Analytical version."
  (let* ((perception-fn (gen-perception-fn (perception-fns agent)))
         (lookahead-count (slot-value agent 'lookahead-count))
         (lookbehind-count (slot-value agent 'lookbehind-count)))
    (multiple-value-bind (chosen-inputs chosen-outputs)
        (get-inputs-outputs num-rules instrument rates idxs perception-fn lookahead-count lookbehind-count)
      (make-ant-con chosen-inputs chosen-outputs))))
;; (plot-agent-rules *agent*)

;; (length (read-from-string (assoccess (conn (query (:select 'antecedents :from 'agents) :alist)) :antecedents)))

(def (function d) log-agent (type agent)
  (let ((agent-id (slot-value agent 'id))
        (avg-revenue (slot-value agent 'avg-revenue))
        (trades-won (slot-value agent 'trades-won))
        (trades-lost (slot-value agent 'trades-lost))
        (avg-return (slot-value agent 'avg-return))
        (total-return (slot-value agent 'total-return))
        (avg-sl (slot-value agent 'avg-sl))
        (avg-tp (slot-value agent 'avg-tp))
        (metric-labels '("AVG-REVENUE" "TRADES-WON" "TRADES-LOST" "AVG-RETURN" "TOTAL-RETURN" "AVG-RR" "DIRECTION")))
    (with-open-stream (s (make-string-output-stream))
      (format s "<pre><b>(~a) </b>Agent ID ~a~%" type agent-id)
      (format-table s `((,(format nil "~6$" avg-revenue)
                          ,trades-won
                          ,trades-lost
                          ,(format nil "~2$" avg-return)
                          ,(format nil "~2$" total-return)
                          ,(format-rr avg-sl avg-tp)
                          ,(if (plusp avg-tp) "BULL" "BEAR")))
                    :column-label metric-labels)
      (format s "</pre><hr/>")
      (push-to-agents-log (get-output-stream-string s))))
  ;; Don't return anything.
  (values))

(def (function d) optimization (instrument timeframe types gen-agent-fn rates idxs stop-count &optional (stop-criterion))
  ($log $trace :-> :optimization)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents-some instrument timeframe types)))
                  (update-agents-fitnesses instrument timeframe types agents rates idxs)
                  (loop repeat hscom.hsage:*initial-agent-count*
                        collect (evaluate-agent instrument timeframe (funcall gen-agent-fn) rates idxs))))
        (purged-agents))
    ($log $info (format nil "~a agents retrieved to start optimization for ~a ~a" (length agents) instrument timeframe))
    (loop with first-iteration-p = t
          with until-timestamp = (local-time:timestamp+ (local-time:now) stop-count :sec)
          with evaluations = 0
          do (progn
               (if (and (not first-iteration-p)
                        ;; (local-time:timestamp> (local-time:now) until-timestamp)
                        (if (eq stop-criterion :time)
                            (local-time:timestamp> (local-time:now) until-timestamp)
                            (> evaluations stop-count)))
                   (progn
                     ;; Inserting new agents in Pareto Frontier.
                     ($log $info (format nil "Updating Pareto frontier with ~a agents for ~a ~a" (length agents) instrument timeframe))
                     (conn (loop for agent in agents
                                 do (unless (get-agent instrument timeframe types (slot-value agent 'id))
                                      ;; (push-to-log (format nil "Inserting new agent with ID ~a" (slot-value agent 'id)))
                                      ;; (log-agent :omega agent)
                                      (add-agent agent instrument timeframe types))))
                     ($log $info "Pareto frontier updated successfully")
                     (return))
                   (block opt
                     (let* ((challenger (list (evaluate-agent instrument timeframe (funcall gen-agent-fn) rates idxs)))
                            (is-dominated? (when hscom.hsage:*optimize-p*
                                             (agent-dominated?-pareto (car challenger) agents t))))
                       ;; No longer the first iteration after this.
                       (setf first-iteration-p nil)
                       ;; Logging agent direction.
                       (incf evaluations)
                       (when (and (> evaluations stop-count)
                                  (eq stop-criterion :evaluations))
                         (return-from opt))
                       (unless is-dominated?
                         ;; Purging agents.
                         (loop named trials
                               for in-trial in agents
                               do (progn
                                    ;; (incf evaluations)
                                    (if (and hscom.hsage:*optimize-p*
                                             (agent-dominated?-pareto in-trial challenger))
                                        (remove-agent in-trial instrument timeframe types)
                                        (push in-trial purged-agents))
                                    (when (and (> evaluations stop-count)
                                               (eq stop-criterion :evaluations))
                                      (return-from trials))
                                    ))
                         (push (first challenger) purged-agents)
                         (setf agents purged-agents)
                         (setf purged-agents nil)
                         (when (and (> evaluations stop-count)
                                    (eq stop-criterion :evaluations))
                           (return-from opt))))))
               (when (= (mod evaluations 10) 0)
                 ($log $debug (format nil "Optimized for ~a for ~a ~a" evaluations instrument timeframe))))))
  ($log $trace :<- :optimization))

(def (function d) agents-to-alists (agents)
  (let* ((agents-props (prepare-agents-properties agents))
         (vals (loop for agent-props in agents-props
                     collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
                                   (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
                               (append agent-props
                                       `((:r/r . ,(format-rr avg-sl avg-tp))))))))
    (when (> (length agents-props) 0)
      vals)))
;; (agents-to-alists (get-agents-some :AUD_USD hscom.hsage:*train-tf* '(:BULLISH)))

(def (function d) get-agents-all (&optional (limit -1) (offset 0))
  (let ((markets (make-hash-table)))
    (loop for instrument in hscom.hsage:*forex*
          do (let ((agents (make-hash-table)))
               (loop for types in *type-groups*
                     do (let ((values (agents-to-alists (get-agents-some instrument hscom.hsage:*train-tf* (list types) limit offset))))
                          (when values
                            (setf (gethash types agents) values))
                          ))
               (setf (gethash instrument markets) agents)
               ))
    markets))
;; (time (get-agents-all))

(def (function d) get-trades (&optional limit offset segment)
  (if limit
      (conn (query (:select
                    '*
                    :from
                    (:as (:select '*
                          (:as (:over (:row-number)
                                (:partition-by 'instrument 'timeframe
                                 :order-by
                                 (:desc 'creation-time)
                                 (:desc 'activation)))
                           :idx)
                          :from
                          (:as (:select 'patterns.instrument
                                'patterns.timeframe
                                'patterns.type
                                'trades.id
                                'trades.label
                                (:as (:* 'trades.creation-time 1000000) 'creation-time)
                                'trades.test-trades-won
                                'trades.test-trades-lost
                                'trades.test-avg-revenue
                                'trades.test-avg-activation
                                'trades.test-avg-return
                                'trades.test-total-return
                                'trades.tp
                                'trades.sl
                                'trades.activation
                                'trades.decision
                                'trades.result
                                'trades.entry-price
                                'trades.entry-time
                                :distinct-on 'trades.id
                                :from 'trades
                                :inner-join 'patterns-trades
                                :on (:= 'trades.id 'patterns-trades.trade-id)
                                :inner-join 'patterns
                                :on (:= 'patterns-trades.pattern-id 'patterns.id))
                           'full-results))
                     'idx-results)
                    :where (:<= 'idx '$1))
                   limit
                   :alists))
      (conn (query (:order-by (:select 'patterns.instrument
                               'patterns.timeframe
                               'patterns.type
                               'trades.id
                               'trades.label
                               (:as (:* 'trades.creation-time 1000000) 'creation-time)
                               'trades.test-trades-won
                               'trades.test-trades-lost
                               'trades.test-avg-revenue
                               'trades.test-avg-activation
                               'trades.test-avg-return
                               'trades.test-total-return
                               'trades.tp
                               'trades.sl
                               'trades.activation
                               'trades.decision
                               'trades.result
                               'trades.entry-price
                               'trades.entry-time
                               :distinct-on 'trades.id
                               :from 'trades
                               :inner-join 'patterns-trades
                               :on (:= 'trades.id 'patterns-trades.trade-id)
                               :inner-join 'patterns
                               :on (:= 'patterns-trades.pattern-id 'patterns.id))
                    'trades.id
                    (:desc 'trades.creation-time))
                   :alists))))
;; (get-trades)

(defparameter *trades-columns*
  '('patterns.instrument
    'patterns.timeframe
    'patterns.type
    'trades.id
    'trades.label
    (:as (:* 'trades.creation-time 1000000) 'creation-time)
    'trades.test-trades-won
    'trades.test-trades-lost
    'trades.test-avg-revenue
    'trades.test-avg-activation
    'trades.test-avg-return
    'trades.test-total-return
    'trades.tp
    'trades.sl
    'trades.activation
    'trades.decision
    'trades.result
    'trades.entry-price
    (:as (:* 'trades.entry-time 1000000) 'entry-time)))

(def (function d) -get-trades (strategy instrument timeframe)
  `(:order-by (:select ,@*trades-columns*
               ;; :distinct-on 'trades.id
               :from 'trades
               :inner-join 'patterns-trades
               :on (:= 'trades.id 'patterns-trades.trade-id)
               :inner-join 'patterns
               :on (:= 'patterns-trades.pattern-id 'patterns.id)
               :where (:and (:like 'trades.label ,(string-downcase (format nil "%~a%" strategy)))
                       ;; We'll process human strategies elsewhere, due to its difference in structure.
                       (:not (:like 'trades.label "human%"))
                       (:like 'patterns.instrument ,(string-upcase (format nil "%~a%" instrument)))
                       (:like 'patterns.timeframe ,(string-upcase (format nil "%~a%" timeframe)))))
    ;; 'trades.id
    (:desc 'trades.creation-time)))
;; (-get-trades "" "EUR_USD" "M15")
;; (get-trades-flat -1 0 "human")
;; (get-trades-flat -1 0 "hermes")

(def (function d) get-trades-by-ids (ids)
  (let ((ids (if (vectorp ids)
                 (loop for id across ids collect id)
                 ids)))
    (conn (query (sql-compile
                  `(:order-by (:select ,@*trades-columns*
                               ;; :distinct-on 'trades.id
                               :from 'trades
                               :inner-join 'patterns-trades
                               :on (:= 'trades.id 'patterns-trades.trade-id)
                               :inner-join 'patterns
                               :on (:= 'patterns-trades.pattern-id 'patterns.id)
                               :where (:in 'trades.id (:set ,@ids)))
                    ;; 'trades.id
                    (:desc 'trades.creation-time)))
                 :alists))))
;; (get-trades-by-ids #("184F9098-472B-4896-950B-8356AD3C0CC9" "8BA3540D-3FD9-4012-A368-DBA94DFE60CF"))

(def (function d) -make-human-trades-alist (human-trades)
  (apply #'nconc
         (loop for trade in human-trades
               collect (let ((run-return 0)
                             (run-revenue 0)
                             (run-won 0)
                             (run-lost 0))
                         (loop for revenue across (assoccess trade :test-revenues)
                               for entry-time across (assoccess trade :test-entry-times)
                               for exit-time across (assoccess trade :test-exit-times)
                               for entry-price across (assoccess trade :test-entry-prices)
                               for exit-price across (assoccess trade :test-exit-prices)
                               for tp across (assoccess trade :test-tps)
                               for sl across (assoccess trade :test-sls)
                               for return across (assoccess trade :test-returns)
                               collect (progn
                                         (if (plusp return) (incf run-won) (incf run-lost))
                                         (incf run-return return)
                                         (incf run-revenue revenue)
                                         `((:instrument . ,(assoccess trade :instrument)) (:timeframe . ,(assoccess trade :timeframe))
                                                                                          (:test-trades-won . ,run-won)
                                                                                          (:test-trades-lost . ,run-lost)
                                                                                          (:type . ,(assoccess trade :type)) (:id . :NULL)
                                                                                          (:decision . ,(if (plusp tp) :BUY :SELL))
                                                                                          (:result . ,(if (plusp return) (abs tp) (* (abs sl) -1)))
                                                                                          (:test-total-return . ,run-return)
                                                                                          (:creation-time . ,entry-time)
                                                                                          (:label . ,(assoccess trade :label))
                                                                                          (:test-avg-revenue . ,(/ run-revenue (+ run-won run-lost)))
                                                                                          (:test-avg-return . ,(/ run-return (+ run-won run-lost)))
                                                                                          (:test-avg-activation . 1)
                                                                                          (:activation . 1)
                                                                                          (:entry-time . ,entry-time) (:exit-time . ,exit-time)
                                                                                          (:entry-price . ,entry-price) (:exit-price . ,exit-price)
                                                                                          (:tp . ,tp) (:sl . ,sl))))))))
;; (time (length (-make-human-trades-alist "EUR_USD" "M15" (conn (query (:select '* :from 'trades :where (:like 'label "human%")) :alist)))))

(def (function d) get-trades-flat (&optional (limit -1) (offset 0) (strategy "")
                                  (instrument "") (timeframe "M15"))
  (let* ((with-human-p (when (or (string= strategy "human")
                                 (string= strategy ""))
                         t))
         (trades (conn (if (plusp limit)
                           (query (sql-compile `(:limit ,(-get-trades strategy instrument timeframe) '$1 '$2))
                                  limit
                                  offset
                                  :alists)
                           (query (sql-compile (-get-trades strategy instrument timeframe)) :alists))))
         (humans (when with-human-p
                   (-make-human-trades-alist (conn (query (:select '*
                                                           ;; 'patterns.instrument
                                                           ;; 'patterns.timeframe
                                                           ;; 'patterns.types
                                                           :from 'trades
                                                           :join 'patterns-trades
                                                           :on (:= 'trades.id 'patterns-trades.trade-id)
                                                           :join 'patterns
                                                           :on (:= 'patterns-trades.pattern-id 'patterns.id)
                                                           :where
                                                           (:and (:like 'label "human%")
                                                            (:like 'patterns.instrument (string-upcase (format nil "%~a%" instrument)))
                                                            (:like 'patterns.timeframe (string-upcase (format nil "%~a%" timeframe))))) :alists))))))
    (if with-human-p
        (let ((all (sort (append trades humans)
                         #'>
                         :key (lambda (trade)
                                (assoccess trade :creation-time)))))
          (if (plusp limit)
              (if (or (>= offset (length all))
                      (>= (+ offset limit) (length all))
                      (>= limit (length all)))
                  all
                  (subseq all offset (+ offset limit)))
              all))
        trades)))

(comment
 (dex:post "http://localhost:9000/v1/trades" :content (json:encode-json-to-string '((:ids . ("184F9098-472B-4896-950B-8356AD3C0CC9" "8BA3540D-3FD9-4012-A368-DBA94DFE60CF"))))
           :headers '((:content-type . "application/json"))))

;; (length (time (get-trades-flat -1 0 "human")))
;; (time (get-trades-flat 0 0 "" "EUR_GBP"))
;; (length (time (get-trades-flat)))
;; (loop for trade in (get-trades-flat -1 0) do (print (local-time:unix-to-timestamp (assoccess trade :creation-time))))

(def (function d) get-trades-grouped (&optional (limit 10))
  (conn (query
         (sql-compile
          `(:select '* :from
            (:as (:select '*
                  (:as (:over (:row-number)
                        (:partition-by 'instrument 'timeframe
                         :order-by
                         (:desc 'creation-time)
                         (:desc 'activation)))
                   :idx)
                  :from
                  (:as ,(-get-trades "" "" "") ;; TODO: This whole function is outdated I think.
                   'full-results))
             'idx-results)
            :where (:<= 'idx '$1)))
         limit
         :alists)))
;; (get-trades-grouped 10)

(def (function d) -get-trades-nested (limit)
  (conn (query (:select
                '*
                :from
                (:as (:select '*
                      (:as (:over (:row-number)
                            (:partition-by 'instrument 'timeframe 'decision
                             :order-by (:desc 'creation-time)))
                       :idx)
                      :from
                      (:as (:select 'patterns.instrument
                            'patterns.timeframe
                            'patterns.type
                            'trades.id
                            'trades.label
                            'trades.creation-time
                            'trades.test-trades-won
                            'trades.test-trades-lost
                            'trades.test-avg-revenue
                            'trades.test-avg-activation
                            'trades.test-avg-return
                            'trades.test-total-return
                            'trades.tp
                            'trades.sl
                            'trades.activation
                            'trades.decision
                            'trades.entry-price
                            'trades.entry-time
                            'trades.result
                            :distinct-on 'trades.id
                            :from 'trades
                            :inner-join 'patterns-trades
                            :on (:= 'trades.id 'patterns-trades.trade-id)
                            :inner-join 'patterns
                            :on (:= 'patterns-trades.pattern-id 'patterns.id))
                       'full-results))
                 'idx-results)
                :where (:and (:<= 'idx '$1)
                        (:in 'creation-time (:select (:max 'trades.creation-time)
                                             :from 'trades
                                             :inner-join 'patterns-trades
                                             :on (:= 'trades.id 'patterns-trades.trade-id)
                                             :inner-join 'patterns
                                             :on (:= 'patterns-trades.pattern-id 'patterns.id)
                                             :group-by 'patterns.instrument))))
               limit
               :alists)))
;; (length (-get-trades-nested 20))

(def (function d) get-trades-nested (limit)
  (let ((trades (-get-trades-nested limit))
        (result))
    (loop for instrument in hscom.hsage:*instruments*
          do (let ((trades (remove-if-not (lambda (elt)
                                            (string= elt (format nil "~a" instrument)))
                                          trades
                                          :key (lambda (elt) (assoccess elt :instrument))))
                   (bullish)
                   (bearish))
               ;; Separating trades into bearish and bullish.
               (loop for trade in trades
                     do (if (plusp (assoccess trade :tp))
                            (push trade bullish)
                            (push trade bearish)))
               ;; Creating nests with top activated and rest.
               (let ((rbullish (sort bullish #'> :key (lambda (elt) (assoccess elt hscom.hsage:*trades-sort-by*))))
                     (rbearish (sort bearish #'> :key (lambda (elt) (assoccess elt hscom.hsage:*trades-sort-by*)))))
                 (push `((:instrument . ,(format nil "~a" instrument))
                         (:bullish . ,rbullish)
                         (:bearish . ,rbearish))
                       result)
                 ;; (when (first rbullish)
                 ;;   (push rbullish result)
                 ;;   ;; (push `((:first . ,(first rbullish))
                 ;;   ;; 	   (:rest . ,(rest rbullish)))
                 ;;   ;; 	 result)
                 ;;   )
                 ;; (when (first rbearish)
                 ;;   (push rbearish result)
                 ;;   ;; (push `((:first . ,(first rbearish))
                 ;;   ;; 	   (:rest . ,(rest rbearish)))
                 ;;   ;; 	 result)
                 ;;   )
                 )))
    (nreverse result)))
;; (length (get-trades-nested 20))

(def (function d) describe-trades (&optional limit filter-fn)
  (let* ((trades (remove-if-not filter-fn (get-trades limit)))
         ;; (trades-won (loop for trade in trades
         ;; 		   summing (assoccess trade :test-trades-won)))
         ;; (trades-lost (loop for trade in trades
         ;; 		    summing (assoccess trade :test-trades-lost)))
         ;; (total-return (loop for trade in trades
         ;; 		     summing (assoccess trade :test-total-return)))
         (test-trades-count 0)
         )
    (when trades
      ;; (format t "Total trades won: ~a. Total trades lost: ~a. Total trades: ~a. ~%Total return: ~a. Avg return: ~a.~%~%"
      ;; 	      trades-won
      ;; 	      trades-lost
      ;; 	      (+ trades-won trades-lost)
      ;; 	      total-return
      ;; 	      (/ total-return (+ trades-won trades-lost)))
      (values
       (/ (loop for trade in trades
                ;; when (and (not (eq (assoccess trade :result) :null))
                ;; 		 ;; (not (string= (assoccess trade :instrument) "USD_CNH"))
                ;; 		 )
                ;; summing (to-pips
                ;; 	  (assoccess trade :instrument)
                ;; 	  (assoccess trade :result))
                summing (progn
                          (incf test-trades-count (+ (assoccess trade :test-trades-won)
                                                     (assoccess trade :test-trades-lost)))
                          (to-pips
                           (assoccess trade :instrument)
                           (assoccess trade :test-avg-return)))
                )
          (length trades))
       test-trades-count)
      ;; (loop for trade in trades
      ;; 	    do (format t "market: :~a, result: ~a, test-total-return: ~5$, test-trades-won: ~a, test-trades-lost: ~a, rr: ~a~%"
      ;; 		       (assoccess trade :instrument)
      ;; 		       (assoccess trade :result)
      ;; 		       (assoccess trade :test-total-return)
      ;; 		       (assoccess trade :test-trades-won)
      ;; 		       (assoccess trade :test-trades-lost)
      ;; 		       (format-rr (assoccess trade :sl)
      ;; 				  (assoccess trade :tp))))
      )))
;; (get-trades 1)
;; (describe-trades 1000 (lambda (trade) (> (assoccess trade :activation) 0.0)))
;; (describe-trades nil (lambda (trade) t))

(def (function d) alist-keys (alist)
  (loop for item in alist collect (car item)))
;; (alist-keys (car (prepare-agents-properties (get-agents-some :AUD_USD hsage.config:*train-tf* '(:bullish)))))

(def (function d) alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-values (car (prepare-agents-properties (get-agents-some :EUR_USD hsage.config:*train-tf* '(:bullish)))))

(def (function d) describe-agents ()
  (with-open-stream (s (make-string-output-stream))
    (format s "<h3>AGENTS POOL.</h3><hr/>")
    (loop for instrument in hscom.hsage:*forex*
          do (loop for types in '((:single))
                   do (let* ((agents-props (prepare-agents-properties (get-agents-some instrument hscom.hsage:*train-tf* types)))
                             (agents-count (get-agents-count instrument hscom.hsage:*train-tf* types))
                             (vals (loop for agent-props in agents-props
                                         collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
                                                       (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
                                                   (append (alist-values agent-props)
                                                           (list (format-rr avg-sl avg-tp)))))))
                        (when (> (length agents-props) 0)
                          (format s "<h4>~a (~a, ~a)</h4>" instrument (car types) agents-count)
                          (format s "<pre>")
                          (format-table s vals :column-label (append (mapcar #'symbol-name (alist-keys (car agents-props)))
                                                                     '("R/R")))
                          (format s "</pre><hr/>")))))
    (get-output-stream-string s)))
;; (describe-agents)

(comment
 (let ((step 0.1))
   (loop for instrument in hscom.hsage:*instruments*
         do (progn
              (format t "~a~%" instrument)
              (loop for act from 0 to 1 by step
                    do (multiple-value-bind (avg len)
                           (describe-trades 1000 (lambda (trade) (and (not (eq (assoccess trade :activation) :null))
                                                                      (string= (assoccess trade :instrument)
                                                                               (format nil "~a" instrument))
                                                                      ;; (> (abs (/ (assoccess trade :tp)
                                                                      ;; 		 (assoccess trade :sl)))
                                                                      ;; 	 (+ 1 act))
                                                                      ;; (< (abs (/ (assoccess trade :tp)
                                                                      ;; 		 (assoccess trade :sl)))
                                                                      ;; 	 (+ 1 act step))
                                                                      (> (assoccess trade :test-avg-activation) act)
                                                                      (< (assoccess trade :test-avg-activation) (+ act step))
                                                                      )))
                         (format t "~$: ~a: ~a~%"
                                 act
                                 len
                                 avg)))
              (format t "~%")
              )))
 )

(def (function d) analysis (&key (granularity 10) (return-results-p nil) (label ""))
  (let* ((trades (conn (query (:select 'trades.creation-time
                               'trades.test-trades-won
                               'trades.test-trades-lost
                               'trades.test-avg-revenue
                               'trades.test-avg-activation
                               'trades.test-avg-return
                               'trades.test-total-return
                               'trades.tp
                               'trades.sl
                               'trades.activation
                               'trades.decision
                               'trades.result
                               'trades.entry-price
                               'trades.entry-time
                               'trades.test-activations
                               'trades.test-revenues
                               'trades.test-returns
                               :from 'trades
                               :where (:= 'label label))
                              :alists))))
    (when trades
      (let* ((acts (flatten (loop for trade in trades
                                  collect (coerce (assoccess trade :test-activations) 'list))))
             (revs (flatten (loop for trade in trades
                                  collect (coerce (assoccess trade :test-revenues) 'list))))
             (rets (flatten (loop for trade in trades
                                  collect (coerce (assoccess trade :test-returns) 'list)))))
        (when (and acts revs rets (> (length acts) granularity))
          (unless return-results-p
            (format t "# of trades: ~a~%~%" (length acts)))
          ;; Uniform distribution returns.
          (let ((step (floor (/ (length acts) granularity)))
                (sorted-acts (hsage.utils:sorted-indexes acts #'<)))
            (unless return-results-p
              (format t "Competence Mean-Return SD-Return Mean-Revenue SD-Revenue Mean-Wins SD-Wins Precision Wins Losses Max-Returns Min-Returns n~%"))
            (let ((results (loop for ceiling from 0 below (- (length acts) step) by step
                                 collect (let ((filtered-rets (loop for i from ceiling below (+ ceiling step)
                                                                    collect (nth (position i sorted-acts) rets)))
                                               (filtered-revs (loop for i from ceiling below (+ ceiling step)
                                                                    collect (nth (position i sorted-acts) revs))))
                                           (list (nth (position ceiling sorted-acts) acts)
                                                 (nth (position (+ ceiling step) sorted-acts) acts)
                                                 (if filtered-rets
                                                     (mean filtered-rets)
                                                     0.0)
                                                 (if filtered-rets
                                                     (standard-deviation filtered-rets)
                                                     0.0)
                                                 (if filtered-revs
                                                     (mean filtered-revs)
                                                     0.0)
                                                 (if filtered-revs
                                                     (standard-deviation filtered-revs)
                                                     0.0)
                                                 (if filtered-rets
                                                     (if (remove-if-not #'plusp filtered-rets)
                                                         (mean (remove-if-not #'plusp filtered-rets))
                                                         0.0)
                                                     0.0)
                                                 (if filtered-rets
                                                     (if (remove-if-not #'plusp filtered-rets)
                                                         (standard-deviation (remove-if-not #'plusp filtered-rets))
                                                         0.0)
                                                     0.0)
                                                 (if filtered-rets
                                                     (* 100
                                                        (/ (length (remove-if-not #'plusp filtered-rets))
                                                           (+ (length (remove-if-not #'plusp filtered-rets))
                                                              (length (remove-if-not #'minusp filtered-rets)))))
                                                     0.0)
                                                 (if filtered-rets
                                                     (length (remove-if-not #'plusp filtered-rets))
                                                     0.0)
                                                 (if filtered-rets
                                                     (length (remove-if-not #'minusp filtered-rets))
                                                     0.0)
                                                 (if filtered-rets
                                                     (apply #'max filtered-rets)
                                                     0.0)
                                                 (if filtered-rets
                                                     (apply #'min filtered-rets)
                                                     0.0)
                                                 (if filtered-rets
                                                     (length filtered-rets)
                                                     0.0))
                                           ))))
              (if return-results-p
                  results
                  (loop for result in results
                        do (apply #'format t "~2$-~2$ ~5$ ~5$ ~5$ ~5$ ~5$ ~5$ ~2$ ~a ~a ~5$ ~a ~a~%" result))))
            ))))))
;; (analysis :label "hermes.consensus-1" :granularity 20)
;; (analysis :label (format nil "hermes.consensus-~a" hscom.hsage:*consensus-threshold*) :granularity 10)

;; Plots
;; Per market comparisons
;; First quadrant, second quadrant, second half, etc.
;; All together per market, all together
;; Table with means and standard deviations. Hypothesis testing.

(def (function d) hypothesis-test (&key (numerator 0) (denominator 100) (granularity 100))
  (let* ((label hscom.hsage:*consensus-threshold*)
         (no-results (analysis :granularity granularity :label "hermes.consensus-1" :return-results-p t))
         (yes-results (analysis :granularity granularity :label (format nil "hermes.consensus-~a" label) :return-results-p t)))
    (when (and no-results yes-results)
      (let ((n1 (reduce #'+ (mapcar #'last-elt (subseq no-results (* numerator (ceiling (/ (length no-results) denominator)))))))
            (mu1 (mean (mapcar #'third (subseq no-results (* numerator (ceiling (/ (length no-results) denominator)))))))
            (sd1 (mean (mapcar #'fourth (subseq no-results (* numerator (ceiling (/ (length no-results) denominator)))))))
            (n2 (reduce #'+ (mapcar #'last-elt (subseq yes-results (* numerator (ceiling (/ (length yes-results) denominator)))))))
            (mu2 (mean (mapcar #'third (subseq yes-results (* numerator (ceiling (/ (length yes-results) denominator)))))))
            (sd2 (mean (mapcar #'fourth (subseq yes-results (* numerator (ceiling (/ (length yes-results) denominator))))))))
        (when (and (> sd1 0) (> sd2 0))
          (format t "NO CONSENSUS~%")
          (format t "n: ~a~%" n1)
          (format t "Mean: ~a~%" mu1)
          (format t "SD: ~a~%" sd1)
          (format t "~%CONSENSUS~%")
          (format t "n: ~a~%" n2)
          (format t "Mean: ~a~%" mu2)
          (format t "SD: ~a~%" sd2)
          (multiple-value-bind (significance t-statistic)
              (hsage.stat:t-test-two-sample mu1 sd1 n1 mu2 sd2 n2)
            (format t "~%significance: ~a~%" significance)
            (format t "t-statistic: ~a~%~%" t-statistic)))))
    )
  )
;; (hypothesis-test)

;; (length (aref (antecedents (first (get-agents-some :AUD_USD :M15 '((:SINGLE))))) 0))

;; (get-agents-count :AUD_USD :M15 '((:SINGLE)))
;; (get-agents-count :USD_CHF :M15 '((:SINGLE)))
;; (get-agents-count :EUR_USD :M15 '((:SINGLE)))


;; (mean (mapcar #'third (analysis :granularity 10 :label "hermes.consensus-1" :return-results-p t)))
;; (mean (mapcar #'third (analysis :granularity 10 :label "hermes.consensus-5" :return-results-p t)))
;; (mean (mapcar #'fourth (analysis :granularity 10 :label "hermes.consensus-1" :return-results-p t)))
;; (mean (mapcar #'fourth (analysis :granularity 10 :label "hermes.consensus-5" :return-results-p t)))

(comment
 (let ((granularity 10)
       (lbls `(1 ,hscom.hsage:*consensus-threshold*)))
   (plot-xy
    (loop for label in lbls
          collect (loop for result in (analysis :granularity granularity :return-results-p t :label (format nil "hermes.consensus-~a" label))
                        collect (format nil "~2$-~2$" (first result) (second result))))
    (loop for label in lbls
          collect (loop for result in (analysis :granularity granularity :return-results-p t :label (format nil "hermes.consensus-~a" label))
                        collect (format nil "~5$" (nth 2 result))))
    ))

 ;; Single mean return.
 (let ((granularity 10)
       (label 1))
   (plot-xy
    (list (loop for result in (analysis :granularity granularity :return-results-p t :label (format nil "hermes.consensus-~a" label))
                collect (format nil "~2$-~2$" (first result) (second result))))
    (list (loop for result in (analysis :granularity granularity :return-results-p t :label (format nil "hermes.consensus-~a" label))
                collect (format nil "~5$" (nth 2 result))))))

 ;; SD-Return.
 (let ((granularity 10))
   (plot-xy
    (loop for result in (analysis granularity t)
          collect (format nil "~2$-~2$" (first result) (second result)))
    (loop for result in (analysis granularity t)
          collect (format nil "~5$" (nth 3 result)))))

 ;; Precision.
 (let ((granularity 10))
   (plot-xy
    (loop for result in (analysis granularity t)
          collect (format nil "~2$-~2$" (first result) (second result)))
    (loop for result in (analysis granularity t)
          collect (format nil "~5$" (nth 6 result)))))
 )

(comment
 (loop for instrument in hscom.hsage:*instruments*
       do (progn
            (format t "~a: " instrument)
            (loop for type in '((:single))
                  do (format t "~a, " (length (get-agents-some instrument hscom.hsage:*train-tf* type))))
            (format t "~%"))
       finally (describe-trades 300 (lambda (trade) (and (not (eq (assoccess trade :activation) :null))
                                                         (> (assoccess trade :activation) 0.8)))))
 )

(def (function d) get-global-revenue (&key from to)
  (let ((trades (conn (if (and from to)
                          (query (:order-by (:select '*
                                             :from (:as (:order-by (:select
                                                                    'trades.result
                                                                    'trades.decision
                                                                    'trades.creation-time
                                                                    'patterns.*
                                                                    :distinct-on 'trades.id
                                                                    :from 'trades
                                                                    :inner-join 'patterns-trades
                                                                    :on (:= 'trades.id 'patterns-trades.trade-id)
                                                                    :inner-join 'patterns
                                                                    :on (:= 'patterns.id 'patterns-trades.pattern-id)
                                                                    :where (:and (:not (:is-null 'trades.result))
                                                                            (:>= 'creation-time from)
                                                                            ;; (:not (:= 'patterns.instrument "USD_CNH"))
                                                                            ;; (:not (:= 'patterns.type "STAGNATED"))
                                                                            ;; (:= 'patterns.type "STAGNATED")
                                                                            ;; (:= 'patterns.instrument "USD_CNH")
                                                                            (:<= 'creation-time to)
                                                                            ))
                                                         'trades.id)
                                                    'results))
                                  (:desc 'creation-time))
                                 :alists)
                          (query (:order-by (:select '*
                                             :from (:as (:order-by (:select
                                                                    'trades.result
                                                                    'trades.decision
                                                                    'trades.creation-time
                                                                    'patterns.*
                                                                    :distinct-on 'trades.id
                                                                    :from 'trades
                                                                    :inner-join 'patterns-trades
                                                                    :on (:= 'trades.id 'patterns-trades.trade-id)
                                                                    :inner-join 'patterns
                                                                    :on (:= 'patterns.id 'patterns-trades.pattern-id)
                                                                    :where (:and (:not (:is-null 'trades.result))
                                                                            ;; (:>= 'creation-time from)
                                                                            ;; (:not (:= 'patterns.instrument "USD_CNH"))
                                                                            ;; (:not (:= 'patterns.type "STAGNATED"))
                                                                            ;; (:= 'patterns.type "STAGNATED")
                                                                            ;; (:= 'patterns.instrument "USD_CNH")
                                                                            ;; (:<= 'creation-time to)
                                                                            ))
                                                         'trades.id)
                                                    'results))
                                  (:desc 'creation-time))
                                 :alists)
                          ))))
    (loop for trade in trades
          summing (let ((instrument (make-keyword (assoccess trade :instrument))))
                    ;; (if (or (and (string= (assoccess trade :decision) "SELL")
                    ;; 		 (string= (assoccess trade :type) "BEARISH"))
                    ;; 	    (and (string= (assoccess trade :decision) "BUY")
                    ;; 		 (string= (assoccess trade :type) "BULLISH")))
                    ;; 	(to-pips instrument (assoccess trade :result))
                    ;; 	0)
                    (to-pips instrument (assoccess trade :result))))))
;; (get-global-revenue)
;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 3 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 2000 :day)))
;; (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 4 :day)))

;; Running hours.
;; (loop for i from 0 below 72 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 0 :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :hour)))))

;; Per day.
;; (loop for i from 0 below 10 do (format t "~a: ~a~%" i (get-global-revenue :to (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) i :day)) :from (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) (1+ i) :day)))))

(def (function d) -get-trade-time (trade)
  "Used in `-validate-trades`."
  (let ((entry-time (assoccess trade :entry-time))
        (creation-time (assoccess trade :creation-time)))
    (ceiling (* (if hscom.all:*is-production*
                    ;; The exact time when the trade got created, e.g 4:33 PM.
                    creation-time
                    (if (not (equal entry-time :null))
                        ;; The time of the last traded candle in the testing dataset.
                        ;; This time will be a rounded hour (if using hours), e.g. 4:00 PM.
                        entry-time
                        creation-time))
                1000000))))

(def (function d) -validate-trades (instrument trades older-than)
  "We use `older-than` to determine what trades to ignore due to possible lack of prices for validation."
  (let* ((day (local-time:timestamp-day-of-week (local-time:now)))
         (older-than (cond ((= day 0) (+ older-than 2)) ;; it's Sunday, add 2 days
                           ((= day 1) (+ older-than 1)) ;; it's Monday, add 1 day
                           (t older-than) ;; default, leave unchanged.
                           )))
    (when (> (length trades) 0)
      ($log $info (format nil "Trying to validate ~a trades." (length trades)))
      (let* ((oldest (first (sort (copy-sequence 'list trades) #'< :key #'-get-trade-time)))
             ;; (newest (first (sort (copy-sequence 'list trades) #'> :key #'-get-trade-time)))
             (rates (get-rates-range-big instrument hscom.hsage:*validation-timeframe*
                                         (-get-trade-time oldest)
                                         ;; (-get-trade-time newest)
                                         (* (local-time:timestamp-to-unix (local-time:now)) 1000000)
                                         )))
        (loop for trade in trades
              do (let* ((idx (position (-get-trade-time trade) rates :test #'<= :key (lambda (rate) (read-from-string (assoccess rate :time))))))
                   (when idx
                     (let ((sub-rates (subseq rates idx))
                           (from-timestamp (local-time:unix-to-timestamp (ceiling (/ (-get-trade-time trade) 1000000)))))
                       (when (or (not hscom.all:*is-production*)
                                 (local-time:timestamp< from-timestamp
                                                        (local-time:timestamp- (local-time:now) older-than :day)))
                         (multiple-value-bind (result exit-time)
                             (get-trade-result (assoccess trade :entry-price)
                                               (assoccess trade :tp)
                                               (assoccess trade :sl)
                                               sub-rates)
                           (when result
                             ($log $info (format nil "Result obtained for trade: ~a." (assoccess trade :id)))
                             (conn
                              (let ((dao (get-dao 'trade (assoccess trade :id))))
                                (setf (slot-value dao 'result) (if result result :NULL))
                                (setf (slot-value dao 'exit-time) (if exit-time (/ (read-from-string exit-time) 1000000) :NULL))
                                (update-dao dao))))
                           ))))))
        (sleep 1)))))

(def (function d) re-validate-trades (&optional (older-than 0) (last-n-days 30))
  (loop for instrument in hscom.hsage:*instruments*
        do (let ((trades (conn (query (:order-by (:select '*
                                                  :from (:as (:order-by (:select 'trades.* 'patterns.*
                                                                         :distinct-on 'trades.id
                                                                         :from 'trades
                                                                         :inner-join 'patterns-trades
                                                                         :on (:= 'trades.id 'patterns-trades.trade-id)
                                                                         :inner-join 'patterns
                                                                         :on (:= 'patterns.id 'patterns-trades.pattern-id)
                                                                         :where (:and
                                                                                 (:= 'patterns.instrument (format nil "~a" instrument))
                                                                                 (:not (:like 'trades.label "human%"))
                                                                                 (:> :creation-time (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) last-n-days :day)))))
                                                              'trades.id)
                                                         'results))
                                       (:desc 'creation-time))
                                      :alists))))
             (-validate-trades instrument trades older-than))))
;; (time (re-validate-trades 0 5))

(def (function d) validate-trades (&optional (older-than 1))
  ;; TODO: Refactor this. We should be calling something like (SYNC-RATES '(:M1))
  ;; Syncing M1 rates.
  ;; (loop for instrument in hscom.hsage:*instruments*
  ;;       do (loop for timeframe in `(,hscom.hsage:*validation-timeframe*)
  ;;                do (sync-rates instrument timeframe)))
  (loop for instrument in hscom.hsage:*instruments*
        do (let ((trades (conn (query (:order-by (:select '*
                                                  :from (:as (:order-by (:select 'trades.* 'patterns.*
                                                                         :distinct-on 'trades.id
                                                                         :from 'trades
                                                                         :inner-join 'patterns-trades
                                                                         :on (:= 'trades.id 'patterns-trades.trade-id)
                                                                         :inner-join 'patterns
                                                                         :on (:= 'patterns.id 'patterns-trades.pattern-id)
                                                                         ;; :where (:not (:is-null 'trades.result))
                                                                         :where (:and
                                                                                 (:= 'patterns.instrument (format nil "~a" instrument))
                                                                                 (:is-null 'trades.result)))
                                                              'trades.id)
                                                         'results))
                                       (:desc 'creation-time))
                                      :alists))))
             (when trades
               (-validate-trades instrument trades older-than)))))
;; (validate-trades)

(def (function d) delete-trades (from to)
  (conn
   (with-transaction ()
     ;; Deleting patterns-trades.
     (execute (:delete-from 'patterns-trades
               :where (:in 'pattern-id
                       (:select 'id :from 'trades :where (:and (:>= 'creation-time from)
                                                          (:<= 'creation-time to))))))
     ;; Deleting the actual trades.
     (execute (:delete-from 'trades
               :where (:and (:>= 'creation-time from)
                       (:<= 'creation-time to)))))))

;; (delete-trades (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 5 :hour))
;; 	       (local-time:timestamp-to-unix (local-time:timestamp- (local-time:now) 4 :hour)))

;; (conn (query (:select (:count 'id) :from 'trades)))

(def (function d) get-trade-result (entry-price tp sl rates)
  (let ((low-type (if (plusp tp) :low-bid :low-ask))
        (high-type (if (plusp tp) :high-bid :high-ask)))
    (loop for rate in rates do
             (progn
               ;; Then it's a buy. Lose.
               (when (and (> tp 0) (< (- (assoccess rate low-type) entry-price) sl))
                 (return (values sl (assoccess rate :time))))
               ;; Then it's a sell. Lose.
               (when (and (< tp 0) (> (- (assoccess rate high-type) entry-price) sl))
                 (return (values (- sl) (assoccess rate :time))))
               ;; Then it's a buy. Win.
               (when (and (> tp 0) (> (- (assoccess rate high-type) entry-price) tp))
                 (return (values tp (assoccess rate :time))))
               ;; Then it's a sell. Win.
               (when (and (< tp 0) (< (- (assoccess rate low-type) entry-price) tp))
                 (return (values (abs tp) (assoccess rate :time))))
               ))))
;; (get-trade-result 0.72274 -0.0018100000000000893 0.0013900000000000026 )
