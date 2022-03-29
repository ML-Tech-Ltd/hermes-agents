(defpackage hermes-agents.trading
  (:use #:cl #:ciel #:postmodern #:hsage.log #:hscom.log #:hu.dwim.def)
  (:import-from #:defenum
                #:defenum)
  (:import-from #:alexandria
                #:copy-sequence
                #:make-keyword
                #:standard-deviation
                #:if-let)
  (:import-from #:serapeum
                #:string+)
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
                #:whole-reals-to-integers
                #:now)
  (:import-from #:hscom.hsage
                #:*fis-method*
                #:*min-pips-sl*
                #:*exhaust-rules-in-creation-dataset-p*
                #:*ignore-test-conditions-p*
                #:*lookahead*
                #:*lookbehind*
                #:*unique-point-p*
                #:*unique-count*
                #:*max-agents-count*
                #:*min-agents-count*
                #:*creation-dataset-size*
                #:*training-dataset-size*
                #:*testing-dataset-size*)
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
                #:get-rates-count-big
                #:get-rates-random-count-big
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
           #:optimization
           #:evaluate-trade
           #:insert-signal
           #:get-same-direction-outputs-idxs
           #:make-ifis
           #:evaluate-agent
           #:evaluate-agents
           #:test-agents
           #:add-agent
           #:sync-agents
           #:remove-agent
           #:eval-agent
           #:get-agent
           #:get-agents
           #:get-agent-by-id
           #:get-agents-count
           #:agents-to-alists
           #:test-most-activated-agents
           #:gen-agent
           #:gen-agents
           #:update-agent-fitnesses
           #:update-agents-fitnesses
           #:wipe-agents
           #:get-trade-result
           #:validate-trades
           #:re-validate-trades
           #:delete-signals
           #:get-strategy
           #:best-individual
           #:signal-strategy
           #:cache-agents-from-db
           #:uncache-agents-from-db
           #:retire-agents
           #:update-unique-datasets
           #:get-dataset
           #:get-unique-dataset-idxs)
  (:nicknames #:hsage.trading))
(in-package :hermes-agents.trading)

(ciel:enable-punch-syntax)

(def (special-variable) *agents-cache* (make-hash-table :test 'equal :synchronized t))
(def (special-variable) *datasets* (make-hash-table :test #'equal :synchronized t))
(def (special-variable) *unique-datasets* (make-hash-table :test #'equal :synchronized t))

(defclass <instrument> ()
  ((id :col-type int8 :initarg :id :accessor id :col-identity t)
   (name :col-type string :col-unique t :check (:<> 'name "") :initarg :name))
  (:metaclass postmodern:dao-class)
  (:table-name instruments)
  (:keys id))

(defclass <timeframe> ()
  ((id :col-type int8 :initarg :id :accessor id :col-identity t)
   (name :col-type string :col-unique t :check (:<> 'name "") :initarg :name))
  (:metaclass postmodern:dao-class)
  (:table-name timeframes)
  (:keys id))

(defclass <design> ()
  ((id :col-type int8 :initarg :id :accessor id :col-identity t)
   (name :col-type string :col-unique t :check (:<> 'name "") :initarg :name))
  (:metaclass postmodern:dao-class)
  (:table-name designs)
  (:keys id))

(defclass <environment> ()
  ((id :col-type int8 :initarg :id :accessor id :col-identity t)
   (name :col-type string :col-unique t :check (:<> 'name "") :initarg :name))
  (:metaclass postmodern:dao-class)
  (:table-name environments)
  (:keys id))

(defclass <strategy> ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (instrument-id :col-type int8 :initarg :instrument-id :col-references ((instruments id)))
   (timeframe-id :col-type int8 :initarg :timeframe-id :col-references ((timeframe id)))
   (method-id :col-type int8 :initarg :method-id :col-references ((strategies id)))
   (name :col-type string :initarg :name :accessor name)
   (parameters :col-type string :initarg :parameters :accessor parameters)
   (population :col-type float[] :initarg :population :accessor population)
   (best-individual :col-type float[] :initarg :best-individual :accessor best-individual)
   (iterations :col-type bigint :initarg :iterations :accessor iterations))
  (:metaclass postmodern:dao-class)
  (:table-name strategies)
  (:keys id))

(defclass <metrics> ()
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
   (trades-lost :col-type double-float :initarg :trades-lost :accessor trades-lost))
  (:metaclass postmodern:dao-class)
  (:table-name metrics)
  (:keys id))

(defclass <metric-strategy> ()
  ((metrics-id :col-type string :initarg :metrics-id :accessor metrics-id)
   (strategy-id :col-type string :initarg :strategy-id :accessor strategy-id)
   (environment-id :col-type string :initarg :strategy-id :accessor environment-id)
   (timestamp :col-type int8 :initarg :timestamp :accessor timestamp))
  (:metaclass postmodern:dao-class)
  (:table-name metrics-strategies)
  (:keys metrics-id strategy-id environment-id))

(defclass <trade> ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (metrics-id :col-type (or db-null string) :initarg :metrics-id :col-references ((metrics id)) :accessor metrics-id)
   (decision :col-type string :initarg :decision :accessor decision)
   (return :col-type (or db-null double-float) :initarg :return :accessor .return)
   (revenue :col-type (or db-null double-float) :initarg :revenue :accessor revenue)
   (tp :col-type double-float :initarg :tp :accessor tp)
   (sl :col-type double-float :initarg :sl :accessor sl)
   (activation :col-type double-float :initarg :activation :accessor activation)
   (entry-price :col-type double-float :initarg :entry-price :accessor entry-price)
   (exit-price :col-type double-float :initarg :exit-price :accessor exit-price)
   (entry-time :col-type int8 :initarg :entry-time :accessor entry-time)
   (exit-time :col-type int8 :initarg :exit-time :accessor exit-time))
  (:metaclass postmodern:dao-class)
  (:table-name trades)
  (:keys id))

(defclass <signal> ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (trade-id :col-type string :initarg :trade-id :col-references ((trades id)))
   (strategy-id :col-type string :initarg :strategy-id :col-references ((strategies id)))
   (training-metrics-id :col-type string :initarg :training-metrics-id :col-references ((metrics id)))
   (testing-metrics-id :col-type string :initarg :testing-metrics-id :col-references ((metrics id)))
   (timestamp :col-type int8 :initarg :timestamp :accessor timestamp))
  (:metaclass postmodern:dao-class)
  (:table-name signals)
  (:keys id))

(defclass <agent> ()
  ((id :col-type string :initform (format nil "~a" (uuid:make-v4-uuid)) :initarg :id :accessor id)
   (strategy-id :col-type string :initarg :strategy-id :col-references ((strategies id)) :accessor strategy-id)
   (metrics-id :col-type string :initarg :metrics-id :col-references ((metrics id)) :accessor metrics-id)
   (retired :col-type (or db-null boolean) :initarg :retired :initform nil :accessor retired)
   (lookahead :col-type integer :initarg :lookahead :accessor lookahead)
   (lookbehind :col-type integer :initarg :lookbehind :accessor lookbehind)
   (perceptions-count :col-type integer :initarg :perceptions-count :accessor perceptions-count)
   (rules-count :col-type integer :initarg :rules-count :accessor rules-count)
   (perception-fns :col-type string :initarg :perception-fns :accessor perception-fns)
   (antecedents :col-type string :initarg :antecedents :accessor antecedents)
   (consequents :col-type string :initarg :consequents :accessor consequents)
   (creation-begin-time :col-type (or db-null int8) :initarg :creation-begin-time :initform :null)
   (creation-end-time :col-type (or db-null int8) :initarg :creation-end-time :initform :null)
   (.metrics :initarg :.metrics :accessor .metrics))
  (:metaclass postmodern:dao-class)
  (:table-name agents)
  (:keys id))

(def (function d) determine-decision (tp sl)
  (if (or (= sl 0) (= tp 0))
      "HOLD"
      (if (> tp 0)
          "BUY"
          "SELL")))

(def (function d) insert-signal (strategy-id instrument timeframe training-fitnesses testing-fitnesses tp sl activation)
  (bind ((dataset (get-dataset instrument timeframe :testing))
         (training-metrics (insert-metrics training-fitnesses))
         (testing-metrics (insert-metrics testing-fitnesses))
         (decision (determine-decision tp sl))
         (entry-time (assoccess (last-elt dataset) :time))
         (entry-price (if (> tp 0)
                          (hsinp.rates:->close-ask (last-elt dataset))
                          (hsinp.rates:->close-bid (last-elt dataset))))
         (trade (insert-trade decision 0 0 tp sl activation
                              entry-price entry-time 0 0)))
    (insert-trades-from-fitnesses training-fitnesses training-metrics)
    (insert-trades-from-fitnesses testing-fitnesses testing-metrics)
    (conn (make-dao '<signal>
                    :trade-id (id trade)
                    :strategy-id strategy-id
                    :training-metrics-id (id training-metrics)
                    :testing-metrics-id (id testing-metrics)
                    :timestamp (now)))))

(comment
  (test-agents :AUD_USD :M15)
  (evaluate-agents :AUD_USD :M15 :testing)
  (length (get-unique-dataset-idxs :AUD_USD :M15 :testing))
  (length (get-dataset :AUD_USD :M15)))

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

(def (function d) -test-conditions (instrument tp sl test-fitnesses &key (hybridp nil))
  (or (and *ignore-test-conditions-p* ($log $debug "TEST COND. Ignoring test conditions."))
      (and
       (or (/= tp 0) ($log $debug "TEST COND. (/= tp 0)."))
       ;; (if (not (eq instrument :USD_CNH)) (< (assoccess prediction :tp) 100) t)
       (or (> (abs tp) (abs sl)) ($log $debug "TEST COND. (> (abs tp) (abs sl))."))
       (or (/= sl 0) ($log $debug "TEST COND. (/= sl 0)."))
       (or (< (* tp sl) 0) ($log $debug "TEST COND. (< (* tp sl) 0)."))
       (if hybridp
           t
           (and
            (or (> (abs (/ tp sl))
                    hscom.hsage:*agents-min-rr-signal*)
                 ($log $debug "TEST COND. (> (abs (/ tp sl)) *agents-min-rr-signal*)."))
            (or (> (abs (to-pips instrument sl)) *min-pips-sl*)
                 ($log $debug (string+ "TEST COND. (> (abs (to-pips instrument sl)) *min-pips-sl*)." (to-pips instrument sl))))
            (or (/= (assoccess test-fitnesses :trades-won) 0)
                 ($log $debug "TEST COND. (/= (assoccess test-fitnesses :trades-won) 0)."))))
       ;; (< (to-pips instrument (abs sl)) 20)
       )))

(def (function d) insert-trades-from-fitnesses (fitnesses metrics)
  (loop for return in (assoccess fitnesses :returns)
        for revenue in (assoccess fitnesses :revenues)
        for entry-time in (assoccess fitnesses :entry-times)
        for exit-time in (assoccess fitnesses :exit-times)
        for entry-price in (assoccess fitnesses :entry-prices)
        for exit-price in (assoccess fitnesses :exit-prices)
        for tp in (assoccess fitnesses :tps)
        for sl in (assoccess fitnesses :sls)
        for activation in (assoccess fitnesses :activations)
        do (insert-trade (determine-decision tp sl) return revenue tp sl
                         activation entry-price entry-time exit-price exit-time metrics)))

(def (function d) insert-trade (decision return revenue tp sl activation entry-price
                                         entry-time exit-price exit-time
                                         &optional metrics)
  "
INSERT-TRADE creates a new trade on the database. TRADE is an alist containing
all the data for the trade. METRICS is an object of type <METRICS>. INSERT-TRADE
returns an object of type <TRADE>.
"
  (conn (make-dao '<trade>
                  :metrics-id (if metrics (id metrics) :null)
                  :decision decision
                  :return return
                  :revenue revenue
                  :tp tp
                  :sl sl
                  :activation activation
                  :entry-price entry-price
                  :entry-time entry-time
                  :exit-price exit-price
                  :exit-time exit-time)))

(def (function d) update-metrics (metrics metrics-id)
  (conn
   (bind ((dao (get-dao '<metrics> metrics-id)))
     (when dao
       (setf (slot-value dao 'begin-time) (assoccess-default metrics :begin-time -1))
       (setf (slot-value dao 'end-time) (assoccess-default metrics :end-time -1))
       (setf (slot-value dao 'dataset-size) (assoccess-default metrics :dataset-size -1))
       (setf (slot-value dao 'avg-revenue) (assoccess-default metrics :avg-revenue 0))
       (setf (slot-value dao 'stdev-revenue) (assoccess-default metrics :stdev-revenue -1))
       (setf (slot-value dao 'total-revenue) (assoccess-default metrics :total-revenue 0))
       (setf (slot-value dao 'avg-return) (assoccess-default metrics :avg-return 0))
       (setf (slot-value dao 'total-return) (assoccess-default metrics :total-return 0))
       (setf (slot-value dao 'avg-max-pos) (assoccess-default metrics :avg-max-pos 0))
       (setf (slot-value dao 'stdev-max-pos) (assoccess-default metrics :stdev-max-pos -1))
       (setf (slot-value dao 'avg-max-neg) (assoccess-default metrics :avg-max-neg 0))
       (setf (slot-value dao 'stdev-max-neg) (assoccess-default metrics :stdev-max-neg -1))
       (setf (slot-value dao 'avg-tp) (assoccess-default metrics :avg-tp 0))
       (setf (slot-value dao 'stdev-tp) (assoccess-default metrics :stdev-tp -1))
       (setf (slot-value dao 'avg-sl) (assoccess-default metrics :avg-sl 0))
       (setf (slot-value dao 'stdev-sl) (assoccess-default metrics :stdev-sl -1))
       (setf (slot-value dao 'avg-activation) (assoccess-default metrics :avg-activation 0))
       (setf (slot-value dao 'stdev-activation) (assoccess-default metrics :stdev-activation -1))
       (setf (slot-value dao 'max-tp) (assoccess-default metrics :max-tp 0))
       (setf (slot-value dao 'min-tp) (assoccess-default metrics :min-tp 0))
       (setf (slot-value dao 'max-sl) (assoccess-default metrics :max-sl 0))
       (setf (slot-value dao 'min-sl) (assoccess-default metrics :min-sl 0))
       (setf (slot-value dao 'trades-won) (assoccess-default metrics :trades-won -1))
       (setf (slot-value dao 'trades-lost) (assoccess-default metrics :trades-lost -1))
       (update-dao dao)))))

(def (function d) insert-metrics (metrics)
  (conn (make-dao '<metrics>
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
                  :trades-lost (assoccess-default metrics :trades-lost -1))))

;; Use this generalized function instead
(def (function d) get-strategy (instrument timeframe method &optional (name ""))
  (car (conn (query (:order-by
                     (:select 'strategies.* :from 'strategies
                       :join 'instruments
                       :on (:= 'instrument_id 'instruments.id)
                       :join 'timeframes
                       :on (:= 'timeframe_id 'timeframes.id)
                       :join 'methods
                       :on (:= 'method_id 'methods.id)
                       :where (:and
                               (:= 'instruments.name (string instrument))
                               (:= 'timeframes.name (string timeframe))
                               (:= 'methods.name (string method))
                               (:or (:= (string name) "")
                                    (:= 'strategies.name (string name)))))
                     (:desc 'iterations))
                    (:dao <strategy>)))))
;; (get-human-name (get-strategy :AUD_USD :M1 :HERMES))

(def (function d) get-hybrid (instrument timeframe name)
  (get-strategy instrument timeframe :human name))

(def (function d) get-hybrid-iterations (instrument timeframe name)
  (iterations (get-strategy instrument timeframe :human name)))
;; (get-hybrid-iterations :EUR_USD :M15 "human.rsi-stoch-macd")

(def (function d) insert-strategy (instrument timeframe method
                                              &key name parameters best-individual population iterations)
  (conn
   (query
    (sql-compile
     `(:insert-into 'strategies :set
        'instrument-id (:select 'id :from 'instruments :where (:= 'name ,(string+ instrument)))
        'timeframe-id (:select 'id :from 'timeframes :where (:= 'name ,(string+ timeframe)))
        'method-id (:select 'id :from 'methods :where (:= 'name ,(string+ method)))
        ,@(when name `('name ,name))
        ,@(when iterations `('iterations ,iterations))
        ,@(when parameters `('parameters ,parameters))
        ,@(when best-individual `('best-individual (:set ,best-individual)))
        ,@(when population `('population (:set ,population)))
        :returning '*))
    (:dao <strategy> :single))))
;; (insert-strategy :EUR_USD :M15 :HERMES :name "meow")
;; (insert-strategy :EUR_USD :M15 :HERMES :iterations 5 :best-individual #(5 5 100) :parameters "")

(def (function d) update-strategy (instrument timeframe method
                                              &key name parameters best-individual population iterations)
  (conn
   (query
    (sql-compile
     `(:update 'strategies :set
        ,@(when name `('name ,name))
        ,@(when iterations `('iterations (:+ 'iterations ,iterations)))
        ,@(when parameters `('parameters ,parameters))
        ,@(when best-individual `('best-individual (:set ,best-individual)))
        ,@(when population `('population (:set ,population)))
        :where (:and (:= 'instrument-id (:select 'id :from 'instruments :where (:= 'name ,(string+ instrument))))
                     (:= 'timeframe-id (:select 'id :from 'timeframes :where (:= 'name ,(string+ timeframe))))
                     (:= 'method-id (:select 'id :from 'methods :where (:= 'name ,(string+ method)))))
        :returning '*))
    (:dao <strategy> :single))))

(def (function d) insert-metrics-strategies (metrics strategy environment-name)
  (conn (query (:insert-into 'metrics-strategies :set
                 'metrics-id (id metrics)
                 'strategy-id (id strategy)
                 'environment-id (:select 'id :from 'environments :where (:= 'name (string+ environment-name)))
                 'timestamp (now)))))

(def (function d) insert-hybrid (instrument timeframe name environment iterations population best-individual metrics)
  (conn
   (bind ((metrics (insert-metrics metrics))
          (hybrid (update-strategy instrument timeframe :hybrid
                                   :name name
                                   :iterations iterations
                                   :population (if (vectorp population)
                                                   population
                                                   (apply #'vector population))
                                   :best-individual (if (vectorp best-individual)
                                                        best-individual
                                                        (apply #'vector best-individual)))))
     (insert-metrics-strategies metrics
                                hybrid
                                environment))))

(def (function d) gen-search-space (parameters ranges)
  (mapcar (lambda (parameter range)
            (cons parameter range))
          parameters
          ranges))

;; (gen-search-space (assoccess (first (hsper:get-human-strategies)) :parameters)
;;            (whole-reals-to-integers (best-individual (get-hybrid :EUR_USD :M15 "human.rsi-stoch-macd"))))

(def (function d) gen-population-from-genomes (search-space genomes genome-size parameters)
  (loop for i from 0 below (length genomes) by genome-size
        collect (bind ((values (whole-reals-to-integers (subseq genomes i (+ i genome-size))))
                       (gen-values (flatten (mapcar (lambda (gen value)
                                                      (list (make-keyword gen) value))
                                                    parameters
                                                    values))))
                  (apply #'make-being search-space gen-values))))
;; (gen-population-from-genomes )

(def (function d) get-dataset (instrument timeframe &optional environment)
  (bind ((dataset (gethash (list instrument timeframe) *datasets*)))
    (if environment
        (cond ((eq environment :creation) (subseq dataset
                                                  0
                                                  *creation-dataset-size*))
              ((eq environment :training) (subseq dataset
                                                  *creation-dataset-size*
                                                  (+ *creation-dataset-size* *training-dataset-size*)))
              ((eq environment :testing) (subseq dataset
                                                  (+ *creation-dataset-size* *training-dataset-size*)))
              (t (error "Undefined environment.")))
        dataset)))
;; (length (get-dataset :EUR_USD :M15))
(def (function d) get-unique-dataset-idxs (instrument timeframe environment)
  (gethash (list instrument timeframe environment) *unique-datasets*))
;; (length (get-unique-dataset-idxs :EUR_USD :M15 :testing))

(def (function d) update-unique-datasets ()
  "
UPDATE-UNIQUE-DATASETS updates the values in *UNIQUE-DATASETS* with
more recent unique datasets.
"
  (loop for instrument in hscom.hsage:*instruments*
        do (loop for timeframe in hscom.hsage:*timeframes*
                 do (bind ((rates (if hscom.all:*is-production*
                                      (get-rates-count-big instrument timeframe (+ *creation-dataset-size*
                                                                                    *training-dataset-size*
                                                                                    *testing-dataset-size*))
                                      (get-rates-random-count-big instrument timeframe (+ *creation-dataset-size*
                                                                                           *training-dataset-size*
                                                                                           *testing-dataset-size*)))))
                      ;; Storing rates.
                      (setf (gethash `(,instrument ,timeframe) *datasets*) rates)
                      ;; Storing unique datasets.
                      (setf (gethash `(,instrument ,timeframe :creation) *unique-datasets*)
                            (get-unique-dataset (subseq rates
                                                        0
                                                        *creation-dataset-size*)
                                                *unique-count* *lookahead* *lookbehind*))
                      (setf (gethash `(,instrument ,timeframe :training) *unique-datasets*)
                            ;; We need to offset the indexes so they match the rates in *DATASETS*.
                            (mapcar ^(+ _ *creation-dataset-size*)
                                    (get-unique-dataset (subseq rates
                                                                *creation-dataset-size*
                                                                (+ *creation-dataset-size* *training-dataset-size*))
                                                        *unique-count* *lookahead* *lookbehind*)))
                      (setf (gethash `(,instrument ,timeframe :testing) *unique-datasets*)
                            (mapcar ^(+ _ (+ *creation-dataset-size* *training-dataset-size*))
                                    (get-unique-dataset (subseq rates
                                                                (+ *creation-dataset-size* *training-dataset-size*))
                                                        *unique-count* *lookahead* *lookbehind*)))))))
;; (time (update-unique-datasets))

(def (function d) optimize-human-strategy (instrument timeframe human-strategy
                                                      &key (maximize nil)
                                                      (population-size 100)
                                                      (max-iterations 100)
                                                      (mutation-rate 0.1)
                                                      (fitness-metric :total-return))
  ($log $trace :-> :optimize-human-strategy)
  (bind ((search-space (gen-search-space
                        (assoccess human-strategy :parameters)
                        (assoccess human-strategy :args-ranges)))
         (get-fit-fn (lambda (fit)
                       (assoccess (get-fit fit) fitness-metric))))
    (eval `(genetic-algorithm:run-ga (,search-space
                                 :maximize ,maximize
                                 :population-size ,population-size
                                 :max-iterations ,max-iterations
                                 :mutation-rate ,mutation-rate
                                 :first-generation (bind ((hybrid (get-strategy ,instrument
                                                                                ,timeframe
                                                                                :hybrid
                                                                                ,(assoccess human-strategy :name)))
                                                          (genomes (population hybrid))
                                                          (best (best-individual hybrid)))
                                                     (when (not (eq best :null))
                                                       (gen-population-from-genomes
                                                        ',search-space
                                                        genomes
                                                        (length best)
                                                        ',(assoccess human-strategy :parameters))))
                                 :get-fit-fn (lambda (fit)
                                               (assoccess (genetic-algorithm:get-fit fit) ,fitness-metric))
                                 ;; :after-each-iteration (describe (first genetic-algorithm:-population-))
                                 :finally
                                 (bind ((best-individual (select-the-best -population-
                                                                          :get-fit-fn ,get-fit-fn))
                                        (best-genome (get-gens best-individual))
                                        (best-fitness (get-fit best-individual))
                                        (population-genomes (flatten (mapcar (lambda (ind)
                                                                               (get-gens ind))
                                                                             -population-)))
                                        (test-metrics (-evaluate-model
                                                       :instrument ,instrument
                                                       :timeframe ,timeframe
                                                       :environment :testing
                                                       :model (lambda (input-dataset)
                                                                (apply #'funcall ,(assoccess human-strategy :fn)
                                                                       input-dataset
                                                                       best-genome)))))
                                   (insert-hybrid ,(stringify instrument)
                                                  ,(stringify timeframe)
                                                  ,(assoccess human-strategy :name)
                                                  :training
                                                  ,max-iterations
                                                  population-genomes
                                                  best-genome
                                                  best-fitness)
                                   (insert-hybrid ,(stringify instrument)
                                                  ,(stringify timeframe)
                                                  ,(assoccess human-strategy :name)
                                                  :testing
                                                  ,max-iterations
                                                  population-genomes
                                                  best-genome
                                                  test-metrics)))
             (-evaluate-model
              :instrument ,instrument
              :timeframe ,timeframe
              :environment :training
              :model (lambda (input-dataset)
                       (funcall ,(assoccess human-strategy :fn)
                                input-dataset
                                ,@(assoccess human-strategy :parameters)))))))
  ($log $trace :<- :optimize-human-strategy))

(def (function d) get-signal-rates (instrument timeframe)
  (get-rates-count-big instrument timeframe *lookbehind*))
;; (get-signal-rates :AUD_USD :M15)

(def (function d) signal-strategy (instrument timeframe strategy-id model)
  (multiple-value-bind (tp sl activation)
      (funcall model (get-signal-rates instrument timeframe))
    (bind ((test-fitnesses (-evaluate-model
                            :instrument instrument
                            :timeframe timeframe
                            :environment :testing
                            :model model)))
      ;; We're going to allow any trade to pass (not using -TEST-CONDITIONS).
      (when (-test-conditions instrument tp sl test-fitnesses :hybridp t)
        (insert-signal strategy-id instrument timeframe test-fitnesses
                       test-fitnesses tp sl activation)))))

(def (function d) test-agents (instrument timeframe)
  (multiple-value-bind (tp sl activation agent-ids)
      ;; This one gets the final TP and SL.
      (eval-agents instrument timeframe (get-signal-rates instrument timeframe))
    (bind ((strategy (get-strategy instrument timeframe :hermes))
           ;; (train-fitnesses (evaluate-agents instrument timeframe :training))
           (test-fitnesses (evaluate-agents instrument timeframe :testing)))
      (when (-test-conditions instrument tp sl test-fitnesses)
        (insert-signal (id strategy) instrument timeframe test-fitnesses test-fitnesses tp sl activation)
        ))))

(def (function d) calculate-return (tp sl revenue)
  (if (or (= tp 0)
          (= sl 0))
      0
      (if (> revenue 0)
          (* (/ tp sl) -1) ;; To always get positive number.
          -1)))

(def (function d) -evaluate-model (&key instrument timeframe environment model)
  "Used by EVALUATE-AGENT and EVALUATE-AGENTS."
  (bind ((idxs (when *unique-point-p* (get-unique-dataset-idxs instrument timeframe environment)))
         (rates (get-dataset instrument timeframe))
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
    (loop for idx in idxs
          do (bind ((input-dataset (hsinp.rates:get-input-dataset rates idx))
                    (output-dataset (hsinp.rates:get-output-dataset rates idx)))
               (multiple-value-bind (tp sl activation)
                   (funcall model input-dataset)
                 (if (< activation hscom.hsage:*evaluate-agents-activation-threshold*)
                     ;; Ignore. Just increase NUM-DATAPOINTS.
                     (incf num-datapoints)
                     (bind ((trade (evaluate-trade tp sl output-dataset))
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
                               (< (abs (to-pips instrument sl)) hscom.hsage:*min-pips-sl*)
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
                             (push (assoccess (nth idx rates) :time) entry-times)
                             (push exit-time exit-times)
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
                           (incf idx finish-idx)))))))
    ($log $info (format nil "Traded ~a out of ~a datapoints." num-datapoints-traded num-datapoints))
    (bind ((returns (loop for revenue in revenues
                          for tp in tps
                          for sl in sls
                          collect (calculate-return tp sl revenue)))
           (total-return (reduce #'+ returns))
           (abs-tps (mapcar #'abs tps))
           (abs-sls (mapcar #'abs sls)))
      `((:begin-time . ,(assoccess (first rates) :time))
        (:end-time . ,(assoccess (last-elt rates) :time))
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

(def (function d) evaluate-agent (agent instrument timeframe environment &key (return-fitnesses-p nil))
  (bind ((fitnesses (-evaluate-model :instrument instrument
                                     :timeframe timeframe
                                     :environment environment
                                     :model (lambda (input-dataset)
                                              (eval-agent agent input-dataset)))))
    (setf (slot-value agent '.metrics)
          fitnesses)
    ;; (if (and (slot-boundp agent 'metrics-id)
    ;;          (metrics-id agent))
    ;;     (update-metrics fitnesses (metrics-id agent))
    ;;     (progn
    ;;       (setf (metrics-id agent)
    ;;             (id (insert-metrics fitnesses)))))
    (if return-fitnesses-p
        fitnesses
        agent)))
;; (evaluate-agent (gen-agent 3 *rates* (access *beliefs* :perception-fns) 10 55) (subseq *rates* 0 200))

(def (function d) evaluate-agents (instrument timeframe environment)
  (-evaluate-model :instrument instrument
                   :timeframe timeframe
                   :environment environment
                   :model (lambda (input-dataset)
                            (eval-agents instrument timeframe input-dataset))))
;; (time (evaluate-agents :EUR_USD hscom.hsage:*train-tf* '(:BULLISH) (subseq *rates* 0 200)))

(def (function d) wipe-agents ()
  (when hscom.hsage:*wipe-agents-p*
    ($log $trace :-> :wipe-agents)
    (conn (query (:delete-from 'agents :where (:= 1 1))))
    ($log $trace :<- :wipe-agents)))
;; (wipe-agents)

(def (function d) get-agents-from-cache (instrument timeframe)
  (gethash (list instrument timeframe) *agents-cache*))

;; (get-agents :AUD_USD hscom.hsage:*train-tf*)
;; (get-agents-from-cache :AUD_USD hscom.hsage:*train-tf*)

(def (function d) sync-agents (instrument timeframe)
  ;; Get agents from database (A1)
  ;; Get agents from cache (A2)
  ;; Update or add agents from A1 using A2
  ;; Delete agents found in A1 but not in A2
  ($log $trace :-> :sync-agents)
  (let ((A1 (get-agents instrument timeframe))
        (A2 (get-agents-from-cache instrument timeframe)))
    ($log $debug "Database agents:" (length A1))
    ($log $debug "Cache agents:" (length A2))
    (conn
     ;; First we update existing agents on database and insert the new ones.
     ;; If the algorithm crashes, we at least keep some of the agents updated and new ones.
     (loop for agent in A2
           do (if (get-dao '<agent> (slot-value agent 'id))
                  (update-dao agent)
                  (insert-agent agent)))
     ;; Now we delete the agents that are in A1 (database) but not in A2 (cache).
     (let ((ids (mapcar (lambda (agent) (slot-value agent 'id)) A2)))
       (loop for agent in A1
             do (bind ((id (slot-value agent 'id))
                       (foundp (find id ids :test #'string=)))
                  (unless foundp
                    (delete-agent agent)))))))
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

(def (function d) .to-retire-ids (instrument timeframe to-retire-count)
  (conn
   (query
    (:limit
     (:order-by
      (:select 'agents.id :from 'agents
        :join 'metrics
        :on (:= 'metrics.id 'metrics-id)
        :where (:and
                (:= 'strategy-id
                    (:select 'strategies.id :from 'strategies
                      :join 'instruments
                      :on (:= 'instruments.id 'instrument-id)
                      :join 'timeframes
                      :on (:= 'timeframes.id 'timeframe-id)
                      :join 'methods
                      :on (:= 'methods.id 'method-id)
                      :where (:and (:= 'instruments.name (string+ instrument))
                                   (:= 'timeframes.name (string+ timeframe))
                                   (:= 'methods.name (string+ :hermes)))))
                (:= 'agents.retired nil)))
      (:asc 'metrics.avg-return))
     to-retire-count)
    :column)))

(def (function d) retire-agents (instrument timeframe)
  ($log $trace :-> :retire-agents)
  (conn
   (bind ((agents-count (get-agents-count instrument timeframe))
          (to-retire-count (- agents-count (- *max-agents-count* *min-agents-count*))))
     (when (> agents-count *max-agents-count*)
       ($log $info (format nil "Retiring ~a agents out of ~a in ~a ~a"
                           to-retire-count
                           agents-count
                           instrument
                           timeframe
                           ))
       (bind ((ids (.to-retire-ids instrument timeframe to-retire-count)))
         ;; Retiring from cache.
         (setf (gethash (list instrument timeframe) *agents-cache*)
               (remove-if
                ^(find (id _) ids :test #'string=)
                (get-agents-from-cache instrument timeframe)))
         ;; Retiring from DB.
         (query
          (:update 'agents
            :set 'retired t
            :where (:in 'id (:set ids))))))))
  ($log $trace :<- :retire-agents))
;; (retire-agents :AUD_USD :M15)

(let ((sync-table (make-hash-table :test 'equal :synchronized t)))
  (def (function d) cache-agents-from-db (instrument timeframe &optional (safe-cache-p nil))
    "Caches agents from `instrument` and `timeframe` from the database to *AGENTS-CACHE*."
    ($log $trace :-> :cache-agents-from-db)
    (when safe-cache-p
      ($log $info "Adding cache safety for" instrument timeframe)
      (setf (gethash (list instrument timeframe) sync-table) t)
      ($log $info "Caching agents for" instrument timeframe)
      (get-agents instrument timeframe -1 0))
    (unless (gethash (list instrument timeframe) sync-table)
      ($log $info "Caching agents for" instrument timeframe)
      (get-agents instrument timeframe -1 0))
    ($log $trace :<- :cache-agents-from-db))
  ;; (cache-agents-from-db)

  (def (function d) uncache-agents-from-db (instrument timeframe &optional (safe-cache-p nil))
    "Restarts the agents cache for `instrument` and `timeframe`."
    ($log $trace :-> :uncache-agents-from-db)
    (when safe-cache-p
      ($log $info "Removing cache safety for" instrument timeframe)
      (setf (gethash (list instrument timeframe) sync-table) nil))

    (unless (gethash (list instrument timeframe) sync-table)
      ($log $info "Releasing cache for" instrument timeframe)
      (setf (gethash (list instrument timeframe) *agents-cache*) nil))

    (hsage.utils:refresh-memory)
    ($log $trace :<- :uncache-agents-from-db)))

(def (function d) .get-agents (instrument timeframe)
  (conn
   (query
    (:select '*
      :from 'agents
      :where (:and (:= 'strategy-id
                       (:select 'strategies.id :from 'strategies
                         :join 'instruments
                         :on (:= 'instruments.id 'instrument-id)
                         :join 'timeframes
                         :on (:= 'timeframes.id 'timeframe-id)
                         :join 'methods
                         :on (:= 'methods.id 'method-id)
                         :where (:and (:= 'instruments.name (string+ instrument))
                                      (:= 'timeframes.name (string+ timeframe))
                                      (:= 'methods.name (string+ :hermes)))))
                   (:= 'retired nil)))
    (:dao <agent>))))

(def (function d) get-agents (instrument timeframe &optional (limit -1) (offset 0))
  (let (result)
    (if-let ((agents (gethash (list instrument timeframe) *agents-cache*)))
      (loop for agent in (limit-seq agents limit offset)
            do (push agent result))
      (bind ((agents (.get-agents instrument timeframe)))
        (when agents
          (map nil #'agent-correct-perception-fns agents)
          (setf (gethash (list instrument timeframe) *agents-cache*) agents)
          (loop for agent in (limit-seq agents limit offset)
                do (push agent result)))))
    result))

(def (function d) add-agent (agent instrument timeframe)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe) *agents-cache*)
        (append (gethash (list instrument timeframe) *agents-cache*)
                (list agent))))

(def (function d) remove-agent (agent instrument timeframe)
  "Works with *agents-cache*"
  (setf (gethash (list instrument timeframe) *agents-cache*)
        (remove agent
                (gethash (list instrument timeframe) *agents-cache*)
                :test (lambda (elt1 elt2)
                        (string= (slot-value elt1 'id)
                                 (slot-value elt2 'id))))))

(def (function d) insert-agent (agent)
  "Works with database."
  (conn
   (unless (get-dao '<agent> (slot-value agent 'id))
     (setf (slot-value agent 'metrics-id)
           (id (insert-metrics (slot-value agent '.metrics))))
     (insert-dao agent)
     ))
  agent)

(def (function d) delete-agent (agent)
  ($log $debug (string+ "Deleting agent " (id agent)))
  (conn
   (with-transaction ()
     (execute (:delete-from 'metrics :where (:= 'id (metrics-id agent))))
     (delete-dao agent))))

(def (function d) get-agents-count (instrument timeframe)
  (conn
   (query
    (:select (:count 'agents.id)
      :from 'agents
      :where (:and
              (:= 'strategy-id
                  (:select 'strategies.id :from 'strategies
                    :join 'instruments
                    :on (:= 'instruments.id 'instrument-id)
                    :join 'timeframes
                    :on (:= 'timeframes.id 'timeframe-id)
                    :join 'methods
                    :on (:= 'methods.id 'method-id)
                    :where (:and (:= 'instruments.name (string+ instrument))
                                 (:= 'timeframes.name (string+ timeframe))
                                 (:= 'methods.name (string+ :hermes)))))
              (:= 'retired nil)))
    :single)))

(def (function d) eval-agent (agent input-dataset)
  (let ((perception-fn (gen-perception-fn (perception-fns agent))))
    (cond ((eq *fis-method* :index)
           (eval-ifis-idx (funcall perception-fn input-dataset)
                          (slot-value agent 'antecedents)
                          (slot-value agent 'consequents)))
          (t
           (eval-ifis-gen (funcall perception-fn input-dataset)
                          (slot-value agent 'antecedents)
                          (slot-value agent 'consequents)
                          (slot-value agent 'perceptions-count)
                          (slot-value agent 'rules-count)))
          )))

(def (function d) gen-perception-fn (perception-params)
  (hsper:gen-perception-fn perception-params))

(def (function d) update-agent-fitnesses (instrument timeframe agent)
  (bind ((agents (gethash (list instrument timeframe) *agents-cache*))
         (agent-idx (position agent agents :test (lambda (agent1 agent2) (string= (slot-value agent1 'id)
                                                                                  (slot-value agent2 'id))))))
    (setf (nth agent-idx agents) (evaluate-agent agent instrument timeframe :training))))

(def (function d) update-agents-fitnesses (instrument timeframe agents)
  (loop for agent in agents
        do (update-agent-fitnesses instrument timeframe agent))
  agents)

(def (function d) ->agent-metric (agent metric)
  (assoccess (slot-value agent '.metrics) metric))

(def (function d) -base-reject (agent)
  "Used by AGENT-DOMINATED?-XXX."
  (bind ((tps (->agent-metric agent :tps))
         (avg-return (->agent-metric agent :avg-return)))
    (or (= (length tps) 0)
        (<= avg-return
            hscom.hsage:*min-agent-avg-return*)
        (< (length tps)
           hscom.hsage:*min-num-trades-training*))))

(def (function d) agent-dominated?-pareto (agent agents &optional (logp nil))
  (if (-base-reject agent)
      ;; AGENT is dominated.
      t
      (bind ((agent-id-0 (slot-value agent 'id))
             (avg-revenue-0 (->agent-metric agent :avg-revenue))
             (trades-won-0 (->agent-metric agent :trades-won))
             (trades-lost-0 (->agent-metric agent :trades-lost))
             (agent-direction-0 (nth 0 (->agent-metric agent :tps)))
             (avg-return-0 (->agent-metric agent :avg-return))
             (total-return-0 (->agent-metric agent :total-return))
             (activations-0 (->agent-metric agent :activations))
             (returns-0 (->agent-metric agent :returns))
             ;; (agent-directions (->agent-metric agent :tps))
             (stdev-revenue-0 (->agent-metric agent :stdev-revenue))
             ;; (entry-times-0 (->agent-metric agent :entry-times))
             (is-dominated? nil))
        ;; (format t "~a, ~a, ~a~%~%" avg-revenue-0 trades-won-0 trades-lost-0)
        (loop for agent in agents
              do (when (> (length (->agent-metric agent :tps)) 0)
                   (bind ((agent-id (->agent-metric agent :id))
                          (avg-revenue (->agent-metric agent :avg-revenue))
                          (trades-won (->agent-metric agent :trades-won))
                          (trades-lost (->agent-metric agent :trades-lost))
                          (agent-direction (nth 0 (->agent-metric agent :tps)))
                          (avg-return (->agent-metric agent :avg-return))
                          (total-return (->agent-metric agent :total-return))
                          (activations (->agent-metric agent :activations))
                          (returns (->agent-metric agent :returns))
                          ;; (agent-directions (->agent-metric agent :tps))
                          (stdev-revenue (->agent-metric agent :stdev-revenue))
                          ;; (entry-times (->agent-metric agent :entry-times))
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
                       (setf is-dominated? t)
                       (return)))))
        is-dominated?)))

(def (function d) agent-dominated?-mactavator (agent agents &optional (logp nil))
  (if (-base-reject agent)
      ;; AGENT is dominated.
      t
      (bind ((total-return-0 (slot-value agent 'total-return))
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

(def (function d) get-agent (instrument timeframe agent-id)
  (find agent-id (gethash (list instrument timeframe) *agents-cache*)
        :key (lambda (agent) (slot-value agent 'id))
        :test #'string=))
;; (get-agent :EUR_USD hscom.hsage:*train-tf* '(:BULLISH) "48F3970F-36C1-4A49-9E54-95746CFEA9FE")
;; (slot-value (first (get-agents :EUR_USD hscom.hsage:*train-tf* '(:BULLISH))) 'id)

(def (function d) eval-agents (instrument timeframe input-dataset)
  (let (tps sls activations ids)
    (let ((agents (get-agents instrument timeframe)))
      (loop for agent in agents
            do (multiple-value-bind (tp sl activation)
                   (eval-agent agent input-dataset)
                 (bind ((last-rate (last-elt input-dataset))
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
    (bind ((idxs (hsage.utils:sorted-indexes activations #'>))
           (tp (nth (position 0 idxs) tps))
           (sl (nth (position 0 idxs) sls))
           (activation (nth (position 0 idxs) activations))
           (len (min hscom.hsage:*consensus-threshold* (length activations))))
      (unless (< len 3)
        (bind ((bullish-acts (loop for idx from 0 below len
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
      (values (/ tp 1)
              (/ sl 1)
              activation
              (list (nth (position 0 idxs) ids))))))

(def (function d) make-agent (inputs outputs perception-fns lookahead lookbehind)
  "Used for manual agent creation."
  (let ((agent (make-instance 'agent)))
    (setf (slot-value agent 'perception-fns) (format nil "~s" perception-fns))
    (setf (slot-value agent 'lookahead) lookahead)
    (setf (slot-value agent 'lookbehind) lookbehind)
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
                           (hscom.utils:assoccess perc :lookahead)
                           (hscom.utils:assoccess perc :lookbehind)
                           :direction-fn #'minusp)
     (let ((agent (make-agent inp out (hscom.utils:assoccess perc :perception-fns) 10 10)))
       (list (antecedents agent)
             (consequents agent)))
     )))

;; (defparameter *agent* (gen-agent 3 :AUD_USD *rates* (hscom.utils:assoccess (hsper:gen-random-perceptions 2) :perception-fns) 10 100))
;; (antecedents *agent*)

(def (function d) interactive-make-agent (&key (lookahead 10) (rules-count 3))
  "Asks the user what perception function to generate for an agent."
  (block all
    (let ((options (get-perceptions))
          (p-rows)
          (percs)
          (instrument :AUD_USD)
          (lookbehind -1)
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
                       (when (> lookbehind lookbehind)
                         (setf lookbehind lookbehind))
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
      (format t "How many datapoints in the future do you want to consider for calculating TP & SL (default = ~a):~%" lookahead)
      (setf lookahead (let ((ans (read-line))) (if (string= ans "") lookahead (read-from-string ans))))
      ;; Creating fuzzy system.
      (let ((percs (make-array (length percs) :initial-contents percs)))
        (loop
          do (multiple-value-bind (inputs outputs idxs)
                 (get-inputs-outputs rules-count
                                     instrument
                                     *rates*
                                     (hsper:gen-perception-fn percs)
                                     lookahead
                                     lookbehind
                                     :direction-fn direction-fn)
               ;; Getting rate plots
               (let ((agent (make-agent inputs outputs percs lookahead lookbehind))
                     (rate-plots (apply #'concatenate 'string
                                        (loop for idx in idxs
                                              collect (plot-rates (subseq *rates* (- idx lookbehind) (+ idx lookahead)))))))
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
  (bind ((max-height 0)
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

;; (get-perceptions)
;; (hsper:fixed=>sma-close 5 10)
;; (gen-perception-fn (get-perceptions))
;; (gen-perception-fn (format nil "~a" (hsper:random=>sma-close)))

;; (ql:quickload :cl-charms)
;; (cl-charms:)
;; (ql:system-apropos "menu")

;; Perception fns (build a list of arrays of arrays)
;; lookahead
;; lookbehind (automatic)
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
                                            (/ (assoccess rate :time) 1000000)
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

(def (function d) gen-agent (num-rules instrument timeframe perception-fns lookahead lookbehind)
  (bind ((dataset (get-dataset instrument timeframe :creation))
         (idxs (get-unique-dataset-idxs instrument timeframe :creation))
         (strategy (get-strategy instrument timeframe :hermes)))
    (let ((agent (make-instance '<agent>)))
      (setf (slot-value agent 'strategy-id) (id strategy))
      (setf (slot-value agent 'creation-begin-time) (assoccess (first dataset) :time))
      (setf (slot-value agent 'creation-end-time) (assoccess (last-elt dataset) :time))
      (setf (slot-value agent 'perception-fns) perception-fns)
      (setf (slot-value agent 'lookahead) lookahead)
      (setf (slot-value agent 'lookbehind) lookbehind)
      (setf (slot-value agent 'rules-count) num-rules)
      (setf (slot-value agent 'perceptions-count)
            (get-perceptions-count perception-fns))
      (multiple-value-bind (antecedents consequents)
          (make-ifis agent num-rules instrument dataset idxs)
        (setf (slot-value agent 'antecedents) antecedents)
        (setf (slot-value agent 'consequents) consequents))
      agent)))

(def (function d) gen-agents (num-agents num-rules instrument rates perception-fns lookahead lookbehind)
  (loop repeat num-agents collect (gen-agent instrument num-rules rates perception-fns lookahead lookbehind)))
;; (gen-agents 2 3 *rates* (assoccess *beliefs* :perception-fns) 10 55)

(def (function d) get-same-direction-outputs-idxs (instrument rates count &key (lookahead 10) (lookbehind 10) direction-fn)
  (bind ((r (random-float 0 1))
         (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
         (opposite-pred (if (> r 0.5) #'minusp #'plusp))
         (idxs (shuffle (iota (- (length rates) lookahead lookbehind) :start lookbehind)))
         (result))
    (loop for idx in idxs
          do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead)))
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
                                         :lookahead lookahead
                                         :lookbehind lookbehind
                                         :direction-fn opposite-pred))))
;; (get-same-direction-outputs-idxs *rates* :lookahead 5)

(def (function d) get-same-direction-outputs-idxs-all (instrument rates &key (lookahead 10) (lookbehind 10) direction-fn)
  (bind ((r (random-float 0 1))
         (pred (if direction-fn direction-fn (if (> r 0.5) #'plusp #'minusp)))
         (opposite-pred (if (> r 0.5) #'minusp #'plusp))
         (idxs (iota (- (length rates) lookahead lookbehind) :start lookbehind))
         (result))
    (loop for idx in idxs
          do (let ((tp-sl (get-tp-sl (subseq rates idx) lookahead)))
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
                                             :lookahead lookahead
                                             :lookbehind lookbehind
                                             :direction-fn opposite-pred))))
;; (length (get-same-direction-outputs-idxs-all :AUD_USD *rates* :lookahead 10))

(def (function d) get-inputs-outputs (num-rules instrument rates idxs perception-fn lookahead lookbehind &key direction-fn)
  (bind ((idxs (if idxs (^(sort (subseq (shuffle (alexandria:copy-sequence 'list idxs)) 0 num-rules) #'<))
                   (^(sort _ #'<)
                     (cond (*exhaust-rules-in-creation-dataset-p*
                            (^(remove-duplicates _)
                              (get-same-direction-outputs-idxs-all
                               instrument rates
                               :lookahead lookahead
                               :lookbehind lookbehind
                               :direction-fn direction-fn)))
                           (t
                            (^(remove-duplicates _)
                              (get-same-direction-outputs-idxs
                               instrument rates num-rules
                               :lookahead lookahead
                               :lookbehind lookbehind
                               :direction-fn direction-fn)))))))
         (chosen-inputs (loop for idx in idxs collect (funcall perception-fn (get-input-dataset rates idx))))
         (chosen-outputs (loop for idx in idxs collect (get-tp-sl (get-output-dataset rates idx) lookahead))))
    (values chosen-inputs chosen-outputs idxs)))

(comment
 (time (bind ((perc (hsper:gen-random-perceptions 5))
              ((:values inputs outputs)
               (get-inputs-outputs 10 :EUR_USD *rates*
                                   (hsper:gen-perception-fn (hscom.utils:assoccess perc :perception-fns))
                                   (hscom.utils:assoccess perc :lookahead)
                                   (hscom.utils:assoccess perc :lookbehind))))
             (list (length inputs)
                   (length outputs)))))

(def (function d) make-ant-con (inputs outputs)
  (values
   (bind ((v (flatten
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
   (bind ((v (flatten
              (loop for output in outputs
                    collect (bind ((tp (assoccess output :tp))
                                   (sl (assoccess output :sl)))
                              ;; Consequent creation.
                              (list 0 tp
                                    (* hscom.hsage:*n-times-sl-for-max-sl* sl) sl))))))
     (make-array (length v) :initial-contents v))))
;; (make-ifis *agent* 3 :AUD_USD *rates*)

(def (function d) make-ifis (agent num-rules instrument rates idxs)
  "Analytical version."
  (bind ((perception-fn (gen-perception-fn (perception-fns agent)))
         (lookahead (slot-value agent 'lookahead))
         (lookbehind (slot-value agent 'lookbehind)))
    (multiple-value-bind (chosen-inputs chosen-outputs)
        (get-inputs-outputs num-rules instrument rates idxs perception-fn lookahead lookbehind)
      (make-ant-con chosen-inputs chosen-outputs))))
;; (plot-agent-rules *agent*)

(def (function d) optimization (instrument timeframe gen-agent-fn stop-count &optional (stop-criterion))
  ($log $trace :-> :optimization)
  ;; Checking if we need to initialize the agents collection.
  (let ((agents (if-let ((agents (get-agents instrument timeframe)))
                  (update-agents-fitnesses instrument timeframe agents)
                  (loop repeat hscom.hsage:*initial-agent-count*
                        collect (evaluate-agent (funcall gen-agent-fn) instrument timeframe :training))))
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
                     (loop for agent in agents
                           do (unless (get-agent instrument timeframe (slot-value agent 'id))
                                (add-agent agent instrument timeframe)))
                     ($log $info "Pareto frontier updated successfully")
                     (return))
                   (block opt
                     (bind ((challenger (list (evaluate-agent (funcall gen-agent-fn) instrument timeframe :training)))
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
                                        (remove-agent in-trial instrument timeframe)
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
  (bind ((agents-props (prepare-agents-properties agents))
         (vals (loop for agent-props in agents-props
                     collect (let ((avg-tp (read-from-string (assoccess agent-props :avg-tp)))
                                   (avg-sl (read-from-string (assoccess agent-props :avg-sl))))
                               (append agent-props
                                       `((:r/r . ,(format-rr avg-sl avg-tp))))))))
    (when (> (length agents-props) 0)
      vals)))
;; (agents-to-alists (get-agents :AUD_USD hscom.hsage:*train-tf* '(:BULLISH)))

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

(def (function d) alist-keys (alist)
  (loop for item in alist collect (car item)))
;; (alist-keys (car (prepare-agents-properties (get-agents :AUD_USD hsage.config:*train-tf* '(:bullish)))))

(def (function d) alist-values (alist)
  (loop for item in alist collect (cdr item)))
;; (alist-values (car (prepare-agents-properties (get-agents :EUR_USD hsage.config:*train-tf* '(:bullish)))))

(def (function d) analysis (&key (granularity 10) (return-results-p nil) (label ""))
  (bind ((trades (conn (query (:select 'trades.creation-time
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
      (bind ((acts (flatten (loop for trade in trades
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
  (bind ((label hscom.hsage:*consensus-threshold*)
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

(def (function d) -get-trade-time (trade)
  "Used in `-validate-trades`."
  (let ((entry-time (slot-value trade 'entry-time))
        (creation-time (slot-value trade 'creation-time)))
    (if hscom.all:*is-production*
        ;; The exact time when the trade got created, e.g 4:33 PM.
        creation-time
        (if (not (equal entry-time :null))
            ;; The time of the last traded candle in the testing dataset.
            ;; This time will be a rounded hour (if using hours), e.g. 4:00 PM.
            entry-time
            creation-time))))

(def (function d) -validate-trades (instrument trades older-than)
  "We use `older-than` to determine what trades to ignore due to possible lack of prices for validation."
  (bind ((day (local-time:timestamp-day-of-week (local-time:now)))
         (older-than (cond ((= day 0) (+ older-than 2)) ;; it's Sunday, add 2 days
                           ((= day 1) (+ older-than 1)) ;; it's Monday, add 1 day
                           (t older-than) ;; default, leave unchanged.
                           )))
    (when (> (length trades) 0)
      ($log $info (format nil "Trying to validate ~a trades." (length trades)))
      (bind ((oldest (first (sort (copy-sequence 'list trades) #'< :key ^(entry-time _))))
             ;; (newest (first (sort (copy-sequence 'list trades) #'> :key #'-get-trade-time)))
             (rates (get-rates-range-big instrument
                                         hscom.hsage:*validation-timeframe*
                                         (entry-time oldest)
                                         ;; (-get-trade-time newest)
                                         (now)
                                         )))
        (loop for trade in trades
              do (bind ((idx (position (entry-time trade) rates :test #'<= :key (lambda (rate) (assoccess rate :time)))))
                   (when idx
                     (let ((sub-rates (subseq rates idx))
                           (from-timestamp (local-time:unix-to-timestamp (entry-time trade))))
                       (when (or (not hscom.all:*is-production*)
                                 (local-time:timestamp< from-timestamp
                                                        (local-time:timestamp- (local-time:now) older-than :day)))
                         (multiple-value-bind (revenue return exit-time exit-price)
                             (get-trade-result (entry-price trade)
                                               (tp trade)
                                               (sl trade)
                                               sub-rates)
                           (when revenue
                             ($log $info (format nil "Trade validated. Revenue: ~2$, Return: ~2$, EntryT: ~a, EntryP: ~5$, ExitT: ~a, ExitP: ~5$."
                                                 (to-pips instrument revenue)
                                                 return
                                                 (entry-time trade)
                                                 (entry-price trade)
                                                 exit-time
                                                 exit-price))
                             (setf (.return trade) return)
                             (setf (revenue trade) revenue)
                             (setf (exit-time trade) exit-time)
                             (setf (exit-price trade) exit-price)
                             (conn (update-dao trade)))))))))))))

(def (function d) get-trades-no-result (instrument timeframe)
  (conn (query (:select 'trades.*
                 :from 'signals
                 :join 'trades
                 :on (:= 'trades.id 'signals.trade-id)
                 :join 'strategies
                 :on (:= 'strategies.id 'signals.strategy-id)
                 :where (:and (:= 'trades.exit-time 0)
                              (:= 'strategies.instrument-id
                                  (:select 'id
                                    :from 'instruments
                                    :where (:= 'instruments.name (string+ instrument))))
                              (:= 'strategies.timeframe-id
                                  (:select 'id
                                    :from 'timeframes
                                    :where (:= 'timeframes.name (string+ timeframe))))))
               (:dao <trade>))))
;; (get-trades-no-result :AUD_USD :M15)

(def (function d) validate-trades (&optional (older-than 0))
  ;; TODO: Refactor this. We should be calling something like (SYNC-RATES '(:M1))
  ;; Syncing M1 rates.
  ;; (loop for instrument in hscom.hsage:*instruments*
  ;;       do (loop for timeframe in `(,hscom.hsage:*validation-timeframe*)
  ;;                do (sync-rates instrument timeframe)))
  (loop for instrument in hscom.hsage:*instruments*
        do (loop for timeframe in hscom.hsage:*timeframes*
                 do (let ((trades (get-trades-no-result instrument timeframe)))
                      (when trades
                        (-validate-trades instrument trades older-than))))))
;; (validate-trades)

(def (function d) delete-signals (from to)
  (conn
   (bind ((trade-ids (query (:select 'trade-id :from 'signals
                              :where (:and (:>= 'timestamp from)
                                           (:<= 'timestamp to))))))
     (with-transaction ()
       ;; Deleting signals.
       (execute (:delete-from 'signals
                  :where (:and (:>= 'timestamp from)
                               (:<= 'timestamp to))))
       ;; Deleting trades associated to these signals.
       (execute (:delete-from 'trades
                  :where (:in 'id (:set trade-ids))))))))

(def (function d) get-trade-result (entry-price tp sl rates)
  (let ((low-type (if (plusp tp) :low-bid :low-ask))
        (high-type (if (plusp tp) :high-bid :high-ask)))
    (loop for rate in rates do
             (progn
               ;; Then it's a buy. Lose.
               (when (and (> tp 0) (< (- (assoccess rate low-type) entry-price) sl))
                 (return (values sl
                                 (calculate-return tp sl sl)
                                 (assoccess rate :time)
                                 (+ entry-price sl))))
               ;; Then it's a sell. Lose.
               (when (and (< tp 0) (> (- (assoccess rate high-type) entry-price) sl))
                 (return (values (- sl)
                                 (calculate-return tp sl (- sl))
                                 (assoccess rate :time)
                                 (+ entry-price (- sl)))))
               ;; Then it's a buy. Win.
               (when (and (> tp 0) (> (- (assoccess rate high-type) entry-price) tp))
                 (return (values tp
                                 (calculate-return tp sl tp)
                                 (assoccess rate :time)
                                 (+ entry-price tp))))
               ;; Then it's a sell. Win.
               (when (and (< tp 0) (< (- (assoccess rate low-type) entry-price) tp))
                 (return (values (abs tp)
                                 (calculate-return tp sl (abs tp))
                                 (assoccess rate :time)
                                 (+ entry-price (abs tp)))))
               ))))
;; (get-trade-result 0.72274 -0.0018100000000000893 0.0013900000000000026 )

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
