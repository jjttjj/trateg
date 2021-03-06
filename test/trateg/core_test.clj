(ns trateg.core-test
  (:require [clojure.test :refer :all]
            [data.spx :as data]
            [trateg.core :refer :all]
            [trateg.indicator :as ind]
            [trateg.ta4j :as ta4j :refer [ind ind-values analysis rule crit-values]])
  (:import [org.ta4j.core Order Order$OrderType]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;data;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def series4j (ta4j/->series data/spx-bars))

(def strat4j
  (let [rsi (ind :RSI (ind :helpers/ClosePrice series4j) 14)]
    (ta4j/base-strategy (rule :CrossedDownIndicator rsi 30) 
                   (rule :WaitFor Order$OrderType/BUY 5))))
(def trades4j (ta4j/run-strat series4j strat4j))

(defn mkbars [bar-maps {:keys [rsi-period atr-period]}]
  (let [bar-cols   (row-maps->column-map bar-maps)
        indicators {:atr (ind/atr atr-period
                                  (:high bar-cols)
                                  (:low bar-cols)
                                  (:close bar-cols))
                    :rsi (ind/rsi rsi-period (:close bar-cols))}]
    (doall (column-map->row-maps (merge bar-cols indicators)))))


(def BARS (mkbars data/spx-bars {:rsi-period 14 :atr-period 20}))

(def STRAT #(-> %
                (cross-trigger :under :rsi 30 :enter :long)
                (hold-for-exit 5)
                enforce-single-position
                enforce-no-entry-on-exit-bar
                execute-pending))
(def results1 (run-strat BARS STRAT))

(def trades1 (:trades results1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;indicator tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-atr
  (is (all-fuzzy= (ind-values (ind :ATR series4j 14))
                  (ind/atr 14
                           (map :high data/spx-bars)
                           (map :low data/spx-bars)
                           (map :close data/spx-bars)))))

(deftest test-stochastic
  (is (all-fuzzy=
       (ind-values (ind :StochasticOscillatorK series4j 14))
       (ind/stochastic 14 (map (juxt :high :low :close) data/spx-bars)))))

(deftest test-sma
  (is (all-fuzzy= 
       (ind-values (ind :SMA (ind :helpers/ClosePrice series4j) 14))
       (ind/sma 14 (map :close data/spx-bars)))))

(deftest test-ema
  (is (all-fuzzy= 
       (ind-values (ind :EMA (ind :helpers/ClosePrice series4j) 14))
       (ind/ema 14 (map :close data/spx-bars)))))

(deftest test-rsi
  (is (all-fuzzy= 
       (ind-values (ind :RSI (ind :helpers/ClosePrice series4j) 14))
       (ind/rsi 14 (map :close data/spx-bars)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;crit tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-win-rate
  (is (fuzzy= (crit-values :AverageProfitableTrades series4j trades4j)
              (win-rate trades1))))

(deftest test-total-return
  (is (fuzzy= (crit-values :TotalProfit series4j trades4j)
              (total-return trades1))))

(deftest test-number-of-trades
  (is (fuzzy= (crit-values :NumberOfTrades series4j trades4j)
              (count trades1))))

(deftest test-max-drawdown
  (is (fuzzy= (max-drawdown-over-bars data/spx-bars trades1)
              (crit-values :MaximumDrawdown series4j trades4j))))

(deftest test-cash-flow
  (is
   (all-fuzzy=
    (cash-flow data/spx-bars trades1)
    (ind-values (analysis :CashFlow series4j trades4j)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;trade results tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-trades
  (is (every? true?
              (map (fn [mine theirs]
                     (and (fuzzy= (:entry-price mine) (:entry-price theirs) )
                          (fuzzy= (:exit-price mine) (:exit-price theirs) )
                          (= (:entry-time mine) (:entry-time theirs))
                          (= (:exit-time mine) (:exit-time theirs))
                          (= (:entry-index mine) (:entry-index theirs))
                          (= (:exit-index mine) (:exit-index theirs))))
                   trades1
                   (ta4j/record->clj series4j trades4j)))))
