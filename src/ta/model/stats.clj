(ns ta.model.stats
  (:require
   [medley.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;util;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def diff-tolerance 0.0000000001)

(defn fuzzy=
  ([x y] (fuzzy= diff-tolerance x y))
  ([tolerance x y]
   (let [diff (Math/abs (- x y))]
     (< diff tolerance))))

(defn all-fuzzy= [a b]
  (and (= (count a) (count b))
       (every? true? (map (fn [a b] (fuzzy= a b)) a b))))

(defn mean [coll]
  (/ (reduce + coll) (count coll)))

;;for sample (not population)
(defn standard-deviation [coll]
  (let [avg     (mean coll)
        squares (map #(Math/pow (- % avg) 2) coll)]
    (-> (reduce + squares)
        (/ (dec (count coll)))
        Math/sqrt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculations on single trade;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn side-add [side initial gain]
  (case side
    :long  (+ initial gain)
    :short (- initial gain)))

(defn nominal-return [side entry exit]
  (case side
    :long  (- exit entry)
    :short (- entry exit)))

(defn ratio-return [side entry exit]
  (case side
    :long  (/ exit entry)
    :short (/ entry exit)))

(defn nominal-profit [{:keys [entry-price exit-price side] :as trade}]
  (nominal-return side entry-price exit-price))

(defn ratio-profit [{:keys [side entry-price exit-price] :as trade}]
  (ratio-return side entry-price exit-price))

(defn trade-complete? [trade] (-> trade :exit-price some?))

(defn win? [{:keys [side entry-price exit-price] :as trade}]
  (when (and entry-price exit-price)
    (case side
      :long  (> exit-price entry-price)
      :short (< exit-price entry-price))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculations on multiple trades;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn win-rate [trades]
  (if (empty? trades)
    0
    (double
     (/ (count (filter win? trades))
        (count trades)))))

(defn total-return [trades]
  (->> trades
       (map ratio-profit)
       (reduce *)))

(defn profit-factor [trades]
  (let [{:keys [profit loss]}
        (reduce (fn [acc {:keys [entry-price exit-price] :as trade}]
                  (if (trade-complete? trade)
                    (let [win?  (win? trade)
                          loss? (not win?)]
                      (cond
                        win?  (update acc :profit + (nominal-profit trade))
                        loss? (update acc :loss + (nominal-profit trade))))
                    acc))
                {:profit 0 :loss 0}
                trades)]
    (/ profit (Math/abs loss))))

(defn mean-return [trades]
  (mean (map ratio-profit trades)))

(defn mean-nominal-return [trades]
  (mean (map nominal-profit trades)))

(defn sharpe-ratio [trades risk-free-rate]
  (/ (- (total-return trades) risk-free-rate)
     (standard-deviation (map ratio-profit trades))))

(defn max-drawdown [returns]
  (let [peaks (reductions max returns)]
    (reduce max (map (fn [p x] (/ (- p x) p)) peaks returns))))

(defn cash-flow-over-trades [trades]
  (reductions * 1 (map ratio-profit trades)))

(defn max-drawdown-over-trades [trades]
  (max-drawdown (cash-flow-over-trades trades)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculations on trades and bars;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cash-flow [bars trades]
  (loop [bars     bars
         trades   trades
         last-ret 1.0
         rets     []
         ix       0]
    (let [bar (first bars)
          {:keys [entry-price entry-index exit-index side] :as trade}
          (first trades)

          ret    (if (and trade
                          (and (> ix entry-index) (<= ix exit-index)))
                   (* (get rets entry-index)
                      (ratio-return side entry-price (:close bar)))
                   last-ret)
          trades (if (= ix exit-index) (rest trades) trades)]
      (if-let [bars (next bars)]
        (recur bars trades ret (conj rets ret) (inc ix))
        (conj rets ret)))))

(defn max-drawdown-over-bars [bars trades]
  (max-drawdown (cash-flow bars trades)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tools and charts to analyze result sets;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gauntlet [{:keys [trades bars] :as strat-result}]
  {:total-return (total-return trades)
   :mean-return  (mean (map ratio-profit trades))
   :sd-return    (standard-deviation (map ratio-profit trades))
   :win-rate     (win-rate trades)
   :trade-ct     (count trades)
   :sharpe       (sharpe-ratio trades 1.01)
   :max-drawdown (max-drawdown-over-bars bars trades)})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Repl utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;https://github.com/semperos/rankle/blob/master/src/com/semperos/rankle/util.clj
(defn print-table-neat
  "Visually calmer version of clojure.pprint/print-table"
  ([ks rows]
   (when (seq rows)
     (let [widths  (map
                    (fn [k]
                      (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                    ks)
           spacers (map #(apply str (repeat % "-")) widths)
           fmts    (map #(str "%" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (format fmt (str col)))))
                          trailer))]
       (println)
       #_(println (fmt-row "| " " | " " |" (zipmap ks ks)))
       (println (fmt-row "  " "   " "  " (zipmap ks ks)))
       #_(println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (println (fmt-row "--" "- -" "--" (zipmap ks spacers)))
       (doseq [row rows]
         #_(println (fmt-row "| " " | " " |" row))
         (println (fmt-row "  " "   " "  " row))))))
  ([rows]
   (print-table-neat (keys (first rows)) rows)))

(defn format-row [row]
  (m/map-vals #(cond->> % (float? %) (format "%.3f")) row))

(defn print-result-table [label-key rows]
  (let [cols (->> rows first keys (remove #{label-key}) (cons label-key))]
    (print-table-neat cols (map format-row rows))))
