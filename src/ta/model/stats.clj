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

(defn nominal-profit [{:keys [px-entry px-exit side] :as trade}]
  (nominal-return side px-entry px-exit))

(defn ratio-profit [{:keys [side px-entry px-exit] :as trade}]
  (ratio-return side px-entry px-exit))

(defn trade-complete? [trade] (-> trade :px-exit some?))

(defn win? [{:keys [side px-entry px-exit] :as trade}]
  (when (and px-entry px-exit)
    (case side
      :long  (> px-exit px-entry)
      :short (< px-exit px-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculations on multiple roundtrips;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn win-rate [roundtrips]
  (if (empty? roundtrips)
    0
    (double
     (/ (count (filter win? roundtrips))
        (count roundtrips)))))

(defn total-return [roundtrips]
  (->> roundtrips
       (map ratio-profit)
       (reduce *)))

(defn profit-factor [roundtrips]
  (let [{:keys [profit loss]}
        (reduce (fn [acc {:keys [px-entry px-exit] :as trade}]
                  (if (trade-complete? trade)
                    (let [win?  (win? trade)
                          loss? (not win?)]
                      (cond
                        win?  (update acc :profit + (nominal-profit trade))
                        loss? (update acc :loss + (nominal-profit trade))))
                    acc))
                {:profit 0 :loss 0}
                roundtrips)]
    (/ profit (Math/abs loss))))

(defn mean-return [roundtrips]
  (mean (map ratio-profit roundtrips)))

(defn mean-nominal-return [roundtrips]
  (mean (map nominal-profit roundtrips)))

(defn sharpe-ratio [roundtrips risk-free-rate]
  (/ (- (total-return roundtrips) risk-free-rate)
     (standard-deviation (map ratio-profit roundtrips))))

(defn max-drawdown [returns]
  (let [peaks (reductions max returns)]
    (reduce max (map (fn [p x] (/ (- p x) p)) peaks returns))))

(defn cash-flow-over-roundtrips [roundtrips]
  (reductions * 1 (map ratio-profit roundtrips)))

(defn max-drawdown-over-roundtrips [roundtrips]
  (max-drawdown (cash-flow-over-roundtrips roundtrips)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculations on roundtrips and bars;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cash-flow [bars roundtrips]
  (loop [bars     bars
         roundtrips   roundtrips
         last-ret 1.0
         rets     []
         ix       0]
    (let [bar (first bars)
          {:keys [px-entry idx-entry idx-exit side] :as trade}
          (first roundtrips)

          ret    (if (and trade
                          (and (> ix idx-entry) (<= ix idx-exit)))
                   (* (get rets idx-entry)
                      (ratio-return side px-entry (:close bar)))
                   last-ret)
          roundtrips (if (= ix idx-exit) (rest roundtrips) roundtrips)]
      (if-let [bars (next bars)]
        (recur bars roundtrips ret (conj rets ret) (inc ix))
        (conj rets ret)))))

(defn max-drawdown-over-bars [bars roundtrips]
  (max-drawdown (cash-flow bars roundtrips)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tools and charts to analyze result sets;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gauntlet [{:keys [roundtrips bars] :as strat-result}]
  {:total-return (total-return roundtrips)
   :mean-return  (mean (map ratio-profit roundtrips))
   :sd-return    (standard-deviation (map ratio-profit roundtrips))
   :win-rate     (win-rate roundtrips)
   :trade-ct     (count roundtrips)
   :sharpe       (sharpe-ratio roundtrips 1.01)
   :max-drawdown (max-drawdown-over-bars bars roundtrips)})


(defn gauntlet2 [{:keys [roundtrips] :as strat-result}]
  {:total-return (total-return roundtrips)
   :mean-return  (mean (map ratio-profit roundtrips))
   :sd-return    (standard-deviation (map ratio-profit roundtrips))
   :win-rate     (win-rate roundtrips)
   :trade-ct     (count roundtrips)
   :sharpe       (sharpe-ratio roundtrips 1.01)
   ;:max-drawdown (max-drawdown-over-bars bars roundtrips)
   })


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
