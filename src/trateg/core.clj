(ns trateg.core
  (:require [cheshire.core :as json]
            [cheshire.generate :as json-gen]
            [clojure.java.shell :as shell]
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
;;data shapes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn row-maps->column-map [row-maps]
  (let [header (-> row-maps first keys)]
    (reduce (fn [acc row]
              (reduce-kv
               (fn [acc column-name value]
                 (update acc column-name conj value))
               acc
               row))
            (apply hash-map (interleave header (repeat [])))
            row-maps)))

(defn column-map->rows [cm]
  (reduce-kv
   (fn [rs column values]
     (map (fn [row value] (conj row value)) rs values))
   (repeat (count (val (first cm))) [])
   cm))

(defn column-map->row-maps [cm]
  (let [header (keys cm)
        rows   (column-map->rows cm)]
    (mapv (partial zipmap header) rows)))

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
;;Run strategy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-strat [bars handler]
  (assert (vector? bars))
  (let [bars (vec (doall (map-indexed (fn [i x] (assoc x :index i)) bars)))]
    (reduce
     (fn [ctx bar]
       (handler (assoc ctx :current-bar bar)))
     {:trades []
      :bars   bars}
     bars)))

(defn default-entry [bar]
  {:entry-price (:close bar)
   :entry-time  (:end-zdt bar)
   :entry-index (:index bar)
   :side        :long})

(defn default-exit [bar]
  {:exit-price (:close bar)
   :exit-time  (:end-zdt bar)
   :exit-index (:index bar)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Handlers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn enforce-single-position [ctx]
  (cond-> ctx
    (and (:position ctx) (:pending-entry ctx)) (dissoc :pending-entry)))

(defn enforce-no-entry-on-exit-bar [ctx]
  (cond-> ctx
    (and (:pending-entry ctx) (:pending-exit ctx)) (dissoc :pending-entry)))

(defn hold-for-exit [{:keys [position current-bar] :as ctx} n]
  (cond-> ctx
    ;;(and position (= (:bars-held position) n))
    (and position (= (:index current-bar) (+ n (:entry-index position))))
    ;;what if pending exit already exists?
    (update :pending-exit merge (default-exit current-bar))))

(defn execute-pending [ctx]
  (let [{:keys [position pending-entry pending-exit current-bar] :as ctx} ctx]
    (cond-> ctx
      pending-entry (-> (assoc :position pending-entry)
                        (dissoc :pending-entry))
      pending-exit  (-> (update :trades conj (merge position pending-exit))
                        (dissoc :pending-exit :position)))))

(defn cross-trigger
  ([ctx direction bar-key threshold action]
   (cross-trigger ctx direction  bar-key threshold action nil))
  ([ctx direction bar-key threshold action side]
   (let [{:keys [current-bar]} ctx
         {:keys [close index]} current-bar
         x                     (get current-bar bar-key)
         last-x                (-> ctx :bars (get (dec index)) bar-key)
         threshold             (if (keyword? threshold)
                                 (get current-bar threshold) threshold)
         cross?                (if (= :under direction)
                                 (and (<  x threshold) last-x (> last-x threshold))
                                 (and (>  x threshold) last-x (< last-x threshold)))]

     (cond-> ctx
       (and cross? (= action :enter))
       (assoc :pending-entry (merge (default-entry current-bar)
                                    {:side side}))

       (and (:position ctx) cross? (= action :exit))
       (assoc :pending-exit (default-exit current-bar))))))

(defn set-exit [ctx trade-key exit-key exit-fn]
  (cond-> ctx
    (contains? ctx trade-key) (assoc-in [trade-key exit-key] (exit-fn ctx))))

(defn initial-stop [ctx stop-fn]
  (set-exit ctx :pending-entry :stop stop-fn))

(defn initial-tp [ctx tp-fn]
  (set-exit ctx :pending-entry :tp tp-fn))

(defn record-exit-orders [ctx]
  (-> ctx
      (update :stops
              (fnil conj [])
              [(-> ctx :current-bar :end-zdt)
               (or (some-> ctx :position :stop)
                   (some-> ctx :pending-entry :stop)
                   (some-> ctx :pending-exit :stop))])
      (update :tps
              (fnil conj [])
              [(-> ctx :current-bar :end-zdt)
               (or (some-> ctx :position :tp)
                   (some-> ctx :pending-entry :tp)
                   (some-> ctx :pending-exit :tp))])))

(defn check-exits [ctx]
  (let [{{:keys [side tp stop] :as pos} :position
         {:keys [high low] :as bar}     :current-bar} ctx]
    (if pos
      (cond-> ctx
        (and stop
             (or (and (= :long side) (<= low stop))
                 (and (= :short side) (>= high stop))))
        (update :pending-exit merge
                (default-exit bar)
                {:exit-price stop})
        
        (and tp
             (or (and (= side :long) (>= high tp))
                 (and (= side :short) (<= low tp))))
        (update :pending-exit merge
                (default-exit bar)
                {:exit-price tp}))
      ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visualization;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;highcharts uses epoch millis for times
(json-gen/add-encoder java.time.ZonedDateTime
                      (fn [zdt gen] (.writeNumber gen (-> zdt .toInstant .toEpochMilli str))))

(defn browse-file [f]
  (let [open-script (if (-> "os.name" System/getProperty #{"Linux"})
                      "xdg-open"
                      "open")]
    (-> (shell/sh open-script (str f)) :err empty?)))

(defn view-highchart [opts]
  (let [html-base (slurp (io/resource "highcharts/base.html"))
        temp-dir  (java.io.File. "tmp-charts")
        _         (when-not (.exists temp-dir) (.mkdir temp-dir))
        temp-file (doto (java.io.File/createTempFile
                         "highcharts"
                         ".html" temp-dir)
                    .deleteOnExit)]
    (->> opts
         json/encode
         (format html-base)
         (spit temp-file))
    (browse-file temp-file)))

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

(defn trade-chart [{:keys [trades bars stops tps] :as results} indicator-key]
  (view-highchart
   {:rangeSelector {:selected 1}
    :chart         {:height 600}
    :navigator     {:enabled true}
    :xAxis         {:plotBands (for [{:keys [side] :as trade} trades]
                                 {:color
                                  (cond
                                    (and (= side :long)
                                         (win? trade))
                                    "rgba(0, 0, 255, 0.50)"
                                    (= side :long)
                                    
                                    "rgba(0, 0, 255, 0.10)"
                                    (and (= side :short)
                                         (win? trade))
                                    "rgba(255, 0, 0, 0.50)"
                                    (= side :short)
                                    "rgba(255, 0, 0, 0.10)")
                                  :from (:entry-time trade)
                                  :to   (:exit-time trade)})
                    :crosshair {:snap true}}
    :yAxis
    [{:height    "40%"
      :crosshair {:snap false}}
     {:height    "40%"
      :top       "50%"
      :crosshair {:snap false}
      :plotLines [{:value     30
                   :color     "blue"
                   :width     2
                   :dashStyle "shortdash"}
                  {:value     70
                   :color     "red"
                   :width     2
                   :dashStyle "shortdash"}]}]

    :tooltip {:split true :shared true}
    :series  [{:type         "candlestick"
               :name         "price"
               :data         (map (juxt :end-zdt :open :high :low :close :volume) bars)
               :id           "priceseries"
               :dataGrouping {:enabled false}}
              (when stops
                {:type         "line"
                 :name         "stop"
                 :data         stops
                 :dataGrouping {:enabled false}
                 :yAxis        0
                 :color        "black"})
              (when tps {:type         "line"
                         :name         "profit target"
                         :data         tps
                         :dataGrouping {:enabled false}
                         :yAxis        0
                         :color        "black"})
              {:type         "line"
               :name         (name indicator-key)
               :linkedTo     "priceseries"
               :data         (->> bars (map (juxt :end-zdt indicator-key)))
               :yAxis        1
               :dataGrouping {:enabled false}}]}))

(defn performance-chart [{:keys [trades bars] :as result}]
  (let [bars       bars
        price-data     (mapv (juxt :end-zdt :open :high :low :close :vol) bars)
        ixs            (mapv :end-zdt bars)
        cash-flow      (cash-flow bars trades)
        cash-flow-data (map vector ixs cash-flow)
        peaks          (reductions max cash-flow)
        drawdowns      (map (fn [p x] (/ (- p x) p))
                            peaks
                            cash-flow)
        max-drawdowns  (reductions max drawdowns)
        drawdowns-data     (map vector ixs drawdowns)
        max-drawdowns-data (map vector ixs max-drawdowns)]
    (view-highchart
     {:rangeSelector {:enabled false}
      :chart         {:height 600}
      :navigator     {:enabled false}
      :scrollbar     {:enabled false}
      :yAxis         [{:lineWidth 1
                       :title     {:text "Price"}}
                      {:lineWidth 1
                       :title     {:text "Returns"}
                       :opposite  false}]
      :series        [{:type         "line"
                       :name         "price"
                       :id           "priceseries"
                       :data         price-data
                       :dataGrouping {:enabled false}
                       :zIndex       2
                       :yAxis        0
                       :color        "#000000"}
                      {:type         "area"
                       :name         "return"
                       :data         cash-flow-data
                       :yAxis        1
                       :dataGrouping {:enabled false}
                       :zIndex       0
                       :color        "#0000ff"
                       :fillOpacity  0.3}
                      {:type         "area"
                       :name         "drawdown"
                       :data         drawdowns-data
                       :color        "#ff0000"
                       :fillOpacity  0.5
                       :yAxis        1
                       :zIndex       1
                       :dataGrouping {:enabled false}}
                      {:type         "line"
                       :name         "max drawdown"
                       :data         max-drawdowns-data
                       :color        "#800000"
                       :yAxis        1
                       :zIndex       1
                       :dataGrouping {:enabled false}}]})))

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
