(ns ta.model.chart
  (:require 
   [clojure.walk :refer [prewalk]]
   ;[cheshire.core :as json]
   ;[cheshire.generate :as json-gen]
   [trateg.core :refer :all]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visualization;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;highcharts uses epoch millis for times
;(json-gen/add-encoder java.time.ZonedDateTime
;                      (fn [zdt gen] (.writeNumber gen (-> zdt .toInstant .toEpochMilli str))))


(defn zoned-time-to-epoch-milli [zdt]
  (-> zdt .toInstant .toEpochMilli))


(defn replace-ZonedDateTime 
  "replaces type ZonedDateTime to epoch-with-milliseconds"
  [spec]
  (prewalk
   (fn [x]
      (if (= java.time.ZonedDateTime (type x))
       (zoned-time-to-epoch-milli x)
       x))
   spec))


(defn view-highchart [specs]
  (let [;spec (->> specs json/encode)
        spec-safe (replace-ZonedDateTime specs)
        ;_ (println "safe spec: " spec-safe)
        ]
    ^:R [:highchart spec-safe]))


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
               :data         (map (juxt :date :open :high :low :close :volume) bars)
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
               :data         (->> bars (map (juxt :date indicator-key)))
               :yAxis        1
               :dataGrouping {:enabled false}}]}))



(defn performance-chart [{:keys [trades bars] :as result}]
  (let [bars       bars
        price-data     (mapv (juxt :date :open :high :low :close :vol) bars)
        ixs            (mapv :date bars)
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
