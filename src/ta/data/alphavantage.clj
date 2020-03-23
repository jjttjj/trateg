(ns ta.data.alphavantage
   (:require
     [clj-http.client :as client]
     [cheshire.core] ; JSON Encoding
     [clj-time.core :as t]
     [clj-time.format :as fmt]
     ))

;; https://www.alphavantage.co/documentation/#

; 5 API requests per minute
; 500 requests per day

;; Fingerprint Cards (STO: FING-B) == FING-B.STO
;; Novo Nordisk (CPH: NOVO-B) == NOVO-B.CPH
;; BMW (DE: BMW) == BMW.DE


;; https://github.com/RomelTorres/alpha_vantage/issues/13

(def api-key (atom "demo"))


(defn set-key! 
  "to use alphavantage api, call at least once set-key! api-key"
  [key]
  (reset! api-key key)
  nil ; Important not to return by chance the key, as this would be shown in the notebook.
  )


(defn get-av [params]
  (-> (client/get "https://www.alphavantage.co/query"
                 {:accept :json
                  :query-params (assoc params :apikey @api-key)})
      (:body)
      (cheshire.core/parse-string true)))


(defn fix-keywords [m]
  (let [ks (keys m)
        vs (vals m)]
    (zipmap (map #(keyword (second (re-find #"\d+.\s(\w+)" (name %)))) ks) vs)))


(defn search [keywords]
  (->> (get-av {:function "SYMBOL_SEARCH" 
                :keywords keywords})
       :bestMatches
       (map fix-keywords)
       (into [] )
       ))


(def MetaData (keyword "Meta Data"))
(def TimeSeriesDaily (keyword "Time Series (Daily)"))
(def TimeSeriesFXDaily (keyword "Time Series FX (Daily)"))
(def TimeSeriesDigitalCurrencyDaily (keyword "Time Series (Digital Currency Daily)"))



(def bar-format-standard {
  :open (keyword "1. open")
  :high (keyword "2. high")
  :low (keyword "3. low")
  :close (keyword "4. close")
  :volume (keyword "5. volume")
  })

(def bar-format-crypto {
    :open (keyword "1a. open (USD)")
    :high (keyword "2a. high (USD)")
    :low (keyword "3a. low (USD)")
    :close (keyword "4a. close (USD)")
    :volume (keyword "5. volume")
    :marketcap (keyword "6. market cap (USD)")
    })


(def row-date-format-
  (fmt/formatter "yyyy-MM-dd")) ; 2019-08-09

(defn as-float [str]
  (if (nil? str)
      nil
      (Float/parseFloat str)))

(defn convert-bar [bar-format volume? item]
  (let [bars (second item)
        bar {:date (fmt/parse row-date-format-(subs (str (first item)) 1))
             :open (as-float ((:open bar-format) bars))
             :high (as-float ((:high bar-format) bars))
             :low (as-float ((:low bar-format) bars))
             :close (as-float ((:close bar-format) bars))
             }]
   (if volume?
       (assoc bar :volume (as-float ((:volume bar-format) bars)))
       bar)))

(defn convert-bars [request-type response]
  (let [bar-format (if (= request-type TimeSeriesDigitalCurrencyDaily) bar-format-crypto bar-format-standard)
        volume? (= request-type TimeSeriesDaily)
        ;_ (println "Bar format: " bar-format)
         ]
    (->> response
       (request-type)
       (seq)
       ;(first)
       ;(convert-bar)
       (map (partial convert-bar bar-format volume?))
       (sort-by :date) 
    )))


(defn get-daily
  "size: compact=last 100 days. full=entire history"
  [size symbol]
  (->> (get-av {:function "TIME_SERIES_DAILY"
               :symbol symbol
               :outputsize (name size)
               :datatype "json"})
       (convert-bars TimeSeriesDaily)))

(defn get-daily-fx
  "size: compact=last 100 days. full=entire history"
  [size symbol]
  (->> (get-av {:function "FX_DAILY"
               :from_symbol (subs symbol 0 3)
               :to_symbol (subs symbol 3)
               :outputsize (name size)
               :datatype "json"})
       (convert-bars TimeSeriesFXDaily)))


(defn get-daily-crypto
  "size: compact=last 100 days. full=entire history"
  [size symbol]
  (->> (get-av {:function "DIGITAL_CURRENCY_DAILY"
               :symbol symbol
               :market "USD"
               :outputsize (name size)
               :datatype "json"})
      (convert-bars TimeSeriesDigitalCurrencyDaily)))

(defn get-crypto-rating
  "size: compact=last 100 days. full=entire history"
  [symbol]
  (get-av {:function "CRYPTO_RATING"
                :symbol symbol
                :datatype "json"}))


(comment

  (search "BA")

  (def b (get-daily "MSFT"))
  (keys b)
  (MetaData b)
  (keys (TimeSeriesDaily b))
  (vals (TimeSeriesDaily b))

  (->> b
      (convert-bars)
      (clojure.pprint/print-table [:date :open :high :low :close :volume])
      )

   (def symbols ["BTC" "ETH" "LTC" "DASH" "NANO" "EOS" "XLM"])

   (get-daily :compact "MSFT")
   (get-daily-fx :compact "EURUSD")
   (get-daily-crypto :compact "BTC")

)