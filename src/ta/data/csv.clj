(ns ta.data.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalTime ZonedDateTime ZoneId]
           java.time.format.DateTimeFormatter))

(def date-fmt (DateTimeFormatter/ofPattern "MM/dd/yyyy"))

(def EST (ZoneId/of "America/New_York"))


(defn load-csv [file]
  (csv/read-csv (io/reader (io/resource file))))

;(def csv (csv/read-csv (io/reader (io/resource "trateg/spx.csv"))))

(defn header-csv [file] 
  (->> (load-csv file) 
       first 
       (map (comp keyword str/lower-case))))

(defn load-csv-bars [file]
  (let [csv (load-csv file) ]
  (for [[date time open high low close volume oi] (rest csv)]
    {:end-zdt (ZonedDateTime/of (LocalDate/parse date date-fmt)
                                (LocalTime/parse time)
                                EST)
     :open    (Double/parseDouble open)
     :high    (Double/parseDouble high)
     :low     (Double/parseDouble low)
     :close   (Double/parseDouble close)
     :volume  (Double/parseDouble volume)})))


