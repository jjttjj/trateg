(ns trateg.rank)

(defn rank-input [data field-in index]
  (map
   (fn [[symbol data-symbol]]
     (let [ts-field-in (get data-symbol field-in)
           field-val (nth ts-field-in index)
           ;_ (println [symbol field-val])
           ]
       [symbol field-val])) data))

(defn to-map [ranked]
  (let [m (atom {})]
    (doall (map (fn [[key data]]
           ;(println "key: " key " data: " data)
           (swap! m assoc key data)) 
         ranked))
    @m))


(defn rank-calc [data field-in ascending? index]
  (let [ranks (rank-input data field-in index)
        ranks-sorted (sort-by #(get % 1) ranks)
        ranks-sorted (if ascending? ranks-sorted (reverse ranks-sorted))
        ;_ (println "sorted: " ranks-sorted)
        ranked (map-indexed
                (fn [idx itm] [(get itm 0) [(get itm 1) (+ idx 1)]])
                ranks-sorted)
        ;_ (println "ranked: " ranked)
        ]
    (to-map ranked)))
;; => #'tradeg.crosssection/rank-calc

(defn rank-series-for-key [rank-list symbol]
  (let [extract-array (fn [ranking] (symbol ranking))
        extract-rank (fn [ranking] (get (symbol ranking)1 ))]
    (into [] 
          (map-indexed
           (fn [idx itm] (extract-rank itm))
           rank-list)
          )))

(defn rank [data length field-in ascending? field-out]
  (let [rank-list (map (fn [index]
                         (rank-calc data field-in ascending? index))
                       (range length))]
      (to-map 
       (doall 
        (map (fn [[symbol d]]
               ;(println symbol "data:" d)
               (let [rank-ts (rank-series-for-key rank-list symbol)]
                 [symbol (assoc d field-out rank-ts)]
                 ))
             data))
       )))

