(ns trateg.trade
  " backtest on portfolio level

     predicate-exit [model position]
       - exits are executed before entries
       - position has :symbol

     predicate-entry [symbol data]
        if entry: [:symbol priority]   (higher priority is better)
        otherwise: nil
  ")

;; operate on symbol data

(defn get-ts
  "gets a timeseries or timeseries value
   fot a symbol"
  ([model symbol field]
   (get-in @model [:symbols symbol field]))
  ([model symbol field index]
   (get (get-ts model symbol field) index)))

;; Position Management in backtest

(defn initial-portfolio []
  (atom {:balance 100000
         :position-size 20000
         :positions {} ; :a {:symbol :a :side :long :qty 20000 :px-entry 0.5 :idx-enty -1}}
         :trades []
         :roundtrips []}))

(defn open-position [model portfolio symbol side index]
  (let [_ (println "opening " symbol index)
        open-price (get-ts model symbol :price index)
        position-size (:position-size @portfolio)
        qty (int (Math/floor (/ position-size open-price)))
        open-trade {:op :open
                    :symbol symbol
                    :side (if (= :long side) :buy :sell)
                    :qty qty
                    :price open-price
                    :idx index}
        position {:symbol symbol
                  :side side
                  :qty qty
                  :px-entry open-price
                  :idx-entry index}
        {:keys [positions trades]} @portfolio
        new-trades (conj trades open-trade)
        new-positions (assoc positions symbol position)]
    (swap! portfolio assoc
           :trades new-trades
           :positions new-positions)))

(defn close-position [model portfolio position index]
  (let [{:keys [symbol side qty]} position
        _ (println "closing " symbol index)
        close-price (get-ts model symbol :price index)
        close-trade {:symbol symbol
                     :op :close
                     :qty qty
                     :idx index
                     :side (if (= :long side) :sell :buy)
                     :price close-price}
        roundtrip (assoc position :px-exit close-price :idx-exit index)
        {:keys [positions trades roundtrips]} @portfolio
        new-trades (conj trades close-trade)
        new-positions (dissoc positions symbol)
        new-roundtrips (conj roundtrips roundtrip)]
    (swap! portfolio assoc
           :trades new-trades
           :positions new-positions
           :roundtrips new-roundtrips)))


(defn exit-index [model portfolio p-exit index]
  (let [positions (:positions @portfolio)]
    (doall (map (fn [[_ position]]
         ;(println "checking exit-index " index (:symbol position))
                  (when (p-exit model position)
                    (close-position model portfolio position index)))
                positions))))


(defn no-position
  [portfolio symbol]
  (nil?
   (get-in portfolio [:positions symbol])))

(defn entry-index [model portfolio p-entry index]
  (let [symbols (:symbols @model)]
    (doall (remove nil?
                   (map
                    (fn [[symbol data]]
               ;(println "checking entry-index " symbol)
                      (when (no-position portfolio symbol)
                        (let [entry-signal (p-entry symbol data)]
                          (when (not (nil? entry-signal)) entry-signal))))
                    symbols)))))


(defn portfolio-entry-filter [model portfolio entry-signals index]
  (doall
   (map
    (fn [signal]
      (open-position model portfolio (first signal) :long index))
    entry-signals)))


;; backtest loop

(defn trade-index [model portfolio p-exit p-entry index]
  (println "trading " index)
  (exit-index model portfolio p-exit index)
  (let [entry-list (entry-index model portfolio p-entry index)]
    (println "entry list: " entry-list)
    (portfolio-entry-filter model portfolio entry-list index)))


(defn trade [model p-exit p-entry]
  (let [{:keys [length]} @model
        portfolio (initial-portfolio)]
    ; to test opening positions
    ;(open-position model portfolio :b :long 0)
    (doall (map
            (fn [index]
              (trade-index model portfolio p-exit p-entry index))
            (range length)))
    ;@model
    @portfolio))



(comment

  ;; example
  
  (def model- (atom {:length 3
                     :symbols {:a {:price [1 2 3]}
                               :b {:price [4 5 6]}}}))

  (defn exit-always [model position &index]
    (let [symbol (:symbol position)]
      (println "exit-always" symbol)
      true))

  (defn entry-always [symbol data]
    (println "entry-always" symbol)
    [symbol 10])


  (trade model- exit-always entry-always)
;; => {:balance 100000, :positions {:a {:symbol :a, :side :long, :entry 0.5, :qty 20000, :idx-enty -1}}, :trades []}
  )