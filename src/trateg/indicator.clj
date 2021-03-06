(ns trateg.indicator
  (:require [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as rfs]))

(defn sma-xf [n]
  (comp (x/window n rfs/avg #(rfs/avg %1 %2 -1))))

(defn sma [n xs] (into [] (sma-xf n) xs))

(defn ema-xf
  ([n] (ema-xf n (/ 2 (inc n))))
  ([n alpha]
   (comp (x/reductions
          (fn [prev-ema x]
            (if prev-ema
              (-> (- x prev-ema)
                  (* alpha)
                  (+ prev-ema))
              x))
          nil)
         (drop 1))))

(defn ema
  ([n xs]
   (into [] (ema-xf n) xs))
  ([n alpha xs]
   (into [] (ema-xf n alpha) xs)))

;;must be passed [high low close]
(defn stochastic-xf [n]
  (comp (x/window n
                  (fn
                    ([] {:lo (sorted-map-by <)
                         :hi (sorted-map-by >)
                         ;;:cl ##NaN
                         })
                    ([acc]
                     (let [{:keys [hi lo cl]} acc
                           hi                 (-> hi first key)
                           lo                 (-> lo first key)
                           diff               (if (= hi lo)
                                                ##NaN
                                                (- hi lo))]
                       (-> (- cl lo)
                           (/ diff)
                           (* 100))))
                    ([acc x]
                     (let [[high low close] x]
                       (-> acc
                           (assoc :cl close)
                           (update :lo update low (fnil inc 0))
                           (update :hi update high (fnil inc 0))))))
                  
                  (fn [acc out]
                    (let [[high low close] out
                          f                (fn [acc old]
                                             (if (= (get acc old) 1)
                                               (dissoc acc old)
                                               (update acc old dec)))]
                      (-> acc
                          (update :lo f low)
                          (update :hi f high)))))))

(defn stochastic [n xs]
  (into [] (stochastic-xf n) xs))

(defn tr [high low prevcl]
  (max (- high low)
       (Math/abs (- high prevcl))
       (Math/abs (- low prevcl))))

(defn atr [n highs lows closes]
  (->> (map tr (rest highs) (rest lows) closes)
       (cons (- (first highs) (first lows)))
       (ema n (/ 1 n))))

(defn gains [xs]
  (->> (map (fn [curr last]
              (if (> curr last)
                (- curr last)
                0))
            (rest xs)
            xs)
       (cons 0)))

(defn losses [xs]
  (->> (map (fn [curr last]
              (if (> curr last)
                0
                (- last curr)))
            (rest xs)
            xs)
       (cons 0)))

(defn rsi [n xs]
  (let [g (ema n (/ 1 n) (gains xs)) 
        l (ema n (/ 1 n) (losses xs))]
    (map (fn [g l]
           (cond
             (and (zero? l) (zero? g)) 0
             (zero? l)                 100
             :else                     (- 100 (/ 100 (inc (/ g l))))
             ))
         g l)))



