(ns trateg.core
  (:require 
   [cheshire.core :as json]
   [cheshire.generate :as json-gen]
   [clojure.java.shell :as shell]
   [medley.core :as m]
   [clojure.java.io :as io]))


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

