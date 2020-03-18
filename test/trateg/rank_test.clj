(ns trateg.rank-test
  (:require [clojure.test :refer :all]
            [trateg.rank :refer [rank]]
            ))

(def data-
  {:a {:price [100 105 110]}
   :b {:price [105 110 109]}
   :c {:price [110 104 103]}})

(def expected- 
  {:a {:price [100 105 110], :rank [3 2 1]}
   :b {:price [105 110 109], :rank [2 1 2]}
   :c {:price [110 104 103], :rank [1 3 3]}})


(deftest test-rank
  (is (= expected- (rank data- 3 :price false :rank))))


(comment 

(rank-input data- :price 1)
(rank-calc data- :price false 1)


(to-map [[:b [110 1]] [:a [105 2]] [:c [104 3]]])

(rank-series-for-key
 [{:c [110 1], :b [105 2], :a [100 3]}
  {:b [110 1], :a [105 2], :c [104 3]}
  {:a [110 1], :b [109 2], :c [103 3]}]
 :a)

)

