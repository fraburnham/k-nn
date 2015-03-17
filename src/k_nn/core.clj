(ns k-nn.core
  (:import (com.evolvingneuron KDTree)
           (com.evolvingneuron IKDTree)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;is there a better way to expose constants like this? just pass them?
;keyword args? This seems cleanest, but it is hardly any good for
;parallelism
(def partition-const 30)

;read more, not sure why I need both
(defprotocol point-proto
  (get-class [this])
  (get-features [this]))

(deftype data-point [class features]
  point-proto
  (get-class [this] (. this class))
  (get-features [this] (. this features)))

(defn abs [x]
  (if (neg? x) (- 0 x) x))

(defn sum [f & rest]
  (reduce +' (apply (partial map f) rest)))

(defn sq-diff [a b]
  (Math/pow (- a b) 2))

(defn euclidean-distance [a b]
  (Math/sqrt (sum sq-diff a b)))

(defn sq-euclidean-distance [a b]
  (sum sq-diff a b))

(def interface
  (reify IKDTree
    (distance [this a b]
      (new BigDecimal
           ^Double (sq-euclidean-distance (get-features a) (get-features b))))
    (getDimensions [this point]
      (count (get-features point)))
    (getDimensionValue [this dimension point]
      (nth (get-features point) dimension))
    (setDimensionValue [this dimension value point]
      (->data-point (get-class point)
                    (assoc (get-features point) dimension value)))))

(defn prepare [dataset]
  ;build the KDTree to use later
  ;map an insert over each data point? seems easy
  (let [kdtree (new KDTree interface)]
    (doall (map #(.insert kdtree %) dataset))
    kdtree))

;dataset is a list of features and their classifications
;dataset looks like:
;({:class class :features (feature feature feature)})
;input is a list of features with no classification.
(defn classify [k ^KDTree kdtree input]
  ;use KDTree to get the 1-nn for quick testing
  ;expand to find k-nn later on...
  (.closest kdtree input))
