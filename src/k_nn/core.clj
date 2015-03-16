(ns k-nn.core
  (:import (com.evolvingneuron KDTree)
           (com.evolvingneuron IKDTree)))

;is there a better way to expose constants like this? just pass them?
;keyword args? This seems cleanest, but it is hardly any good for
;parallelism
(def partition-const 30)

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
           (sq-euclidean-distance (:features a) (:features b))))
    (getDimensions [this point]
      (count (:features point)))
    (getDimensionValue [this dimension point]
      (nth (:features point) dimension))
    (setDimensionValue [this dimension value point]
      {:class (:class point)
       :features (assoc (:features point) dimension value)})))

(defn prepare [dataset]
  ;build the KDTree to use later
  ;map an insert over each data point? seems easy
  (let [kdtree (new KDTree interface)]
    (doall (map #(.insert kdtree %) dataset))
    kdtree))

;nearest neighbors
;return the classification of the neighbors as a list
;may be able to factor this out...
(defn nn [k distances]
  (take k (sort-by :distance distances)))

;dataset is a list of features and their classifications
;dataset looks like:
;({:class class :features (feature feature feature)})
;input is a list of features with no classification.
(defn classify [k ^KDTree kdtree input]
  ;use KDTree to get the 1-nn for quick testing
  ;expand to find k-nn later on...
  (.closest kdtree input))
