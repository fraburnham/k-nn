(ns k-nn.core
  (:require [clojure.pprint])
  (:import (com.evolvingneuron KDTree)
           (com.evolvingneuron IKDTree)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn sum [f & rest]
  (reduce + (apply (partial map f) rest)))

(defn sq-diff [a b]
  (Math/pow (- a b) 2))

(defn euclidean-distance [a b]
  (Math/sqrt (sum sq-diff a b)))

(defn sq-euclidean-distance [a b]
  (sum sq-diff a b))

(def interface
  (reify IKDTree
    (distance [this a b]
      (sq-euclidean-distance (:features a) (:features b)))
    (getDimensions [this point]
      (count (:features point)))
    (getDimensionValue [this dimension point]
      (aget ^floats (:features point) dimension))
    (setDimensionValue [this dimension value point]
      (let [ret {:class (:class point)
                 :features (aclone ^floats (:features point))}]
        (aset ^floats (:features ret) dimension value)
        ret))))

(defn prepare [dataset]
  ;build the KDTree to use later
  ;map an insert over each data point? seems easy
  (let [kdtree (new KDTree interface)]
    (doall (map #(.insert kdtree %) dataset))
    kdtree))

;dataset is a list of features and their classifications
;dataset looks like:
;({:class class :features (feature feature feature)})
(defn classify [k ^KDTree kdtree input]
  ;use KDTree to get the 1-nn for quick testing
  ;expand to find k-nn later on...
  (.closest kdtree input))
