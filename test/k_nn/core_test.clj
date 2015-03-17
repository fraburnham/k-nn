(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s])
  (:import (com.evolvingneuron KDNode)))

(defn format-csv-data [filename]
  (map #(assoc {}
         :features
         (float-array
           (map (fn [x] (Float/parseFloat x)) (drop-last %)))
         :class (Integer/parseInt (last %)))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

(def dataset (format-csv-data "test-data/iris-data.csv"))
(def tests (format-csv-data "test-data/iris-test-data.csv"))

(deftest iris-test
  (testing "1-nn on famous iris data"
    (let [kdtree (prepare dataset)]
      (doall (map (fn [test]
                    (is (= (:class (.getValue ^KDNode (classify 1 kdtree test)))
                           (:class test))))
                  tests)))))
