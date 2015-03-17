(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s])
  (:import (com.evolvingneuron KDNode)))

;trying out test driven development
;start with making tests for each of the expected interface functions
;Red Green Refactor

(defn format-csv-data [filename]
  (map (fn [z]
         (->data-point
           (Integer/parseInt (last z))
           (mapv (fn [x] (new BigDecimal (Float/parseFloat x))) (drop-last z))))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

(def dataset (format-csv-data "test-data/iris-data.csv"))
(def tests (format-csv-data "test-data/iris-test-data.csv"))

(deftest iris-test
  (testing "1-nn on famous iris data"
    (let [kdtree (prepare dataset)]
      (doall (pmap (fn [test]
                    (is (= (get-class (.getValue ^KDNode (classify 1 kdtree test)))
                           (get-class test))))
                  tests)))))
