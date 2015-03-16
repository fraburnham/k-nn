(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s]))

;trying out test driven development
;start with making tests for each of the expected interface functions
;Red Green Refactor

(defn format-csv-data [filename]
  (map #(assoc {}
               :features (map (fn [x] (Float/parseFloat x)) (drop-last %))
               :class (Integer/parseInt (last %)))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

(def dataset (format-csv-data "test-data/iris-data.csv"))
(def tests (format-csv-data "test-data/iris-test-data.csv"))

(deftest prepare-test
  (testing "Preparation of data for classify. K-D tree or brute force (no prep)"
    ;need to assert that the tree is returned, write a different test to verify
    ;that the tree is giving the correct data based on the algo passed
    ))

(deftest iris-test
  (testing "1-nn on famous iris data"
    (let [kdtree (prepare dataset)]
      (doall (map (fn [test]
                    (is (= (:class (classify 1 kdtree test))
                           (:class test))))
                  tests)))))
