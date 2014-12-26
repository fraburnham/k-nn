(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

;alright, some rundown
;pull in the data from iris-data.csv

(defn format-csv-data [filename]
  (map #(assoc {}
               :features (map (fn [x] (Float/parseFloat x)) (drop-last %))
               :class (Integer/parseInt (last %)))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

(defn trial []
  (let [dataset (format-csv-data "iris-data.csv")
        tests (format-csv-data "iris-test-data.csv")]
    (map (fn [test]
           (println (classify 5 dataset (:features test)))
           (println "Expected:" (:class test)))
         tests)))
