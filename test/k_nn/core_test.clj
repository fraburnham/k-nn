(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s]))

(defn format-csv-data [filename]
  (map #(assoc {}
               :features (map (fn [x] (Float/parseFloat x)) (drop-last %))
               :class (Integer/parseInt (last %)))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

;from http://clojure-doc.org/articles/language/functions.html
(defn round
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/floor (* d factor)) factor)))

(def dataset (format-csv-data "test-data/iris-data.csv"))
(def tests (format-csv-data "test-data/iris-test-data.csv"))

;this test proves that the math functions that power it are working
;and cooperating. I'm not really sure what the best way to test the
;individual functions would be.
(deftest euclidean-distance-test
  (testing "Euclidean distance vs http://calculator.vhex.net/post/calculator-result/euclidean-distance"
      (is (= 5.196152 (round (euclidean-distance '(5 7 8 2) '(1 8 5 3)) 6)))))

(deftest iris-test
  (testing "1-nn on famous iris data"
    (doall (pmap (fn [test]
                     (is (= (:class (classify 1 dataset (:features test)))
                           (:class test))))
                   tests))))
