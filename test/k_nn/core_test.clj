(ns k-nn.core-test
  (:require [clojure.test :refer :all]
            [k-nn.core :refer :all]
            [clojure.string :as s]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(defn format-csv-data [filename]
  (map #(assoc {}
               :features (map (fn [x] (Float/parseFloat x)) (drop-last %))
               :class (Integer/parseInt (last %)))
       (map #(s/split % #",")
            (s/split (slurp filename) #"\n"))))

(defn trial []
  (let [d (format-csv-data "iris-data.csv")
        tests (format-csv-data "iris-test-data.csv")
        extremes (find-extremes d)
        subspaces (define-subspaces extremes 4)
        dataset (update-subspaces d subspaces)]
    (doall (map (fn [test]
                  (println test)
           (println (classify 1 dataset subspaces (:features test)))
           (println "Expected:" (:class test)))
         tests))))
