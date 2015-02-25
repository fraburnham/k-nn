(ns k-nn.core
  (:require [clojure.math.combinatorics :as combo]))

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

;look through the neighborhood to find the extremes in all dimensions
;return [[-x-extreme x-extreme][-y-extreme y-extreme][etc]]
(defn find-extremes [dataset]
  (println "find-extremes time")
  (time
    (map (fn [i]
           ((fn [vals]
              [(apply min vals)
               (apply max vals)])
             (map #(nth (:features %) i) dataset)))
         (range (count (:features (first dataset)))))))

(defn division-lines [divisions extreme division-size]
  (println "division-lines time")
  (time
    (take (inc divisions) (iterate #(+ % division-size) extreme))))

;using the extreme points in each dimension as the outer bound of space
;find the supplied number of subspace sections and return the labeled
;subspaces and their bounds
(defn define-subspaces [extremes divisions]
  (println "define-subspaces time")
  (time
    (let [extreme-distances (map (comp abs (partial apply -)) extremes)
          division-sizes (map #(/ % divisions) extreme-distances)]
      (map (partial division-lines divisions)
           (map first extremes) division-sizes))))

;TODO: make idiomatic
(defn group-point [subspaces point]
  (map (fn [division-lines p]
         (loop [d division-lines
                i 0]
           (if (or (<= p (first d)) (empty? d))
             (if (empty? d) (dec i) i)
             (recur (rest d) (inc i)))))
       subspaces (:features point)))

;caller will have to update subspaces
;TODO: make it complete enough to be one call
(defn update-subspaces [dataset subspaces]
  (println "update-subspaces time")
  (time
    (map #(assoc % :subspace (group-point subspaces %)) dataset)))

;dataset contains only points in the same subspace as the input point
(defn subspace-distances [dataset input]
  (println "subspace-distances time")
  (time
    (flatten
      (map (fn [point]
             {:distance (euclidean-distance input (:features point))
              :class (:class point)
              :features (:features point)})
           dataset))))

;nearest neighbors
;return the classification of the neighbors as a list
(defn nn [k distances]
  (println "nn time")
  (time
    (take k (sort-by :distance distances))))

(defn same-subspace? [input-subspace point]
  (= input-subspace (:subspace point)))

(defn adjust-subspace [dimension s-space i-space input closest]
  (println "adjust-subspace time")
  (time
    (let [min-distance (nth s-space (let [r (dec i-space)]
                                      (if (neg? r) 0 r)))
          max-distance (nth s-space i-space)
          input-distance (abs (- input closest))]
      (cond (> input-distance min-distance) (let [r (dec i-space)]
                                              (if (neg? r) 0 r))
            (> input-distance max-distance) (let [r (inc i-space)]
                                              (if (>= r (count dimension)) i-space r))
            :else i-space))))

;TODO: find a way to expand the search space to include all adjacent subspaces
;return a new subspace dataset that has points from all adjacent subspaces
#_(defn expand-subspace [dataset subspaces input-subspace]
  #_(remove #(= '(1 1) %) (distinct (combo/combinations '(0 0 1 1 2 2) 2)))
  )

(defn distances [dataset subspaces input]
  (println "distances time")
  (time
    (let [input-subspace (group-point subspaces {:features input})
          subspace-dataset (filter (partial same-subspace? input-subspace) dataset)
          ;TODO: improve performance
          ;currently if there aren't enough points in space the algo brute-force
          ;searches the space
          d (if (= (count subspace-dataset) 0) (subspace-distances dataset input)
                                               (subspace-distances subspace-dataset input))
          _ (println (count d) (count subspace-dataset))
          [closest] (nn 1 d)
          adjusted-subspace (map (partial adjust-subspace (count subspaces))
                                 subspaces input-subspace input (:features closest))]
      (if (= input-subspace adjusted-subspace)
        d
        (do
          (println "getting neighbor subspace")
          (concat d
                  (subspace-distances (filter (partial same-subspace? input-subspace) dataset) input)))))))

;dataset is a list of features and their classifications
;dataset looks like:
;({:class class :features (feature feature feature)})
;input is a list of features with no classification.
(defn classify [k dataset subspaces input]
  (let [[class count] ;class and number of neighbors of that class
         (first
           (sort-by second >
                    (frequencies
                      (map :class (nn k (distances dataset subspaces input))))))]
    {:class class
     :certainty (/ count k)}))
