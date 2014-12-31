(ns k-nn.core)

;is there a better way to expose constants like this? just pass them?
;keyword args? This seems cleanest, but it is hardly any good for
;parallelism
(def partition-const 30)

;not gonna use pmap here, k-nn shouldn't have more than 10 dimensions
;the overhead of setting up a thread to add ten numbers is not worth it
(defn sum [f & rest]
  (reduce +' (apply (partial map f) rest)))

(defn sq-diff [a b]
  (Math/pow (- a b) 2))

;return the euclidean distance between points a and b
(defn euclidean-distance [a b]
  (Math/sqrt (sum sq-diff a b)))

(defn sq-euclidean-distance [a b]
  (sum sq-diff a b))

;map (makes a lazy seq, right?) over the dataset and calculate the distance
;each point is from the input, return the distance and classification of the
;point whose distance is measured
(defn distances [dataset input]
  (flatten
    (pmap #(map (fn [point]
                  {:distance (euclidean-distance input (:features point))
                   :class (:class point)})
                %)
          (partition-all partition-const dataset)))
  #_(map (fn [point]
         {:distance (euclidean-distance input (:features point))
          :class (:class point)})
       dataset))

;nearest neighbors
;return the classification of the neighbors as a list
(defn nn [k distances]
  (take k (sort-by :distance distances)))

;dataset is a list of features and their classifications
;dataset looks like:
;({:class class :features (feature feature feature)})
;input is a list of features with no classification.
(defn classify [k dataset input]
  (let [[class count] ;class and number of neighbors of that class
         (first
           (sort-by second >
                    (frequencies
                      (map :class (nn k (distances dataset input))))))]
    {:class class
     :certainty (/ count k)}))
