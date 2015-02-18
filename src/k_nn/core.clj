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

;add naive grouping
;it will split the space into equal segments and assign every
;point to the subspace it belongs to. when searching distances
;using something like find-naive-group will find the space in which
;the new point resides. This will allow it to check the distance to fewer particles.
;Once the distance to the closest particle in this space is known check the distance
;to the nearest edge of the subspace. If the edge of subspace is closer, check
;the neighboring space for potentially closer points.

;look through the neighborhood to find the extremes in all dimensions
;return [[-x-extreme x-extreme][-y-extreme y-extreme][etc]]
;TODO: refactor this into a map
(defn find-extremes [dataset]
  (loop [i 0
         extremes []]
    (if (= i (count (:features (first dataset))))
      extremes
      (recur (inc i)
             (conj extremes
                   ((fn [vals]
                      [(apply min vals)
                       (apply max vals)])
                     (map #(nth (:features %) i) dataset)))))))

;chances are these loops that build a return and use an index
;can be made in a more idomatic fashion...
;this may be an iterate and take...
(defn division-lines [divisions extreme division-size]
  (loop [r [extreme]
         i 0]
    (if (= i divisions)
      r
      (recur (conj r (+ (last r) division-size))
             (inc i)))))

;using the extreme points in each dimension as the outer bound of space
;find the supplied number of subspace sections and return the labeled
;subspaces and their bounds
(defn define-subspaces [extremes divisions]
  (let [extreme-distances (map (comp #(Math/abs %) (partial apply -)) extremes)
        division-sizes (map #(/ % divisions) extreme-distances)]
    (map (partial division-lines divisions)
         (map first extremes) division-sizes)))

(defn group-point [subspaces point]
  (map (fn [division-lines p]
         (loop [d division-lines
                i 0]
           (if (or (<= p (first d)) (empty? d))
             (if (empty? d) (dec i) i)
             (recur (rest d) (inc i)))))
       subspaces (:features point)))

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
