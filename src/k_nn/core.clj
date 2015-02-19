(ns k-nn.core)

;is there a better way to expose constants like this? just pass them?
;keyword args? This seems cleanest, but it is hardly any good for
;parallelism
(def partition-const 30)

(defn abs [x]
  (if (neg? x) (- 0 x) x))

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

;look through the neighborhood to find the extremes in all dimensions
;return [[-x-extreme x-extreme][-y-extreme y-extreme][etc]]
(defn find-extremes [dataset]
  (map (fn [i]
         ((fn [vals]
            [(apply min vals)
             (apply max vals)])
           (map #(nth (:features %) i) dataset)))
       (range (count (:features (first dataset))))))

;chances are these loops that build a return and use an index
;can be made in a more idomatic fashion...
;this may be an iterate and take...
;look at that! follow the idiom and the code is more concise
(defn division-lines [divisions extreme division-size]
  (take (inc divisions) (iterate #(+ % division-size) extreme)))

;using the extreme points in each dimension as the outer bound of space
;find the supplied number of subspace sections and return the labeled
;subspaces and their bounds
(defn define-subspaces [extremes divisions]
  (let [extreme-distances (map (comp abs (partial apply -)) extremes)
        division-sizes (map #(/ % divisions) extreme-distances)]
    (map (partial division-lines divisions)
         (map first extremes) division-sizes)))

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
  (map #(assoc % :subspace (group-point subspaces %)) dataset))

;dataset contains only points in the same subspace as the input point
(defn subspace-distances [dataset input]
  (flatten
    (pmap #(map (fn [point]
                  {:distance (euclidean-distance input (:features point))
                   :class (:class point)
                   :features (:features point)})
                %)
          (partition-all partition-const dataset))))

;nearest neighbors
;return the classification of the neighbors as a list
(defn nn [k distances]
  (take k (sort-by :distance distances)))

(defn same-subspace? [input-subspace point]
  (= input-subspace (:subspace point)))

(defn adjust-subspace [dimension s-space i-space input closest]
  (let [min-distance (nth s-space (let [r (dec i-space)]
                                    (if (neg? r) 0 r)))
        max-distance (nth s-space i-space)
        input-distance (abs (- input closest))]
    (cond (> input-distance min-distance) (let [r (dec i-space)]
                                            (if (neg? r) 0 r))
          (> input-distance max-distance) (let [r (inc i-space)]
                                            (if (>= r (count dimension)) i-space r))
          :else i-space)))

;return a new subspace dataset that has points from all adjacent subspaces
(defn expand-subspace [dataset subspaces input-subspace]

  )

(defn distances [dataset subspaces input]
  ;maybe make this a loop target so the input-subspace can be adjusted
  ;as needed
  (let [input-subspace (group-point subspaces {:features input})
        subspace-dataset (filter (partial same-subspace? input-subspace) dataset)
        d (subspace-distances subspace-dataset input)
        [closest] (nn 1 d)
        adjusted-subspace (map (partial adjust-subspace (count subspaces))
                               subspaces input-subspace input (:features closest))]
    ;now check if there are enough points in this subspace (count subspace-dataset)
    ;if it is less than k then the search space needs to be expanded (this should be done
    ;in every possible dimension, since nothing of value is learned
    ))

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
