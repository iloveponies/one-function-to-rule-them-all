(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))


(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
   (reduce str (first a-seq) (map (fn [seq] (str " " seq)) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (interpose x a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc x] (cond
                       (and (<= x (get acc 0)) (>= x (get acc 1)))
                       [x x]
                       (<= x (get acc 0))
                       (assoc acc 0 x)
                       (>= x (get acc 1))
                       (assoc acc 1 x)
                       :else
                       acc)) [10000 -100000] a-seq))(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
