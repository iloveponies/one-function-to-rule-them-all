(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 2)
    a-seq
    (let [acc [(first a-seq) x (second a-seq)]]
    (reduce (fn [a b] (conj (conj a x) b)) acc (drop 2 a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b] [(min (first a) b) (max (second a) b)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (loop [i 0]
    (if (or (> i (- (count sorted-seq) 1)) (> (nth sorted-seq i) n))
      (concat (take i sorted-seq) [n] (drop i sorted-seq))
      (recur (inc i)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

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
