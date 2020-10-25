(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (reduce #(if (empty? %1)
             (conj %1 %2)
             (conj %1 x %2)) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] x] [(min mn x) (max mx x)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (loop [acc []
         s sorted-seq]
    (if (or (empty? s) (< n (first s)))
      (concat (conj acc n) s)
      (recur (conj acc (first s)) (rest s)))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [set elm]
                 (if (contains? set elm)
                   (disj set elm)
                   (conj set elm)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and [& preds]
  (fn [x]
    (loop [ps preds]
      (cond (empty? ps) true
            ((complement (first ps)) x) false
            :else (recur (rest ps))))))

(defn my-map [f & seqs]
  (let [simple-map #(reduce (fn [a x] (conj a (%1 x))) [] %2)
        heads #(simple-map first %1)
        tails #(simple-map rest %1)
        any-empty? #(boolean (some empty? %1))]
    (loop [s seqs
           acc []]
      (if (any-empty? s)
        acc
        (recur (tails s) (conj acc (apply f (heads s))))))))
  