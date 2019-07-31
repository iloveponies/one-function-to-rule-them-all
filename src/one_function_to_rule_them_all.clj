(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc x] (str acc " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (reverse (rest (reduce (fn [acc y] (cons x (cons y acc))) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (reduce (fn [acc x] [(min x (first acc)) (max x (last acc))]) [fst fst] rst)))

(defn insert [sorted-seq n]
  (concat (take-while (partial > n) sorted-seq)
          (vector n)
          (drop-while (partial > n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc x]
    (if (contains? acc x)
      (disj acc x)
      (conj acc x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x] (reduce (fn [acc y] (and acc (y x))) true more)))

(defn my-map [f & seqs]  
  (loop [lefts seqs
         result []]
    (let [fsts (map first lefts)
          rsts (map rest lefts)]
      (if (some empty? lefts)
        result
        (recur rsts (concat result (vector (apply f fsts))))))))
