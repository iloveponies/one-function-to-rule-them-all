(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc v] (str acc " " v)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc v]
            (if (empty? acc)
              (conj acc v)
              (conj acc x v))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc v] (cons v acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc v]
                  (if (empty? acc)
                    [v v]
                    [(min v (first acc)) (max v (last acc))]))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (let [smaller-than #(< % n)]
    (concat (take-while smaller-than sorted-seq) [n] (drop-while smaller-than sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [acc v]
            (if (contains? acc v)
              (disj acc v)
              (conj acc v))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x] (reduce (fn [acc v] (and acc (v x))) true more)))

(defn my-map
  ([f a-seq] (reduce (fn [acc v] (conj acc (f v))) [] a-seq))
  ([f a-seq & more-seqs] (let [seqs (cons a-seq more-seqs)
                               applier (fn [acc v] (conj acc (apply f v)))]
                           (reduce applier [] (partition (count seqs) (apply interleave seqs))))))

