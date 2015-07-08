(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc cur] (conj acc x cur)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [lo (reduce min a-seq)
        hi (reduce max a-seq)]
    [lo hi]))

(defn insert [sorted-seq n]
  (loop [head []
         s sorted-seq]
    (cond (empty? s) (concat head [n])
          (< n (first s)) (concat head [n] s)
          :else (recur (conj head (first s)) (rest s)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc cur]
            (if (contains? acc cur)
              (disj acc cur)
              (conj acc cur)))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p1] p1)
  ([p1 p2 & more] (reduce pred-and (fn [x] (and (p1 x) (p2 x))) more)))

(defn my-map [f & more]
  (loop [seqs more
         acc []]
    (if (some empty? seqs)
      acc
      (let [heads (reduce (fn [a-seq cur] (conj a-seq (first cur))) [] seqs)
            tails (reduce (fn [a-seq cur] (conj a-seq (rest cur))) [] seqs)]
        (recur tails (conj acc (apply f heads)))))))
