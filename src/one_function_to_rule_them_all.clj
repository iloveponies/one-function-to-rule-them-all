(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
  ()
  (reduce (fn [a-sequence an-item] (if (or (== (count a-sequence) (* 2 (count a-seq))) (empty? a-sequence))
                                     (concat a-sequence [an-item])
                                     (concat a-sequence [x] [an-item]))) [] a-seq)))

(defn my-count [a-seq]
    (reduce (fn [acc next-element] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b]
            (conj a b))
          () a-seq))

(defn min-max-element [a-seq]
   [(apply min a-seq) (apply max a-seq)])

(defn insert [sorted-seq n]
  (loop [result [] a-seq sorted-seq]
    (cond
      (empty? a-seq)
        (conj result n)
      (<= n (first a-seq))
        (concat (conj result n) a-seq)
      :else (recur (conj result (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (loop [sorted-sequence () a-sequence a-seq]
    (cond
      (empty? a-sequence)
        sorted-sequence
      :else (recur (insert (vec sorted-sequence) (first a-sequence)) (rest a-sequence)))))

(defn parity [a-seq]
  (reduce (fn [a-set b]
            (if (contains? a-set b)
              (disj a-set b)
              (conj a-set b)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])
