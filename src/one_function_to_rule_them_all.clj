(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [acc b] (conj acc x b))
     [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt x] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] x] [(min mn x) (max mx x)])
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (loop [the-seq sorted-seq
         new-seq []
         n-inserted false]
    (if (empty? the-seq)
      (if n-inserted
        new-seq
        (conj new-seq n))
      (let [elem (first the-seq)]
        (if (and (not n-inserted) (> elem n))
          (recur (rest the-seq) (conj new-seq n elem) true)
          (recur (rest the-seq) (conj new-seq elem) n-inserted))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x] (reduce (fn [acc y] (and acc (y x))) true more)))

(defn take-nth-from-all [n a-seq]
  (loop [the-seq a-seq
         acc []]
    (if (empty? the-seq)
      acc
      (recur (rest the-seq) (conj acc (get (first the-seq) n))))))

(defn my-map [f & more]
  (let [min-count
        (reduce (fn [acc x] (min acc (count x))) (count (first more)) more)
        reordered-more
        (reduce (fn [acc n] (conj acc (take-nth-from-all n more))) [] (range min-count))]
    (apply list (reduce (fn [acc x] (conj acc (apply f x))) [] reordered-more))))

