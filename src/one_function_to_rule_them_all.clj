(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (let [cat-help (fn [str1 str2]
      (str str1 " " str2))]
    (reduce cat-help a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
    (let [pose-help (fn [a b]
      (if (empty? a)
        (conj a b)
        (conj a x b)))]
  (seq (reduce pose-help [] a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                    (inc count))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-help (fn [a b]
                        (conj a b))]
  (reduce reverse-help () a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-help (fn [min-max elem]
    (if (empty? min-max)
      [elem elem]
      [(min elem (first min-max)) (max elem (last min-max))]))]
  (reduce min-max-help [] a-seq)))

(defn insert [sorted-seq n]
  (let [insert-help (fn [head-seq tail-seq]
    (cond
      (empty? tail-seq) (conj head-seq n)
      (< n (first tail-seq))
        (concat (conj head-seq n) tail-seq)
      :else (recur (conj head-seq (first tail-seq)) (rest tail-seq))))]
  (insert-help [] sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [parity-helper (fn [odds elem]
    (if (contains? odds elem)
      (set (remove #{elem} odds))
      (conj odds elem)))]
  (reduce parity-helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
    (let [count-help (fn [count elem] (inc count))]
    (reduce count-help 1 more))) )

(defn my-* ([] 1) ([x] x)
  ([x & more]
    (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred & more]
    (fn [x] (let [pred-check (fn [curr pred2]
      (and curr (pred2 x)))]
  (reduce pred-check (pred x) more)))))

(defn my-map [f a-seq]
  [:-])
