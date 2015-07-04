(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (reduce str [x " " y])) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
    (reduce (fn [a b] (conj a x b)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq) 0
    (let [counter (fn [length c] (inc length))]
      (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  (if (empty? a-seq) a-seq
    (reduce (fn [a b] (conj a b)) (list (first a-seq)) (rest a-seq))))

(defn min-max-element [a-seq]
  (let [result [Integer/MAX_VALUE Integer/MIN_VALUE]
        check-min (fn [a b] (if (< b (get a 0)) (assoc a 0 b) a))
        check-max (fn [a b] (if (> b (get a 1)) (assoc a 1 b) a))]
    (reduce (fn [z x] (check-min (check-max z x) x)) result a-seq)))

(defn insert [sorted-seq n]
  (loop [result '()
         unchecked sorted-seq
         i (count sorted-seq)]
    (if (zero? i) (concat sorted-seq (vector n))
     (if (>= (first unchecked) n) (concat result (vector n) unchecked)
      (recur (concat result (vector (first unchecked))) (rest unchecked) (dec i)))
      )))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) (vector (first a-seq)) (rest a-seq)))

(defn parity [a-seq]
  (reduce (fn [x y] (if (contains? x y) (disj x y) (conj x y))) #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] pred1)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  [] true)
