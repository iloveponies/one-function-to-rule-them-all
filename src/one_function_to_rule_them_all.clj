(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [helper (fn [before x] (str before " " x))]
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [helper (fn [before after] (conj before x after))]
      (reduce helper [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [helper (fn [count elem] (inc count))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [rev-set elem] (cons elem rev-set))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [[min-e max-e] elem ] 
                 [(min min-e elem) (max max-e elem)]) ]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [ins-seq []
         sorted-seq sorted-seq]
    (cond
      (empty? sorted-seq)
        (conj ins-seq n)
      (< n (first sorted-seq))
        (concat (conj ins-seq n) sorted-seq)
      :else (recur (conj ins-seq (first sorted-seq)) 
                   (rest sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))



(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    (let [helper (fn [counter x] (inc counter))]
      (reduce helper 2 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [n] true))
  ([x] x)
  ([x y] (fn [n] (and (x n) (y n))))
  ([x y & more]
    (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])