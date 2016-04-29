(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq ))

(defn str-cat [a-seq]
  (let [helper (fn [first rest] (str first " " rest))]
  (if (empty? a-seq)
    ""
    (reduce helper a-seq ))))

(defn my-interpose [x a-seq]
  (let [helper (fn [first rest] (conj first x rest))]
  (if (<= (count a-seq) 1)
    a-seq
    (rest (reduce helper [] a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse (fn [seq e] (cons e seq))]
    (reduce reverse [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-helper (fn [seq element]
                         (cond
                           (<= element (get seq 0)) (assoc seq 0 element)
                           (>= element (get seq 1)) (assoc seq 1 element)
                           :else seq ))]
    (if (empty? a-seq)
      []
      (reduce min-max-helper [(get a-seq 0) (get a-seq 0)] a-seq))))

(defn insert [sorted-seq n]
  (loop [result-seq '() seq sorted-seq]
    (cond
      (empty? seq) (concat result-seq (cons n seq))
      (<= n (first seq)) (concat result-seq (cons n seq))
      :else (recur (concat result-seq (list (first seq))) (rest seq)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [k] true))
  ([x] x)
  ([x y] (fn [n] (and (x n) (y n))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))


(defn my-map [f a-seq]
  [:-])
