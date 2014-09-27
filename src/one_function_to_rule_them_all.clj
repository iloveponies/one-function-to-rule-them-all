(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (rest(reduce (fn [s1 s2]
                 (conj s1 x s2))
               []
               a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count s1]
                  (if (nil? s1)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [s1 s2]
            (cons s2 s1))
          () a-seq))

(defn min-max-element [a-seq]
  (conj (vector (reduce min a-seq)) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y & more]
    (+ 2 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
