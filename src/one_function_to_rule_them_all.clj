(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [strs (interpose " " a-seq)]
    (reduce str strs)))

(defn my-interpose [x a-seq]
  (loop [a-seq a-seq
         x x
         li []]
    (cond
      (empty? a-seq) []
      (empty? (rest a-seq)) (conj li (first a-seq))
      :else (recur (rest a-seq) x (conj li (first a-seq) x)))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [elem elem1]
              (conj elem elem1))]
    (reduce rev '() a-seq)))

(defn min-max-element [a-seq]
  (let [min (fn [min e]
              (if (< e min)
                e
                min))
        max (fn [max e]
              (if (> e max)
                e
                max))]
    (conj [] (reduce min a-seq) (reduce max a-seq))))

(defn insert [sorted-seq n]
  (loop [a-seq sorted-seq
         b-seq []
         n n]
    (if (or (empty? a-seq) (< n (first a-seq)))
      (concat (conj b-seq n) a-seq)
      (recur (rest a-seq)
             (conj b-seq (first a-seq))
             n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

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
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
