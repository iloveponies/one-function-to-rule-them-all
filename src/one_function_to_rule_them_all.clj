(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj % x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [sum a]
                  (if (= nil a)
                    sum
                    (inc sum)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (loop [size (count a-seq)
         y ()]
    (if (= size 0)
      y
      (recur (dec size) (concat y (vector (get a-seq (dec size))))))))

(defn apu [x y]
  [(min (first x) y) (max (last x) y)])

(defn min-max-element [a-seq]
(reduce apu [100000 -100000] a-seq))

(defn insert [sorted-seq n]
  (concat (filter #(< % n) sorted-seq) [n] (filter #(> % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert (sort a-seq) nil))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] (* 1 x))
  ([x & more] (reduce * (my-* x) more)))

(defn pred-and
  ([] (constantly true))
  ([pred1] pred1)
  ([pred1 pred2]  #(and (pred1 %) (pred2 %)))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  )
