(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (not (empty e))
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) () a-seq))

(defn min-max-element [a-seq]
  (let [minmax (fn [[minx maxx] a]
                 (let [nmi (if minx (min minx a) a)
                       nma (if maxx (max maxx a) a)]
                   [nmi nma]))]
    (reduce minmax [] a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) [n]
   (> n (first sorted-seq)) (cons (first sorted-seq) (insert (rest sorted-seq) n))
   :else (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (+ 2 (my-count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [a] a))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
