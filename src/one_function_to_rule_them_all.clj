(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) (str "")
    (reduce str (interpose " " a-seq) )))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [x elem] (inc x))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [sq elem] (cons elem sq))]
    (reduce helper () a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [sq elem]
                  (cond
                    (< elem (first sq)) (assoc sq 0 elem)
                    (> elem (last sq)) (assoc sq 1 elem)
                    :else sq))]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert-helper [sq n]
  (cond
    (empty? sq) ()
    (< n (first sq)) (cons n sq)
    :else (cons (first sq) (insert-helper (rest sq) n))))

(defn insert [sorted-seq n]
  (concat (filter #(< % n) sorted-seq)  [n] (filter #(> % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn my-toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce my-toggle (set nil) a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (my-count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and [& more]
  (fn [x] (let [and-helper (fn [t elem] (and t (elem x)))]
            (reduce and-helper true more))))

(defn my-map [f a-seq & more]
   )
