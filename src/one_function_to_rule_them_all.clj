(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [f (fn [x y] (str x " " y))]
      (reduce f a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (fn [a b] (conj (conj a x) b))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [f (fn [i _] (inc i))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [xs x] (cons x xs))]
    (reduce f [] a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [[mi ma] x] [(min mi x), (max ma x)])]
    (reduce f [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [xs [] ys sorted-seq]
    (cond
      (empty? ys) (conj xs n)
      (< (first ys) n) (recur (conj xs (first ys)) (rest ys))
      :else (concat (conj xs n) ys))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & xs] (reduce my-* (* x y) xs)))

(defn pred-and
  ([] (fn [_] true))
  ([p & ps]
   (let [f (fn [q r] (fn [a] (and (q a) (r a))))]
     (reduce f p ps))))

(defn my-map [f & m-seq]
  (loop [res [] xs m-seq]
    (if (empty? (first xs))
      res
      (let [ys (map first xs)
            yss (map rest xs)]
        (recur (conj res (apply f ys)) yss)))))
