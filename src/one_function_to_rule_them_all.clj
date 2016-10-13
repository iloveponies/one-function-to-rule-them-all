(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [result _]
              (inc result))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [result n]
              (conj result n))]
    (reduce rev () a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [result n]
                  (vector
                    (apply min (conj result n))
                    (apply max (conj result n))))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (concat
    (first (split-with #(< % n) sorted-seq))
    [n]
    (second (split-with #(< % n) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [odd-keeper (fn [result s]
                     (if (not= 0 (mod (val s) 2))
                       (conj result (key s))
                       #{}))]
    (reduce odd-keeper #{} (frequencies a-seq))))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and
                           (pred1 x)
                           (pred2 x))))
  ([pred1 pred2 & more] (fn [x] (and
                                  ((reduce pred-and pred1 more) x)
                                  (pred2 x)))))

(defn my-map [f a-seq]
  [:-])
