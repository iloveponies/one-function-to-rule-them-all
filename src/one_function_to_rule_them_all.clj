(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [helper (fn [a b] (str a " " b))]
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
    (let [helper (fn [a b] (conj a x b))]
      (rest (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [helper (fn [i elem]
                  (inc i))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [reversed-seq elem]
                 (cons elem reversed-seq))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [start-of-seq []
         rest-of-seq sorted-seq]
    ; If we're not at the end of the original sequence and the current number
    ; we're looking at is smaller than n, we add the current number into our new seq
    ; and loop through the rest of the original seq
    (if (and (not (empty? rest-of-seq)) (< (first rest-of-seq) n))
      (recur (conj start-of-seq (first rest-of-seq)) (rest rest-of-seq))
      (concat (conj start-of-seq n) rest-of-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [odd-occurance-set elem]
                 (if (contains? odd-occurance-set elem)
                   (disj odd-occurance-set elem)
                   (conj odd-occurance-set elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& param-count]
  (count param-count))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f & a-seq]
  (if (empty? a-seq)
    '()
    (loop [])))
