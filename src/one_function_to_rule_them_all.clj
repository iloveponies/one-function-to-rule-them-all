(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce conj a-seq x)))

(defn my-count [a-seq]
  (let [my-counter (fn [co s]
                     (inc co))]
  (reduce my-counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [old rev-seq]
              (cons (last old) rev-seq))]
    (reduce rev a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [vec se] [(min (get vec 0) se) (max (get vec 1) se)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (let [indx (fn [idx sort-seq n]
               (cond
                (empty? sort-seq)
                 idx
                (> (first sort-seq) n)
                 idx
                :else (recur (inc idx) (rest sort-seq) n)))]
    (concat (take (indx 0 sorted-seq n) sorted-seq) (conj (drop (indx 0 sorted-seq n) sorted-seq)n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [parities elem]
                 (if (contains? parities elem)
                   (disj parities elem)
                   (conj parities elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (unchecked-negate-int x))
  ([x y] (- x y)))

(defn count-params [& params] (count params))

(defn my-*
  ([] 1)
  ([x] 1)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [p] true))
  ([p] p)
  ([p1 p2] (fn [p] (and (p1 p) (p2 p))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])

