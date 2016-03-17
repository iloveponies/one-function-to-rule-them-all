(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))


(defn my-interpose [x a-seq]
    (drop 1 (interleave (repeat x) a-seq)))


(defn my-count [a-seq]
 (reduce (fn [n _] (inc n)) 0 a-seq))


(defn my-reverse [a-seq]
 (if (empty? a-seq)
    a-seq
    (loop [[x & xs] a-seq
           new-seq []]
      (if (empty? xs)
        (cons x new-seq)
        (recur xs (cons x new-seq))))))

(defn min-max-element [a-seq]
 (if (empty? a-seq)
    a-seq
    (let [min-max-reducer (fn [min-max elem]
                            (vector
                             (min (first min-max) elem)
                             (max (last min-max) elem)))]
      (reduce min-max-reducer [(first a-seq) (first a-seq)] (rest a-seq)))))


(defn insert [sorted-seq n]
  (loop [b [] a sorted-seq]
    (let [next (first a)]
    (if (or (empty? a) (< n next))
        (concat (conj b n) a)
        (recur (conj b next) (rest a))))))

(defn insertion-sort [a-seq]
 (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [items (set a-seq)
        odd-times? (fn [x] (odd? (count (filter #{x} a-seq))))]
    (set (filter odd-times? items))))

(defn minus [x]
  [x] (- x))
  ([x y] (- x y)))

((defn count-params
  ([& more] (let [counter (fn [x y] (inc x))]
              (reduce counter 0 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  (cond
    (some empty? a-seq)
      '()
    :else
      (cons (apply f (map first a-seq))
      (apply (partial my-map f) (map rest a-seq)))))