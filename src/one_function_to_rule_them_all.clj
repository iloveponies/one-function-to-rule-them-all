(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) []
    (reduce (fn [m n] (conj m x n)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [f (fn [counter aseq] (inc counter))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq) []
    (reduce (fn [m n] (cons n m)) [(first a-seq)] (rest a-seq))))

(defn min-max-element [a-seq]
  (let [f (fn [result a]
            (cond
              (< a (first result)) [a (last result)]
              (> a (last result)) [(first result) a]
              :else result))]
    (reduce f [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [f (fn [result a] (if (odd? (get (frequencies a-seq) a))
                           (conj result a)
                           result))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (- 0 x))
   ([x y] (- x y)))

(defn count-params
  ([& more] (reduce (fn [counter _] (inc counter)) 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and [& preds]
  (fn [a]
    (if (empty? preds)
      true
      (reduce (fn [e pred] (and e (pred a))) true preds))))

(defn my-map [f & a-seq]
  (let [func (fn [seqs n] (reduce (fn [x y] (concat x [(get y n)])) () seqs))]
   (reduce (fn [x y] (concat x [(apply f (func a-seq y))]))
           ()
           (range (count (first a-seq))))))
