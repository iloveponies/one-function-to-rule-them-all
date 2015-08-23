(ns one-function-to-rule-them-all)


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn concat-elements [a-seq]
  (reduce concat '() a-seq))


(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (seq (reduce (fn [x y] (conj x "x" y)) [(first a-seq)] (rest a-seq)))))


(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))


(defn my-reverse [a-seq]
  (reduce conj '() a-seq))


(defn min-max-element [a-seq]
  (let [f (first a-seq)]
    (reduce (fn [[m M] val] [(min m val) (max M val)]) [f f] (rest a-seq))))


(defn insert [sorted-seq n]
  (loop [acc []
         tail sorted-seq]
    (let [f (first tail)
          r (rest tail)]
      (if (nil? f)
        (conj acc n)
        (if (< n f)
          (concat acc [n] [f] r)
          (recur (conj acc f) r))))))



(defn insertion-sort [a-seq]
  (seq (reduce insert [] a-seq)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [& more]
  (reduce (fn [acc y] (inc acc)) 0 more))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))



(defn pred-and
  ([] (fn [x] true))
  ([x?] x?)
  ([x? y?] (fn [a] (and (x? a) (y? a))))
  ([x? y? & more] (reduce pred-and (pred-and x? y?) more)))


(defn take-first-from-each [& more]
  (reduce
   (fn [[heads tails] seqs]
     (let [f (first seqs)
           r (rest seqs)]
       [(conj heads f) (conj tails r)]))
   [[] []]
   more))


(defn apply-to-each [f & more]
  (let [[head tails] (apply take-first-from-each more)]
    [(apply f head) tails]))


; Wasn't able to do this with reduce but used it in take-first-from-each.
(defn my-map [f & more]
  (loop [acc []
         tails more]
    (if (empty? (first tails))
      acc
      (let [[res new-tails] (apply apply-to-each f tails)]
        (recur (conj acc res) new-tails)))))
