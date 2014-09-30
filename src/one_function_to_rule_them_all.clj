(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc item] (str acc " " item)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc item] (conj acc x item)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [counter _] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (list n)
    (> (first sorted-seq) n) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))


(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [item] true))
  ([pred?] pred?)
  ([pred1? pred2?] (fn [item] (and (pred1? item) (pred2? item))))
  ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))

(defn my-map
  ([f a-seq] (reduce (fn [acc item] (conj acc (f item))) [] a-seq))
  ([f a-seq & seqs]
   (loop [results []
          looping-seqs (cons a-seq seqs)]
     (if (some empty? looping-seqs)
       results
       (recur
         (conj results (apply f (map first looping-seqs)))
         (map rest looping-seqs))))))
