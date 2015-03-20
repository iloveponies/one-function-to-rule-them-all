(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [acc s] (if (empty? acc)
                          [s]
                          (concat acc [x s]))) '() a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc s] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc s] (concat [s] acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc s] (cond
                       (empty? acc) [s s]
                       (< s (first acc)) [s (second acc)]
                       (> s (second acc)) [(first acc) s]
                       :else acc))
          '() a-seq))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) [n]
   (< n (first sorted-seq)) (concat [n] sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc n] (insert acc n)) '() a-seq))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [acc elem] (toggle acc elem)) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (reduce (fn [acc p] (+ acc 1)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce (fn [acc n] (* acc n)) x more)))

(defn pred-and [& ps]
 (fn [x] (reduce (fn [acc p] (and acc (p x))) true ps)))

(defn my-map [f & seqs]
   (defn transpose [seqs]
     (for [i (range (count (first seqs)))]
       (reduce (fn [acc x] (conj acc (get x i))) [] seqs)))
   (loop [acc []
          lst (transpose seqs)]
     (if (empty? lst)
       acc
       (recur (conj acc (apply f (first lst))) (rest lst)))))
