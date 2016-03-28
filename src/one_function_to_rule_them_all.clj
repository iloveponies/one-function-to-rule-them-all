(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [[min-el max-el] el] [(min min-el el) (max max-el el)])
    [(first a-seq) (first a-seq)]
    (rest a-seq)))

(defn insert [sorted-seq n]
  (loop
    [pre-seq []
     post-seq sorted-seq]
    (if (or (empty? post-seq) (< n (first post-seq)))
      (concat (conj pre-seq n) post-seq)
      (recur (conj pre-seq (first post-seq)) (rest post-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set el]
                 ((if (contains? a-set el) disj conj) a-set el))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [ & params]
  (count params))

(defn my-* [& a-seq]
  (reduce * 1 a-seq))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p  q & preds] (reduce pred-and (pred-and p q) preds)))

(defn my-map [f & a-seq]
  (let [get-firsts (fn [& args] (reduce #(conj %1 (first %2)) [] args))
        get-rems (fn [& args] (reduce #(conj %1 (rest %2)) [] args))]
    (loop [acc []
           rem-seqs a-seq]
      (if (empty? (first rem-seqs))
        acc
        (recur (conj acc (apply f (apply get-firsts rem-seqs))) (apply get-rems rem-seqs))))))