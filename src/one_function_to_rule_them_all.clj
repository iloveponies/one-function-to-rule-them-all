(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (reduce #(conj (conj %1 x) %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc val] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) '() a-seq))

(defn min-max-element [a-seq]
  (let [acc [(first a-seq) (first a-seq)]
        helper (fn [[min-val max-val] val]
                  [(min min-val val) (max max-val val)])]
    (reduce helper acc (rest a-seq))))

(defn insert [sorted-seq n]
  (if (or (empty? sorted-seq) (< n (first sorted-seq)))
    (cons n sorted-seq)
    (cons
      (first sorted-seq)
      (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [set var]
                  (if (contains? set var)
                    (disj set var)
                    (conj set var)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x]
    (reduce #(and %1 (%2 x)) true more)))

(defn my-map [f & more]
  (let [n
         (reduce #(min %1 (count %2)) (count (first more)) (rest more))]
    (loop [acc []
           seqs more
           i 0]
      (if (= i n)
        acc
        (recur
          (conj acc (apply f (reduce #(conj %1 (first %2)) [] seqs)))
          (reduce #(conj %1 (rest %2)) [] seqs)
          (inc i))))))
