(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc val] (if-not (empty? acc)
                          (conj acc x val)
                          (conj acc val))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn flip [f] (fn [a b] (f b a)))

(defn my-reverse [a-seq]
  (reduce (flip cons) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[cur-min cur-max] element]
            [(min cur-min element)
             (max cur-max element)])
          (repeat 2 (first a-seq))
          (rest a-seq)))

(defn insert [sorted-seq n]
  (concat (take-while (partial > n) sorted-seq)
          [n]
          (drop-while (partial > n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem] ;; from earlier chapter
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
