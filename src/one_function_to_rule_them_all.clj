(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
(drop-last (reduce #(conj %1 %2 x) [] a-seq)))

(defn my-count [a-seq]
  (loop [acc 0 s a-seq]
    (if (empty? s) acc
      (recur (inc acc) (rest s)))))



(defn my-reverse [a-seq]
  (loop [acc [] s a-seq]
    (if (empty? s) acc
      (recur (cons (first s) acc) (rest s)))))


(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])



(defn insert [sorted-seq n]
  (sort (cons n sorted-seq)))

(defn insertion-sort [a-seq]
 (reduce insert [] a-seq))


(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))


(defn minus [& x]
   (apply - x))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and
  ([] (fn [x] true))                       ; no parameters
  ([p] (fn [x] (p x)))                         ; one parameter
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))        ; two parameters
  ([p1 p2 & more]                       ; more than two parameters
    (reduce pred-and (pred-and p1 p2) more)))


(defn my-map [f a-seq]
  [:-])




