(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (str (first a-seq) (reduce #(str %1 " " %2) "" (rest a-seq)))))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc x] (+ 1 acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (let [helper #(let [[x y] %1] [(min x %2) (max y %2)])]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (+ 1 (apply count-params more))))

(defn my-* [& rest]
  (reduce * 1 rest))

(defn pred-and [& rest]
  (reduce (fn [acc p] (fn [x] (and (acc x) (p x)))) (fn [x] true) rest))

(defn- simple-map [f xs]
  (if (empty? xs) []
    (cons (f (first xs)) (map f (rest xs)))))

(defn my-map [f & seqs]
  (if (some empty? seqs) []
    (cons (apply f (simple-map first seqs)) (apply my-map f (simple-map rest seqs)))))
