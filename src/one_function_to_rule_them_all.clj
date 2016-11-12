(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce #(concat %1 " " %2) a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [f (fn [elements el](conj elements el x))]
      (drop-last (reduce f [] a-seq)))))

(defn my-count [a-seq]
  (let [f (fn [counter _] (inc counter))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [f (fn [result el] [(min el (get result 0 el)) (max el (get result 1 el))])]
    (reduce f [] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (loop [c (first sorted-seq)
           former []
           latter sorted-seq]
      (cond
        (<= n c) (concat former (cons n latter))
        (empty? (rest latter)) (conj former c n)
        :else (recur (first (rest latter)) (conj former c) (rest latter))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [s el] (if (contains? s el)
                            (disj s el)
                            (conj s el)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])
