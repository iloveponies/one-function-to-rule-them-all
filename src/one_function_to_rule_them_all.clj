(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) () a-seq))

(defn min-max-element [a-seq]
  (let [head (first a-seq)]
    (reduce #(identity [(min (first %1) %2) (max (second %1) %2)]) [head head] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [seq-start []
         seq-rest sorted-seq
         number n]
    (cond
      (empty? seq-rest) (conj seq-start n)
      (<= number (first seq-rest)) (concat (conj seq-start number) seq-rest)
      :else (recur (conj seq-start (first seq-rest)) (rest seq-rest) number))))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([& more]
    (my-count more)))

(defn my-* 
  ([] 1) 
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [_] (identity true))) 
  ([p] p)
  ([p1 p2] #(and (p1 %1) (p2 %1)))
  ([p1 p2 & more]
    (fn [x] (reduce #(and %1 (%2 x)) ((pred-and p1 p2) x) more))))

(defn my-map [f a-seq]
  )
