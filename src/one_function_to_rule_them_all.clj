(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [count e] (inc count)) a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [acc e] (conj acc e)) '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc e] [(min (first acc) e)
                         (max (second acc) e)])
            [(first a-seq) (first a-seq)]
            a-seq)))

(defn insert [sorted-seq n]
  (cond 
    (empty? sorted-seq) (conj nil n)
    (> (first sorted-seq) n) (concat (conj nil n) sorted-seq)
    :else (conj (insert (rest sorted-seq) n) (first sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
   (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] (identity true)))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f params] (for [elem params]
               (f elem)))
  ([f params & more]
   (let [params (cons params more)]
     (for [n (range (count params))]
       (apply f (for [p params] (get p n)))))))