(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce
   (fn [x y]
     (str x " " y))
   a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
  (cons (first a-seq) (reduce (fn [in sea] (conj in x sea)) [] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce
   (fn [x y]
     (inc x))
   0
   a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [x y]
     (let [[min max] x]
       (cond
        (< y min) [y max]
        (> y max) [min y]
        :else [min max])))
    [(first a-seq) (first a-seq)]
    (rest a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))


(defn parity [a-seq]
  (reduce
    (fn [a-set elem]
      (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))
   #{}
   a-seq
   ))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& x]
  (if (empty? x)
    (fn [y] true)
    (reduce
     (fn [y z]
       (fn [s]
         (and (y s) (z s))))
     x)))

(defn an-empty? [& colls]
  (reduce (fn [x coll] (or x (empty? coll))) false colls))

(defn rests [& colls]
  (reduce (fn [x y] (conj x (rest y))) [] colls))

(defn firsts [& colls]
  (reduce (fn [x y] (conj x (first y))) [] colls))

(defn build-vect [res colls]
  (if (apply an-empty? colls)
    res
    (build-vect
     (cons (apply firsts colls) res)
     (apply rests colls))))

(defn sing [f x]
  (reverse (reduce (fn [col z] (cons (f z) col)) [] x)))

(defn my-map
  ([f x] (sing f x))
  ([f x & more]
   (reverse (reduce (fn [a b] (cons (apply f b) a)) [] (reverse (build-vect [] (conj more x)))))))

