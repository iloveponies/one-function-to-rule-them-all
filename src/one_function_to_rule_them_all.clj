(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
    (reduce #(conj % x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mini maxi] elem] [(min mini elem) (max maxi elem)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq) [n] (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (* (* x y) (reduce * more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (and (p1 x) (p2 x) ((reduce pred-and more) x)))))

(defn my-map
  ([f a-seq] (if (empty? a-seq) '()
               (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq & more] (let [b-seq (conj more a-seq)]
   (if (some empty? b-seq) '()
     (cons (apply f (my-map first b-seq)) (apply my-map f (my-map rest b-seq)))))))
