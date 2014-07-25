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
  (reduce
   (fn [y ys]
     (into [] (y x ys)))
   a-seq))

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
  (reduce
   insert
   '()
   a-seq))

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
