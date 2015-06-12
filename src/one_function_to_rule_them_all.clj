(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce #(str %1 " " %2)  a-seq)))

(defn my-interpose [x a-seq]
  (if (not-empty a-seq) (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))
    a-seq))

(defn my-count [a-seq]
  (reduce (fn[cnt my-seq] (if my-seq (inc cnt) cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[rev myseq] 
            (cons myseq rev)) [] a-seq))

(defn min-max-element [a-seq]
  (let [start (first a-seq)
        extrfunc (fn[extr elem] [(min (extr 0) elem) (max (extr 1) elem)])]
    (reduce extrfunc [start start] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [front []
         tail sorted-seq]
    (if (or (empty? tail) (<= n (first tail))) (concat front [n] tail)
      (recur (concat front [(first tail)]) (rest tail)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn[my-set item] (if (my-set item) (disj my-set item) (conj my-set item))) #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn[cnt args] (inc cnt)) 0 more))

(defn my-* [& x]
  (reduce (fn[base elem] (* base elem)) 1 x))

(defn pred-and [& x]
  (fn[y] (reduce #(and %1 (%2 y)) true x)))

(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2]) 

(defn my-map [f start & a-seq] 
  (let [args (concat [start] a-seq)
        mincnt (reduce (fn[mn my-seq] (min mn (count my-seq))) (count (first args)) (rest args))]
   
  (loop[cnt (dec mincnt)
        res '()]
  
  (if (< cnt 0) res
    (recur (dec cnt) (cons (apply f (reduce (fn[myset coll] (conj myset (nth coll cnt))) [] args)) res))))))




