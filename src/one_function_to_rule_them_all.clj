(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce  (fn [prev nxt] (str prev " " nxt))  a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [prv nxt] (conj prv x nxt)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [size x] (inc size)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [newseq []
         ss sorted-seq]
    (cond
     (empty? ss)
       (conj newseq n)
     (> n (first ss))
       (recur (conj newseq (first ss)) (rest ss))
     :else
       (concat newseq [n] ss))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (loop [newset #{}
         as a-seq]
    (cond
     (empty? as) newset
     (contains? newset (first as))
       (recur (disj newset (first as)) (rest as))
     :else
       (recur (conj newset (first as)) (rest as)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and ([] boolean)
  ([x] x)
  ([x y] (fn [n] (and (x n)  (y n))))
  ([x y & args] (reduce pred-and (pred-and x y) args)))

(defn my-map [f a-seq]
  [:-])
