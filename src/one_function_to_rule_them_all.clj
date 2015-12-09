(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (drop 1  (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [ nums seq]
                  (inc nums))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rever (fn [res seq]
                (conj (first seq) res))]
      (reduce conj '() a-seq)))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (seq [n]) 
        (< (first sorted-seq) n) (cons (first sorted-seq) (insert (rest sorted-seq) n)) 
        :else (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and 
  ([] (fn [arg] true))
  ([x] x)
  ([x y] (fn [arg] (and (x arg) (y arg))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f & a-seq]
  (if (some empty? a-seq) '()
     (cons (apply f (map first a-seq)) 
            (apply (partial my-map f) (map rest a-seq)) )))
