(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e] (if (= e nil) count (inc count)))]
     (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [output e] (cond (= (first output) nil) 
                                    (assoc (assoc output 0 e) 1 e)
                                 (< e (first output)) (assoc output 0 e)
                                 (> e (last output)) (assoc output 1 e)
                                 :else output))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) 
                                   (conj a-set elem)))]
  (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
    (count x))

(defn my-*
  ([] 1) 
  ([x] x)
  ([x y] (* x y))
  ([x y & more] 
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] x))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] 
    (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  )
