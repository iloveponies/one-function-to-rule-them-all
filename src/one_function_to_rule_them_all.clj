(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if empty? a-seq) ""
  (reduce str (clojure.string/join " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) ()
   (cons (first a-seq)
         (reduce (fn [initial item] (conj (conj initial x) item))
           []
           (rest a-seq)))))

(defn my-count [a-seq]
  (let [my-count (fn [n element]
                   (inc n))]
    (reduce my-count 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [initial item] (cons item initial)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

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

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn pred-and
  ([]  (fn [x] true))
  ([p] (fn [p] p))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f a-seq]
  [:-])
