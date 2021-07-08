(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (clojure.string/join " " a-seq))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [sq1 el] (conj sq1 x el))
     [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [c elem]
                  (if elem
                    (inc c)
                    c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [sq1 el] (conj sq1 el))
     (list (first a-seq)) (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [sq1 el]
       (if (> (first sq1) el)
         (assoc sq1 0 el)
         (if (< (second sq1) el)
           (assoc sq1 1 el)
           sq1)))
     [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [sq1 el] (insert sq1 el))
     (list (first a-seq)) (rest a-seq))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce
     (fn [sq1 el] (toggle sq1 el))
     #{(first a-seq)} (rest a-seq))))

(defn minus ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    (+ 2(count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2]
   (fn [x] (and (pred1 x) (pred2 x))))
  ([x y & more]
    (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
