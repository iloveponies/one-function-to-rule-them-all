(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if
    (empty? a-seq) ""
    (reduce str (interpose " " (concat a-seq)))))

(defn my-interpose [x a-seq]
  (if (>= 1 (count a-seq)) a-seq
  (reduce (fn [a b]
            (if (empty? a) (conj [] b)
              (conj a x b)))
           [] a-seq)))

(defn my-count [a-seq]
  (let
    [counter (fn [c xxxx] (inc c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let
    [reverser (fn [b-seq x]
                (cons x b-seq))]
    (reduce reverser [] a-seq)))


(defn min-max-element [a-seq]
  (if
    (empty? a-seq) []
    (let [minmax (fn [b-seq x]
                   [(min (first b-seq) x) (max (last b-seq) x)])]
      (reduce minmax [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  (loop
    [sorted-seq sorted-seq
     n n
     new-seq '()]
    (cond
      (empty? sorted-seq) (reverse (cons n new-seq))
      (<= n (first sorted-seq)) (concat (reverse (conj new-seq n)) sorted-seq)
     :else (recur (rest sorted-seq) n (cons (first sorted-seq) new-seq)))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) '()
    (reduce insert '() a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
   ))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus ([x]
  (- x))
  ([x y] (- x y)))

(defn count-params [& more]
   (reduce (fn [x y] (inc x)) 0 more))


(defn my-* [& more]
  (reduce (fn [x y] (* x y)) 1 more))


(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [a] (x a)))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))


(defn my-map [f a-seq]
  ())
