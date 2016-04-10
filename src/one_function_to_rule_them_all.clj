(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    (empty? (rest a-seq)) (first a-seq)
    :else (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) '()
    (empty? (rest a-seq)) a-seq
    :else (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b] (vector (min (first a) b) (max (second a) b))) (vector (first a-seq) (first a-seq)) a-seq))

(defn insert [sorted-seq n]
  (concat
    (take-while (fn [x] (< x n)) sorted-seq)
    [n]
    (drop-while (fn [x] (< x n)) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) '() a-seq))

(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem) (conj a-set elem)))
  
(defn parity [a-seq]
  (reduce (fn [a b] (toggle a b)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (fn [x] (reduce (fn [b r] (and b (r x))) (and (p x) (q x)) more))))

(defn my-map [f & seqs]
  (let [fst (fn [sq] (reduce (fn [a b] (conj a (first b))) [] sq))
        rst (fn [sq] (reduce (fn [a b] (conj a (rest b))) [] sq))]
    (if (some empty? seqs)
      ()
      (cons (apply f (fst seqs))
            (apply my-map f (rst seqs))))))
