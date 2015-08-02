(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) 
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[low high] c] [(min low c) (max high c)]) 
          [(first a-seq) (first a-seq)] 
                  a-seq))

(defn insert [s n]
  (cond
    (empty? s) [n]
    (> (first s) n) (concat [n] s)
    :else (concat [(first s)] (insert (rest s) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
    (fn [s x]
      (if (contains? s x)
        (disj s x)
        (conj s x)))
    #{}
    a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (reduce #(and %1 (%2 x)) (p x) more))))

(defn my-map [f & seqs]
  (if (some empty? seqs)
    []
    (cons 
      (apply f (reduce #(cons (first %2) %1) '() seqs))
      (apply my-map (cons f (reduce #(cons (rest %2) %1) '() seqs))))))
