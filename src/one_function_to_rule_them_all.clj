(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [helper (fn [m n] (str m " " n))]
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [helper (fn [y m] (conj (conj y x) m))]
      (rest (reverse (reduce helper '() a-seq))))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [minV (reduce min a-seq) maxV (reduce max a-seq)]
      [minV maxV])))

(defn insert [sorted-seq n]
  (loop [rseqs '()
         inputseq sorted-seq]
    (if (empty? inputseq)
      (reverse (conj rseqs n))
      (if (< (first inputseq) n)
        (recur (conj rseqs (first inputseq)) (rest inputseq))
        (reverse (apply conj (conj rseqs n) inputseq))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-seq element]
  (if (contains? a-seq element)
    (disj a-seq element)
    (conj a-seq element)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
   (reduce (fn [x y] (inc x)) 1 more)))

(defn my-*
  ([] 1)
  ([a] a)
  ([a & more] (reduce * a more)))

(defn pred-and
  ([] (fn [x] true))
  ([m] m)
  ([m y] (fn [x] (and (m x) (y x))))
  ([m y & more]
   (reduce pred-and (pred-and m y) more)))

(defn my-map
  ([f aseq] (reverse (reduce (fn [x y] (cons (f y) x)) '() aseq)))
  ([f aseq & bseq]
    (loop [r '()
           y (cons aseq bseq)]
      (if(some empty? y)
        (reverse (my-map (fn [x] (apply f x)) r))
        (recur (cons (my-map first y) r) (my-map rest y))))))
