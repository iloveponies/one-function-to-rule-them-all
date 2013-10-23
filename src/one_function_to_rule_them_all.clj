(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(conj (conj %1 x) %2) [(first a-seq)] (rest a-seq))))

(defn str-cat [a-seq]
  (reduce str (my-interpose " " a-seq)))

(defn my-count [a-seq]
 (reduce (fn [alku _] (inc alku)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [fs a-seq] (cons a-seq fs)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mini maxi] a-seq]
              [(min a-seq (or mini a-seq))
               (max a-seq (or maxi a-seq))])
          [nil nil] a-seq))

(defn insert [sorted-seq n]
  (loop [new-seq []
         a-seq sorted-seq
         value n]
    (cond
     (empty? a-seq) (conj new-seq value)
     (<= value (first a-seq)) (concat new-seq [value] a-seq)
     :else (recur (conj new-seq (first a-seq))
                  (rest a-seq)
                  value))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
                (disj a-set elem)
                (conj a-set elem)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& x] (count x)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [pred] true))
  ([x] x)
  ([x y] (fn [pred] (and (x pred) (y pred))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f & a-seqs]
    (if (empty? (first a-seqs))
      ()
      (loop [a () b (map first a-seqs) c (map rest a-seqs)]
        (let [d (cons (apply f b) a)]
          (if (empty? (first c))
            (reverse d)
            (recur d (map first c) (map rest c))))))))
