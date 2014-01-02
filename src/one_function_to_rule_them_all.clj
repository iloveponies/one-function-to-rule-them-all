(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mi ma] elem]
            (vector (if mi (min mi elem) elem)
                    (if ma (max ma elem) elem)))
          [nil nil] a-seq))

(defn insert [sorted-seq n]
  (loop [less  []
         great sorted-seq]
    (cond (empty? great) (conj less n)
          (>= (first great) n) (concat (conj less n) great)
          :else (recur (conj less (first great)) (rest great)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more] (reduce * more))

(defn pred-and
  ([] (constantly true))
  ([p] p)
  ([p q] #(and (p %1) (q %1)))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map
  ([f a-seq]
   (loop [from a-seq to []]
     (if (empty? from) (seq to)
       (recur (rest from) (conj to (f (first from)))))))
  ([f a-seq & more]
   (loop [from (cons a-seq more) to []]
     (if (some empty? from) (seq to)
       (recur (my-map rest from) (conj to (apply f (my-map first from))))))))
