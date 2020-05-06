(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest (reduce #(conj % x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [[mn mx] elem]
                  (list (min mn elem)
                        (max mx elem)))
        f (first a-seq)]
    (reduce min-max [f f] a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
                                  (disj a-set elem)
                                  (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [& more]
  (my-count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and
  ([] (fn [_] true))
  ([p?] p?)
  ([p1? p2?] #(and (p1? %) (p2? %)))
  ([p1? p2? & more] (reduce pred-and (pred-and p1? p2?) more)))

(defn my-map
  ([f a-seq] (seq (reduce #(conj % (f %2)) [] a-seq)))
  ([f a-seq & more] (loop [res []
                           l-seq (cons a-seq more)]
                      (if (some empty? l-seq)
                        (seq res)
                        (recur (conj res (apply f (my-map first l-seq))) (my-map rest l-seq))))))
