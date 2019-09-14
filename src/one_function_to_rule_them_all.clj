(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce (fn [prev next] (concat prev next)) [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [prev next] (str prev " " next)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
      (reduce (fn [prev next] (conj prev x next))
              [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [coll next] (cons next coll)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mini maxi] next]
            (cond
             (> next maxi) [mini next]
             (< next mini) [next maxi]
             :else [mini maxi]))
          (repeat 2 (first a-seq)) (drop 2 a-seq)))

(defn insert [sorted-seq n]
  (concat (take-while #(> n %) sorted-seq)
          [n]
          (drop-while #(> n %) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set next] (if (contains? a-set next)
                             (disj a-set next)
                             (conj a-set next)))
          #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  [& x] (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [& args] true))
  ([pred] (fn [& args] (apply pred args)))
  ([pred-a pred-b] (fn [& args] (and (apply pred-a args) (apply pred-b args))))
  ([pred-a pred-b & preds]
     (fn [& args] (every? true?
                          (map #(apply % args)
                               (concat [pred-a pred-b] preds))))))

(defn my-map
  ([f coll]
     (reduce (fn [acc next] (conj acc (f next))) [] coll))
  ([f a b]
     (reduce (fn []))))


(defn my-map
  [f & seqs]
  (let [min-count (count
                   (reduce
                    (fn [a b] (if (< (count b) (count a)) b a))
                    seqs))]
    (loop [idx 0
           colls seqs
           acc []]
      (if (= idx min-count)
        acc
        (let [nth-elements (reduce
                            (fn [elms next] (conj elms (nth next idx)))
                            []
                            seqs)]
          (recur (inc idx) (rest colls)
                 (conj acc
                       (apply f nth-elements))))))))
