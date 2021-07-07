(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (cond 
    (empty? a-seq) ""
    (empty? (rest a-seq)) (first a-seq)
    :else (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (loop [res (conj [] (first a-seq))
           l-seq (rest a-seq)]
      (if (empty? l-seq)
        res
        (recur (conj (conj res x) (first l-seq)) (rest l-seq))))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  (loop [c-min (first a-seq)
         c-max (first a-seq)
         l-seq (rest a-seq)]
    (if (empty? l-seq)
      [c-min c-max]
      (recur (min c-min (first l-seq)) 
             (max c-max (first l-seq)) 
             (rest l-seq)))))

(defn insert [sorted-seq n]
  (loop [head []
         end sorted-seq]
    (if (or (empty? end) (< n (first end)))
      (concat (conj head n) end)
      (recur (conj head (first end)) (rest end)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [p-set n]
  (if (contains? p-set n)
    (disj p-set n)
    (conj p-set n)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [x y] (inc x)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p1?] p1?)
  ([p1? p2?] (fn [x] (and (p1? x) (p2? x))))
  ([p1? p2? & more] (reduce pred-and (pred-and p1? p2?) more)))

(defn my-map 
  ([f & more] 
    (loop [res []
           l-seqs more]
      (if (some empty? l-seqs)
        (seq res)
        (recur (conj res (apply f (map first l-seqs))) (map rest l-seqs))))))

