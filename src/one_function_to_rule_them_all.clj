(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
      (reduce concat [] a-seq)
)

(defn str-cat [a-seq]
      (if (empty? a-seq)
      ""
      (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose-helper [i] (fn [x y] (conj x i y)))

(defn my-interpose [i a-seq]
  (if (empty? a-seq)
    '()
    (seq (reduce (my-interpose-helper i)   [(first a-seq)] (rest a-seq)))))


(defn my-count [a-seq]
  (let [counter (fn [count e]
                    (inc count)
                    )]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq) 
)


(defn min-max-helper [[a-min a-max] c] (vector (min a-min c) (max a-max c)))
  
(defn min-max-element [a-seq]
  (if (empty? a-seq)
      '[]
      (reduce min-max-helper (vector (first a-seq) (first a-seq)) (rest a-seq)))
)


(defn insert [sorted-seq n]
 (concat (take-while #(< %1 n) sorted-seq) [n] (drop-while #(< %1 n) sorted-seq)))


(defn insertion-sort [a-seq]
(loop [iter 0
       result '()]
  (if (< iter (count a-seq))
    (recur (inc iter) (insert result (get a-seq iter)))
    result)) 
)


(defn insertion-sort [a-seq]
      (reduce insert '() a-seq)
)


(defn toggle [s e]
  (if (contains? s e) (disj s e) (conj s e))
)

(defn parity [a-seq]
      (reduce toggle #{} a-seq)
)

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-* 
      ([] 1)
      ([x] x)
      ([x & more]
          (reduce * x more))
)

; this function is learned from one example mentioned onlion
(defn pred-and
  ([] (constantly true))
  ([f] #(f %)) 
  ([f p] #(and (f %) (p %)))
  ([f p & more] (reduce pred-and (pred-and f p) more)))

(defn zip [seqs]
  (apply map vector seqs)
)

(defn my-map [f & seqs]
  (reduce (fn [init z-seq]
  (conj init (apply f z-seq)))
  []
  (zip seqs))
)

