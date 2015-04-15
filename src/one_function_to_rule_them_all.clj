(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (cond (empty? a-seq) a-seq
        :else
        (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(comment
(defn my-count [a-seq]
      (loop [left a-seq acc 0]
        (if (empty? left) acc
            (recur (rest left) (inc acc)))))
)
(defn my-count [a-seq]
  (if (empty? a-seq) 0
      (reduce (fn [a b] (inc a)) 0 a-seq)))

(defn my-reverse [a-seq]
  (cond
    (empty? a-seq) a-seq
    :else
    (reduce (fn [x y] (cons y x)) [(first a-seq)] (rest a-seq))))

(defn min-max-element [a-seq]
  (cond
    (empty? a-seq) a-seq
    :else
    (reduce 
     (fn [a x] 
       (let [mi (get a 0)
             ma (get a 1)]
         [(min mi x) (max ma x)]))
     [(first a-seq) (first a-seq)] 
     (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [left sorted-seq acc []]
    (if 
        (empty? left) (conj acc n)
        (let [f (first left)
              r (rest left)]
          (if (>= f n) 
            (concat (conj acc n) left)
            (recur (rest left) (conj acc (first left))))))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) a-seq
      (reduce insert [] a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))
  

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & y] (reduce (fn [x y] (inc x)) 1 y)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [a] true))
  ([x] (fn [a] (x a)))
  ([x & more] (fn [a] (reduce (fn [n m] (and n (m a))) true (cons x more)))))

(defn map-helper [f sq]
  (loop [left sq acc []]
    (if (empty? left) acc
        (recur (rest left) (conj acc (f (first left)))))))

(defn s-mapper [f seqs]
  (loop [left seqs acc []]
    (let [head (map-helper first left)
          tail (map-helper rest left)]
      (if (empty? (first left)) acc
          (recur tail (conj acc (apply f head)))))))

(defn my-map
  ([f sq] (map-helper f sq))
  ([f sq & more] (s-mapper f (cons sq more))))
