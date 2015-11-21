(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) 
    "" 
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b] (if (empty? a)
                          (seq [b])
                          (concat a (seq [x b]))
    )) () a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b]
    (cond 
      (nil? (first a)) (vector b b)
      (> (get a 0) b) (assoc a 0 b)
      (< (get a 1) b) (assoc a 1 b)
      :else a))
  () a-seq))

(defn insert [sorted-seq n]
  (loop [a-seq sorted-seq
         b-seq '()]
    (cond
      (empty? a-seq) (concat b-seq (vector n))
      (< n (first a-seq)) (concat b-seq (vector n) a-seq)
      :else (recur (rest a-seq) (concat b-seq (vector (first a-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce (fn [a b] 
    (if (contains? (set a) b) 
      (disj (set a) b) 
      (conj (set a) b))) 
    () a-seq))

(defn minus
  ([x] (- 0 x)) 
  ([x y] (- x y)))

(defn count-params
  ([& more] (reduce (fn [a b] (inc a)) 0 more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])