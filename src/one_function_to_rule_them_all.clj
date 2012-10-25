(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose \space a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b)) (conj [] (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (let [counter (fn [cou a] (inc cou))]
      (reduce counter a-seq))))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (conj (conj [] (reduce min a-seq)) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (let [inner (fn [sorted-seq n new-seq]
                (cond 
                 (empty? sorted-seq) (conj new-seq n)
                 (> (first sorted-seq) n) (concat (conj new-seq n) sorted-seq)
                 :else (recur (rest sorted-seq) n (conj new-seq (first sorted-seq)))))]
    (inner sorted-seq n [])))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (defn toggle [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))  
  (reduce (fn [x y] (toggle x y)) #{} a-seq))

(defn minus 
  ([x] (- x x x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & preds] 
    (reduce 
      (fn [pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))  
      (fn [x] (and (pred1 x) (pred2 x)))  
      preds)))

(defn my-map [f a-seq]
  [:-])