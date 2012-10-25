(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if(empty? a-seq)
    ""
    (reduce (fn [acc x] (str acc " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (let [f (fn [acc a] (conj acc x a))]
    (rest(reduce f [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc a] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[acc a] (cons a acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn[[x y] a][(min x a) (max y a)]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [p []
         r sorted-seq
         n n]
    (cond
      (empty? r)
        (conj p n)
      (<= n (first r))
        (concat p [n] r)
      :else 
        (recur (conj p (first r)) (rest r) n))))

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])