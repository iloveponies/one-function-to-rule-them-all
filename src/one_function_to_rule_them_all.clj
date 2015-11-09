(ns one-function-to-rule-them-all)


(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x y] [(apply min (conj x y)) (apply max (conj x y))]) [] a-seq))

(defn insert [sorted-seq n]
  (loop [p-1 []
         p-2 sorted-seq]
    (let [checked-seq (apply conj p-1 (cons n p-2))]
      (if (apply <= checked-seq)
        checked-seq
        (recur (conj p-1 (first p-2)) (rest p-2))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))


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
