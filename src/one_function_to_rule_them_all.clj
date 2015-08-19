(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
  (reduce (fn[acc elem] (str acc " " elem)) a-seq)))


(defn my-interpose [x a-seq]
  (rest (reduce (fn[acc rest-of-seq] (conj (conj acc x) rest-of-seq)) [] a-seq)))

;seems kinda dirty...
(defn my-count [a-seq]
  (reduce (fn[acc useless_torso] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[acc seq] (cons seq acc)) [] a-seq))

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
   (reduce * more))

(defn pred-and
  ([] (fn[x] true))
  ([p1] p1)
  ([p1 p2] (fn[x] (if (and (p1 x) (p2 x))
                        true
                        false)))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])
