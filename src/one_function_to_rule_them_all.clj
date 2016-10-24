(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
   (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [acc e]
                (if (empty? acc)
                 (conj acc e)
                 (conj acc x e)))]
    (reduce helper [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc b] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc n]
            (cond
              (< n (first acc))  [n (second acc)]
              (> n (second acc)) [(first acc) n]
              :else acc))
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (<= n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [acc x] (if (contains? acc x) (disj acc x) (conj acc x))) #{} a-seq))

(defn minus
  ([x]   (* -1 x))
  ([x y] (- x y)))

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
