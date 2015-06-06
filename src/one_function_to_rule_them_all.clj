(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond (empty? a-seq) ""
        :else (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce + (vals (frequencies a-seq))))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-element (reduce min a-seq)
        max-element (reduce max a-seq)]
    [min-element max-element]))

(defn insert [sorted-seq n]
      (loop [acc []
             sorted-seq sorted-seq]
        (cond
          (empty? sorted-seq) (conj acc n)
          (> n (first sorted-seq)) (recur (conj acc (first sorted-seq))
                                            (rest sorted-seq))
          :else (apply conj acc n sorted-seq))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce #(if (not (contains? %1 %2))
             (conj %1 %2)
             (disj %1 %2)) #{} a-seq))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& args]
   (count args)))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (my-* x y) more)))

(defn pred-and
  ([]
   (fn [a-seq] a-seq))
  ([pred1]
   (fn [a-seq] (pred1 a-seq)))
  ([pred1 pred2]
   (fn [a-seq]
     (and (pred1 a-seq)
          (pred2 a-seq))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f a-seq]
  [:-])
