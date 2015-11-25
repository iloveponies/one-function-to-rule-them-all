(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
  ""
  (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc value] (conj acc x value)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc value] (if (nil? value) acc (inc acc))) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc value] (cons value acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [_compare (fn [a-seq _comp]
                     (reduce (fn [acc value] (if (_comp value acc) acc value)) a-seq))]
    [(_compare a-seq <) (_compare a-seq <)]))

(defn insert [sorted-seq n]
      (let [split (split-with (fn [i] (<= i n)) sorted-seq)]
            (concat (first split) [n] (last split))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
(letfn [(toggle [acc n]
  (if (contains? acc n)
    (disj acc n)
    (conj acc n)))]
  (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([& more] (count more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (apply * x y more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p] p)
  ([p p2] (fn [x] (and (p x) (p2 x))))
  ([p p2 & more] (reduce pred-and (pred-and p p2) more)))

(defn my-map [f a-seq]
  [:-])
