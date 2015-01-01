(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq)
  )

(defn str-cat [a-seq]
  (if (empty? a-seq)
   ""
   (reduce
    (fn [first second]
      (str first " " second))
    a-seq))
  )

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce
    (fn [first second]
        (conj (conj first x) second))
    [(first a-seq)]
    (rest a-seq))
    )
  )

(defn my-count [a-seq]
  (let [counter (fn [count not-needed]
                  (inc count))]
  (reduce counter 0 a-seq))
  )

(defn my-reverse [a-seq]
  (reduce conj '() a-seq)
  )

(defn min-max-element [a-seq]
  (let [min-element (fn [min elem]
                      (if (< elem min)
                        elem
                        min))
        max-element (fn [max elem]
                      (if (> elem max)
                        elem
                        max))]
    [(reduce min-element a-seq)
     (reduce max-element a-seq)])
  )

(defn insert-helper[sorted-seq result-seq n]
  (cond
    (empty? sorted-seq) (conj result-seq n)
    (< n (first sorted-seq)) (into [] (concat (conj result-seq n) sorted-seq))
    :else (insert-helper (rest sorted-seq) (conj result-seq (first sorted-seq)) n))
  )

(defn insert [sorted-seq n]
    (insert-helper sorted-seq [] n)
  )

(defn insertion-sort-helper [a-seq result-seq]
  (reduce insert )
  )

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (reduce toggle #{} a-seq)
  )

(defn minus
  ([x] (* x -1))
  ([x y] (- x y))
  )

(defn count-params [& args]
  (my-count args)
  )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more))
  )

(defn pred-and-helper [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x)))
  )

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (pred-and-helper pred1 pred2))
  ([pred1 pred2 & more] (reduce pred-and-helper (pred-and-helper pred1 pred2) more))
  )

(defn my-map-helper [f a-seq result-seq]
  (conj result-seq (f (first a-seq)))
  )

(defn sum [a-seq]
  (let [sum-helper (fn [acc a-seq]
                     (if (empty? a-seq)
                       acc
                       (recur (+ acc (first a-seq))
                              (rest a-seq))))]
    (sum-helper 0 a-seq)))

(defn to-one-vector
  ([x] [x])
  ([x y] [x y])
  ([x y & more] (reduce conj [x y] more))
  )

(defn firsts [seq]
  (reduce
   (fn [acc x]
     (conj acc (first x)))
  []
   seq)
  )

(defn rests [seq]
  (reduce
   (fn [acc x]
     (conj acc (rest x)))
  []
   seq)
  )

(defn first-things-first [seq]
  (let [helper (fn [acc seq]
                (if (empty? (first seq))
                  acc
                  (recur (conj acc (firsts seq))
                         (rests seq))))]
    (helper [] seq)
    ))

(defn my-map-implementer
  [f a-seq]
  (reduce (fn [a-acc a-next]
            (conj a-acc
                  (apply f a-next)))
          []
          a-seq)
  )

(defn my-map
  ([f seq-1] (my-map-implementer f (first-things-first [seq-1])))
  ([f seq-1 seq-2] (my-map-implementer f (first-things-first [seq-1 seq-2])))
  ([f seq-1 seq-2 & more] (my-map-implementer f (first-things-first
                                                (reduce conj [seq-1 seq-2] more))))
  )

