(ns one-function-to-rule-them-all)


(defn concat-elements [a-seq]
  (reduce concat '() a-seq)
  )

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq))
  )

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (conj (into [] (reduce
                    #(concat %1 (vector %2 x))
                    []
                    (drop-last a-seq)))
          (last a-seq)))
  )

(defn my-count [a-seq]
  (reduce (fn [count element]
            (inc count)) 0 a-seq)
  )

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq)
  )

(defn min-max-element [a-seq]
  (reduce #(vector (min (first  %1) %2) (max (last  %1) %2))
          (vector  (first a-seq) (first a-seq))
          (rest a-seq))
  )

(defn insert [sorted-seq n]
  (loop [items []
         a-vec (vec sorted-seq)
         ]
    (cond
     (empty? a-vec) (seq (conj items n))
     (< n (first a-vec)) (seq (apply conj items n a-vec))
     :else (recur (conj items (first a-vec))
                  (rest a-vec))
     )
    )
  )

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn
                 [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq))
  )

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y))
  )

(defn count-params [& x]
  (count x)
  )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
     (reduce *
             (* x y)
             more))
  )

(defn pred-and
  ([]
     (fn [x] (true? true)))
  ([x] x)
  ([pred & more]
     ( reduce (fn [pred1 pred2]
                (fn [y]
                  (and (pred1 y) (pred2 y))))
              pred
              more))
  )

(defn my-map
  ;; single sequence
  ([f a-map]
     (reduce #(conj %1 (f %2) ) [] a-map))
  ;; multiple sequences
  ([f a-map & more]
     (let [make-vector-each-element (fn [a-vec] (reduce #(conj %1 (vector %2))
                                                       []
                                                       a-vec))
           corresponding-conj (fn
                                [a-vec-of-vec a-vec]
                                (loop  [acc []
                                        v1 a-vec-of-vec
                                        v2 a-vec]
                                  (if (empty? v2)
                                    acc
                                    (recur (conj acc (conj (first v1) (first v2)))
                                           (rest v1)
                                           (rest v2)))))]
       (my-map #(apply f %) (reduce corresponding-conj (make-vector-each-element a-map) more))))
  )
