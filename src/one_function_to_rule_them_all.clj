(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) 
          [] 
          a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c e] (inc c)) 
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) 
          ()
          a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[x1 x2] elem]
                 [(min x1 elem) 
                  (max x2 elem)])]
    (reduce helper
            [(first a-seq) (first a-seq)]
            (rest a-seq))))

(defn insert [sorted-seq n]
  (concat
    (take-while #(> n %1) sorted-seq)
    (list n)
    (drop-while #(> n %1) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([f] f)
  ([f g] (fn [x] 
           (and (f x) 
                (g x))))
  ([f g & more] (reduce pred-and (pred-and f g) more)))

(defn my-map 
  ([f col] (for [item col]
             (f item)))
  ([f col & more]
     (let [min-size (apply min (my-map count more))]
       (for [index (range min-size)]
         (apply f
                (my-map #(nth % index) 
                        (conj more col)))))))
