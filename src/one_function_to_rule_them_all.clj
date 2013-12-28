(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
   (seq (reduce (fn [acc s] (conj acc x s))
           [(first a-seq)]
           (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc s] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc s] (conj acc s)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [acc []
         a-seq sorted-seq]
    (if (nil? a-seq)
      acc
      (if (or (empty? a-seq) (>= (first a-seq) n))
        (recur (concat acc [n] a-seq) nil)
        (recur (conj acc (first a-seq)) (rest a-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc elem]
            (if (contains? acc elem)
              (disj acc elem)
              (conj acc elem))) #{}
          a-seq))

(defn minus
  ([x]
   (- 2))
  ([x y]
   (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and
  ([]
   (fn [x] true))
  ([x]
   (fn [z] (x z)))
  ([x y]
   (fn [z] (and (x z) (y z))))
  ([x y & z]
   (reduce pred-and (pred-and x y) z)))

(defn my-map [f & a-seq]
  )



