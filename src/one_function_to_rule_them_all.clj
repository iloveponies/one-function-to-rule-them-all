(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) () a-seq))

(defn min-max-element [a-seq]
  (let [mmin (reduce min a-seq)
        mmax (reduce max a-seq)]
    [mmin mmax]))

(defn insert [sorted-seq n]
  (let [[smaller larger] (split-with #(< % n) sorted-seq)]
    (concat smaller (list n) larger)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [counts (frequencies a-seq)]
    (set (for [[k v] counts :when (odd? v)] k))))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x] (count x))

(defn my-*
  ([] 1)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map 
  ([f a-seq]
   (loop [acc [] xs a-seq]
      (if (empty? xs) acc
        (recur (conj acc (f (first xs))) (rest xs)))))
  
  ([f a-seq b-seq]
   (loop [acc [] xs a-seq ys b-seq]
      (if (or (empty? xs) (empty? ys)) acc
        (recur (conj acc (f (first xs) (first ys))) 
               (rest xs)
               (rest ys)))))
  
  ([f a-seq b-seq & more]
   (loop [seqs (concat [a-seq b-seq] more)
          acc []]
     ;; if any of the sub-sequences is exhausted return
     (if (contains? (set (map empty? seqs)) true) acc
       (recur (map rest seqs) 
              (conj acc (apply f (map first seqs))))))))
