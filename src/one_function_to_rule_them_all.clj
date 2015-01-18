(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (str acc " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (reduce (fn [acc s] (conj acc x s)) (cons (first a-seq) '()) (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc s] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc x] (conj acc x)) '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    '()
    (reduce 
     (fn [acc x]
       (cond
        (< x (acc 0)) [x (acc 1)]
        (> x (acc 1)) [(acc 0) x]
        :else acc)) 
     [(apply min a-seq) (apply max a-seq)]
     a-seq)))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq) (list n) (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) 
             (disj %1 %2)
             (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  [& params]
  (count params))

(defn my-* [& x]
  (cond
   (= 0 (count x)) 1
   (= 1 (count x)) x
   (= 2 (count x)) (* (first x) (second x))
   :else (reduce * 1 x)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
