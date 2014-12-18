(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq ))

(defn str-cat [a-seq]
  (if (empty? a-seq) 
    ""
    (str (reduce #(str %1 " " %2) a-seq))))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [a b] (inc a))]
   (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [minmaxer #(if (empty? %1) 
                    (repeat 2 %2)
                    [(min (first %1) %2) 
                     (max (second %1) %2)])]
    (reduce minmaxer [] a-seq)))

(defn insert [sorted-seq n]
  (loop [result []
         remaining sorted-seq] 
    (if (or (empty? remaining) (< n (first remaining))) 
      (concat result (cons n remaining))
      (recur (conj result (first remaining))
             (rest remaining)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (apply disj [a-set elem])
    (apply conj [a-set elem])))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

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
  ([p] p)
  ([p q] #(and (p %1) (q %1)))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(declare my-map0)

(defn my-zip [some-seqs]
  (loop [result []
         rest-seqs some-seqs]
    (if (empty? (first rest-seqs))
      result
      (recur (conj result (my-map0 first rest-seqs))
             (my-map0 rest rest-seqs))) ))

(defn my-map0
  ([f a-seq] 
     (reduce #(conj %1 (f %2)) []  a-seq))
  ([f a-seq & other-seqs]
     (my-map0 #(apply f %1) 
              (my-zip (cons a-seq other-seqs)))))

(defn my-map 
  ([f a-seq] 
     (reduce #(conj %1 (f %2)) []  a-seq))
  ([f a-seq & other-seqs]
     (loop [all-seqs (cons a-seq other-seqs)
            result  []]
       (if (empty? (first all-seqs) ) 
         result
         (recur (my-map rest all-seqs) 
                (conj result 
                      (apply f (my-map first all-seqs))))))))
