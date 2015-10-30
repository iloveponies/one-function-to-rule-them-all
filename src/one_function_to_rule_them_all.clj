(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
   (reduce concat '() a-seq))

(defn str-cat [a-seq]
   (if (empty? a-seq)
     ""
     (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
   (if (empty? a-seq)
     '()
     (rest (reduce #(conj %1 x %2) [] a-seq))))


(defn my-count [a-seq]
   (reduce (fn [n d] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
   (reduce conj '() a-seq))

(defn min-max-element [a-seq]
   (reduce (fn [x y] [(min (first x) y) (max (second x) y)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
   (loop [s sorted-seq
         r []]
      (cond (empty? s) (list n)
            (empty? (rest s)) (if (< (first s) n)
                                (concat r (list (first s) n))
                                (concat r (list n (first s))))
            (< n (first s)) (concat r (cons n s))
            :else (recur (rest s) (conj r (first s))))))

(defn insertion-sort [a-seq]
   (reduce insert [] a-seq))

(defn parity [a-seq]
   (set  (reduce #(if (odd? (second %2)) (cons (first %2) %1)) {} (frequencies a-seq))))

(defn minus
   ([x] (- x))
   ([x y] (- x y)))

(defn count-params [& x]
   (count x))

(defn my-*
   ([] 1)
   ([x] x)
   ([x & more] (reduce * x more)))

(defn pred-and
   ([] #(= % %))
   ([p] p)
   ([p & more] (reduce (fn [p1 p2] #(and (p1 %) (p2 %))) p more)))


(defn my-map 
   ([f a-seq]
    (loop [s a-seq
           r '()]
      (if (empty? s)
        r
        (recur (rest s) (concat r (list (f (first s))))))))
 
   ([f a-seq1 a-seq2]
    (loop [s1 a-seq1
           s2 a-seq2
           r '()]
      (if (or (empty? s1) (empty? s2))
        r
        (if (coll? (first s1))
          (recur (rest s1) (rest s2) (concat r (list (apply f (first s2) (first s1)))))
          (recur (rest s1) (rest s2) (concat r (list (f (first s1) (first s2)))))))))
   ([f a-seq1 a-seq2 & more]
    (reduce (fn [x y] (my-map f x y)) (my-map f a-seq1 a-seq2) more)))
