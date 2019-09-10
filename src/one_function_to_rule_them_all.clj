(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [w1 w2] (str w1 " " w2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
    (drop 1 (reduce (fn [a b] (concat a [x b])) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (if b (inc a) a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [s sorted-seq r []]
    (cond
     (empty? s) (conj r n)
     (> (first s) n) (concat (conj r n) s)
     :else (recur (rest s) (conj r (first s))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce 
    (fn [s x]
     (if (contains? s x) (disj s x) (conj s x)))
    #{} a-seq)) 

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 & more] (fn [x] (reduce (fn [a b] (and a (b x))) (p1 x) more))))


(defn my-map 
  ([f a-seq]
   (loop [s a-seq r []]
      (if (empty? s) r
          (recur (rest s) (conj r (f (first s)))))))
   ([f a-seq & more]
    (my-map 
      (fn [x] (apply f x))
      (loop [s (cons a-seq more) r []]
        (if (empty? (first s)) r
            (recur 
              (reduce (fn [a b] (conj a (rest b))) [] s)
              (conj r (reduce (fn [a b] (conj a (first b))) [] s))))))))


