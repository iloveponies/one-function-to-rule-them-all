(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a x b))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [xs e] (conj xs e)) '() a-seq))

(defn min-max-element [a-seq]
  (def min-max
    (fn [mm e]
      (cond
       (< e (mm 0))
       [e (mm 1)]

       (> e (mm 1))
       [(mm 0) e]

       :else
       mm)))
  (if (== (count a-seq) 1)
  [(first a-seq) (first a-seq)]
  (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [xs []
         tail sorted-seq]
    (cond
     (empty? tail)
     (conj xs n)

     (> n (first tail))
     (recur (conj xs (first tail)) (rest tail))

     :else
     (concat xs [n] tail))))

(defn insertion-sort [a-seq]
  (reduce (fn [xs e] (insert xs e)) [] a-seq))

(defn parity [a-seq]
  (def toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem))))

  (reduce
   (fn [xs e]
     (toggle xs e)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x]
     (reduce
      (fn [b p] (and b (p x)))
      (and (p1 x) (p2 x))
      more))))

(defn my-map
  ([f & more]
   (def compact (fn [s1 s2]
     (if (empty? s1)
       (reduce (fn [xs e] (conj xs [e])) [] s2)
       (loop [acc []
              ss1 s1
              ss2 s2]
         (if (empty? ss1)
           acc
           (recur (conj acc (cons (first ss2) (first ss1)))
                  (rest ss1)
                  (rest ss2)))))))


   (if (== (count more) 1)
     (reduce
      (fn [xs e] (conj xs (f e))) [] (first more))
     (reduce
      (fn [xs e] (conj xs (apply f e))) []
      (reduce compact [] more)))))
