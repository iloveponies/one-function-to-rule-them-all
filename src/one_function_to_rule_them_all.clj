(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [str-space (fn [x y] (str x " " y))]
      (reduce str-space a-seq))))

(defn my-interpose [x a-seq]
  (let [addx (fn [a b] (conj a x b))]
    (rest (reduce addx [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [c e]
                  (if (nil? e)
                    c)
                    (inc c))]
   (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [a b]
              (cons b a))]
    (reduce rev [] a-seq)))

(defn mm [[min max] e]
  (cond
   (<= e min) (vector e max)
   (>= e max) (vector min e)
   :else (vector min max)))

(defn min-max-element [a-seq]
  (let [mm (fn [[min max] e]
             (cond
              (<= e min) (vector e max)
              (>= e max) (vector min e)
              :else (vector min max)))]
    (reduce mm [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [debut []
         a-seq sorted-seq]
    (cond
     (empty? a-seq) (conj debut n)
     (< n (first a-seq)) (concat debut (cons n a-seq))
     :else (recur (conj debut (first a-seq)) (rest a-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [par (fn [acc e]
              (if (contains? acc e)
                (disj acc e)
                (conj acc e)))]
    (reduce par #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq]
   (reverse
    (loop [acc '()
           s a-seq]
      (if (empty? s)
        acc
        (recur (conj acc (f (first s))) (rest s))))))
  ([f seq1 seq2]
   (loop [acc '()
          s1 seq1
          s2 seq2]
     (if (and (empty? s1) (empty? s2))
       acc
       (recur (conj acc (f (first s1) (first s2)))
              (rest s1)
              (rest s2)))))
  ([f seq1 seq2 & more]
   (let [mym (fn [s1 s2] (my-map f s1 s2))]
     (reduce mym (my-map f seq1 seq2) more))))

