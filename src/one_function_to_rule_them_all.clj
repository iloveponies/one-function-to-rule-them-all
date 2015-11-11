(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(format "%s %s" %1 %2) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(concat %1 (list x %2)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a _] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (reduce #(vector (min (first %1) %2) (max (second %1) %2)) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [s sorted-seq
         r ()]
    (if (apply <= (filter (complement nil?) (list (last r) n (first s))))
      (concat r (list n) s)
      (recur (rest s) (concat r (list (first s)))))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce #((if (%1 %2) disj conj) %1 %2) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (my-* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p1] p1)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn firsts [seqs]
  (loop [f ()
         s seqs]
    (if (empty? s)
      f
      (recur (conj f (first (first s))) (rest s)))))

(defn rests [seqs]
  (loop [r ()
         s seqs]
    (if (empty? s)
      r
      (recur (conj r (rest (first s))) (rest s)))))

(defn zip [seqs]
  (reverse
    (loop [zip-seq ()
           rest-seqs seqs]
      (if (some empty? rest-seqs)
        zip-seq
        (recur (conj zip-seq (firsts rest-seqs)) (rests rest-seqs))))))


(defn my-map [f & seqs]
  (reverse 
    (loop [results ()
           tuples (zip seqs)]
      (if (empty? tuples)
        results
        (recur (conj results (apply f (first tuples))) (rest tuples))))))
