(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s w] (str s " " w))
            (first a-seq)
            (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c))
          0
          a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [r e] (cons e r))
            [(first a-seq)]
            (rest a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce (fn [[min max] n]
              (if (< n min) [n max])
              (if (> n max) [min n]))
            [(first a-seq) (first a-seq)]
            (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [b []
           a sorted-seq]
      (cond 
        (empty? a) (conj b n)
        (< n (first a)) (concat b [n] a)
        :else (recur
                (conj b (first a))
                (rest a))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- 0 x))
  ([a b] (- a b)))

(defn count-params 
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] 
   (reduce my-*
           (my-* x y)
           more)))

(defn pred-and 
  ([] (fn [_] true))
  ([x] (fn [n] (x n)))
  ([x y] (fn [n] (and 
                   (x n)
                   (y n))))
  ([x y & more] (reduce
                  pred-and
                  (pred-and x y)
                  more)))

(defn firsts [seq-seq]
  (if (empty? seq-seq)
    ()
    (cons 
      (first (first seq-seq))
      (firsts (rest seq-seq)))))

(defn rests [seq-seq]
  (if (empty? seq-seq)
    ()
    (cons
      (rest (first seq-seq))
      (rests (rest seq-seq)))))

(defn anyleft? [seqs]
  (loop [s seqs]
    (if (empty? s) false
      (if (empty? (first s)) 
        (recur (rest s))
        true))))

(defn my-map 
  ([f & seqs] 
   (loop [retval ()
          r-seqs seqs]
     (if (anyleft? r-seqs)
       (recur
         (conj retval (apply f (firsts r-seqs)))
         (rests r-seqs))
       (reverse retval)))))
