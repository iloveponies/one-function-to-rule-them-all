(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [cat-space (fn[x,y] (str x " " y))]
      (reduce cat-space a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [interp-fn (fn[a,b] (conj a x b))]
      (reduce interp-fn [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn[count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [prepend (fn[sub-seq e] (cons e sub-seq))]
    (reduce prepend [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [update-minmax (fn[minmax e]
                          (cond
                            (< e (first minmax))
                              [e (last minmax)]
                            (> e (last minmax))
                              [(first minmax) e]
                            :else
                              minmax))]
      (reduce update-minmax [(first a-seq) (first a-seq)] (rest a-seq)))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [prefix (take-while (fn[x] (< x n)) sorted-seq),
          suffix (drop (count prefix) sorted-seq)]
      (concat prefix [n] suffix)))) 

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce insert [(first a-seq)] (rest a-seq))))

(defn parity [a-seq]
  (let [update-parity (fn[par-set e]
                        (if (contains? par-set e)
                          (disj par-set e)
                          (conj par-set e)))]
    (reduce update-parity #{} a-seq)))

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
  ([] (fn[x] true))
  ([p] (fn[x] (p x)))
  ([p q] (fn[x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map-univariate [f a-seq]
  (if (empty? a-seq)
    ()
    (cons (f (first a-seq))
          (my-map-univariate f (rest a-seq)))))

; Transposes a list of sequences
; (trans-seq-list ([1 2 3] [4 5 6])) => ([1 4] [2 5] [3 6])
(defn trans-seq-list [orig-list]
  (let [get-nth (fn[n] (my-map-univariate (fn[x] (get x n)) orig-list)),
        len-seq (count (first orig-list))]
    (my-map-univariate get-nth (range 0 len-seq))))

; First transposes the input so all the first elements of the sequences are together, all of the second elements are together, ...
; Then applies the function to each element of this transposed list in turn
(defn my-map 
  ([f & more] (let [seq-ord (trans-seq-list more)]
                (my-map-univariate (fn[x] (apply f x)) seq-ord))))



