(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (seq (reduce (fn [acc a]
                     (conj acc x a)) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc a] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() (seq a-seq)))

(first [1 2])

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc a] 
              [(min (first acc) a) (max (second acc) a)])
            [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [smaller (fn [x] (< x n))
        init (take-while smaller sorted-seq)
        tail (drop-while smaller sorted-seq)]
    
  (concat init (cons n tail))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc x]
            (if (contains? acc x)
              (disj acc x)
              (conj acc x))) #{} a-seq))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and [& preds]
  (fn [x] (reduce (fn [acc p] (and acc (p x))) true preds)))

(defn my-map [f & seqs]
  (let [helper (fn [resultacc seqs]
                 (if (some empty? seqs)
                   resultacc
                   (let [firsts (reduce (fn [acc s] (conj acc (first s))) [] seqs)
                         rests (reduce (fn [acc s] (conj acc (rest s))) [] seqs)
                         newresultacc (conj resultacc (apply f firsts))]
                     (recur newresultacc rests))))]
    (helper [] seqs)))
                    
(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))