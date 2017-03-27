(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [cat (fn [i s] (str i " " s))]
      (reduce cat a-seq)))) ; join would work too

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [x-between (fn [acc s] (concat acc (vector x s)))]
      (reduce x-between (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [acc e]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [put-first (fn [acc e]
                    (cons e acc))]
    (reduce put-first [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc e]
                  (vector (min (first acc) e)
                          (max (last acc) e)))]
    (reduce min-max
            (vector (first a-seq) (first a-seq))
            a-seq)))

(defn insert [sorted-seq n]
  (loop [pre []
         s sorted-seq]
    *(cond
      (empty? s) (conj sorted-seq n)
      (< n (first s)) (concat pre (cons n s)) ; Adds "n" in order
      :else (recur (conj pre (first s)) (rest s)))))

(defn insertion-sort [a-seq]
  (let [sorter (fn [sorted e]
                 (insert sorted e))
        sortedish (reduce insert [] a-seq)]
    (if (apply <= sortedish)
      sortedish
      (insertion-sort sortedish))))
; The function may only partially sort longer lists. Therefore we use recursion

(defn parity [a-seq]
   (let [toggle (fn [s elem]
                  (if (contains? s elem)
                    (disj s elem)
                    (conj s elem)))]
     (reduce toggle #{} a-seq)))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [n] true))
  ([x] x)
  ([x y] (fn [n] (and (x n) (y n))))
  ([x y & more] (fn [n] (loop [acc (and (x n) (y n))
                               f more]
                          (if (or (empty? f) (not acc))
                            acc
                            (recur ((first f) n) (rest f)))))))

(defn my-map 
  ([f a-seq]
   (loop [acc []
          s a-seq]
     (if (empty? s)
       acc
       (recur (conj acc (f (first s))) (rest s)))))

  ([f a-seq & more-seqs]
    (let [firsts (fn [ss] ; ss = sequence of sequences
                  (loop [a []
                         s ss]
                    (if (empty? s)
                      a
                      (recur (conj a (first (first s))) (rest s)))))
          rests (fn [ss]
                  (loop [a []
                         s ss]
                    (if (empty? (rest (first s)))
                      a
                      (recur (conj a (rest (first s))) (rest s)))))]
      (loop [acc []
             seqs (cons a-seq more-seqs)]
       (if (empty? seqs)
        acc
        (recur (conj acc (apply f (firsts seqs))) (rests seqs))))))) 
