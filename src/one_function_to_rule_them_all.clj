(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [add-sep (fn [a-seq elem]
                    (conj a-seq x elem))]
      (rest (reduce add-sep [] a-seq)))))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (my-interpose \space a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [n e]
                  (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [get-min-max (fn [min-max e]
                      (let [min (min-max 0)
                            max (min-max 1)
                            new-min (if (or (nil? min) (> min e))
                                      e
                                      min)
                            new-max (if (or (nil? max) (< max e))
                                      e
                                      max)]
                        [new-min new-max]))]
    (reduce get-min-max [nil nil] a-seq)))

                      
(defn insert [sorted-seq n]
  (loop [left-seq []
         right-seq sorted-seq]
    (if (empty? right-seq)
      (concat left-seq (conj right-seq n))
      (if (<= n (first right-seq))
        (concat (conj left-seq n) right-seq)
        (recur (conj left-seq (first right-seq))
               (rest right-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (my-count xs))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [e] true))
  ([p] (fn [e] (p e)))
  ([p1 p2] (fn [e] (and (p1 e) (p2 e))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] 
    (let [do-map (fn [acc e]
                   (conj acc (f e)))]
      (reduce do-map [] a-seq)))
  ([f a-seq b-seq]
    (loop [s1 a-seq
           s2 b-seq
           acc []]
      (println s1 s2 acc)
      (if (or (empty? s1) (empty? s2))
          acc
          (recur (rest s1)
                 (rest s2)
                 (conj acc (f (first s1) (first s2)))))))
  ([f a-seq b-seq & more]    
      (loop [seqs (concat [a-seq b-seq] more) 
           acc []]
      (if (contains? (set (map empty? seqs)) true)
        acc
        (recur (map rest seqs)
               (conj acc (apply f (map first seqs))))))))
          
        
                      
                  