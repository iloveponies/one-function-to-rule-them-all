(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [sep (fn [x y] (str x " " y))]
      (reduce sep a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [sep (fn [a b] (conj a x b))]
      (reduce sep [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [c elem] (inc c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (cond
    (empty? a-seq) '()
    :else (let [reverser (fn [s elem] (cons elem s))]
            (reduce reverser (list (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (let [minmax (fn [[total-min total-max] elem]
                   [(min total-min elem) (max total-max elem)])]  
    (reduce minmax [100000 0] a-seq)))

(defn insert [sorted-seq n]
  (loop [start-seq '()
         end-seq sorted-seq]
    (let [lower (concat start-seq (list n))
          upper (concat (list n) end-seq)]
      (cond
        (and (empty? start-seq) (empty? sorted-seq)) (list n)
        (empty? end-seq) (concat start-seq (list n))
        (and (apply <= lower) (apply <= upper)) (concat start-seq (list n) end-seq)
        :else (recur (concat start-seq (list (first end-seq))) (rest end-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [items (frequencies a-seq)
        odd-check (fn [data-set elem] 
                    (if (odd? (items elem))
                      (conj data-set elem)
                      data-set))]
    (reduce odd-check #{} a-seq)))

(defn minus 
  ([x] (* -1 x)) 
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* 
  ([] 1)
  ([x] x) 
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more))
  )

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (if (and (p1 x) (p2 x)) true false)))
  ([p1 p2 & more] (fn [x] 
                    (reduce (fn [res pred] (and res (pred x))) 
                            (and (p1 x) (p2 x))
                            more))))

(defn my-map
  ([f a-seq] 
     (let [h (fn [acc elem] (concat acc (list (f elem))))]
               (reduce h '() a-seq)))
  ([f a-seq & more]
     (let [total-seqs (cons a-seq more)
           min-seq-count (apply min (map count total-seqs))
           interleaved-seqs (apply interleave total-seqs) 
           partitioned-seqs (partition min-seq-count interleaved-seqs)
           helper-fn (partial apply f)]
       (my-map helper-fn partitioned-seqs))))
