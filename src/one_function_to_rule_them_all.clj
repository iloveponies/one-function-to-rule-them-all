(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (seq a-seq)
    (reduce #(str %1 " " %2) a-seq)
    ""))

(defn my-interpose [x a-seq]
  (if (seq a-seq)
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))
    ()))

(defn my-count [a-seq]
  (reduce #(if %2 
            (inc %1) 
            (inc %1)) 
          0 
          a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))


(defn min-max-element [a-seq]
  (reduce (fn [[min-item max-item] next-item]
            [(if min-item
               (min min-item next-item)
               next-item)
             (if max-item
               (max max-item next-item)
               next-item)])
          [] a-seq))

(defn insert [sorted-seq n]
  (loop [new-seq []
         index-seq sorted-seq]
    (cond 
      (empty? index-seq) (seq (conj new-seq n))
      (< n (first index-seq)) (cons n index-seq)
      (nil? (second index-seq)) (if (< (first index-seq) n) 
                                  (concat new-seq index-seq [n])
                                  (concat new-seq [n] index-seq))
      (< (first index-seq) n (second index-seq)) (concat new-seq [(first index-seq) n] (rest index-seq))
      :else (recur (conj new-seq (first index-seq)) (rest index-seq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2)
            (disj %1 %2)
            (conj %1 %2)) 
          #{} a-seq))

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
  ([] (fn [item] true))
  ([x] (x))
  ([x y] #(and (x %1) (y %1)))
  ([x y & more] 
    (fn [item]
      (reduce #(and %1 (%2 item)) true (concat [x y] more)))))

#_(defn my-map
  ([f a-seq] (loop [result []
                    the-seq a-seq] 
               (if (seq the-seq)
                 (recur (conj result (f (first the-seq))) (rest the-seq))
                 result)))
  )

(defn my-map [f & more-seqs]
  (let [first-of-all (fn [seqs]
                       (reduce #(conj %1 (first %2)) [] seqs))
        rest-of-all (fn [seqs]
                      (reduce #(conj %1 (rest %2)) [] seqs))
        any-empty? (fn [seqs]
                    (reduce #(or %1 (empty? %2)) false seqs))]
     (loop [result [] 
            the-seqs more-seqs] 
       (if (any-empty? the-seqs) 
         result
         (recur (conj result (apply f (first-of-all the-seqs))) (rest-of-all the-seqs))))))
  
