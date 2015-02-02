(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) () (rest (reduce #(conj % x %2) [] (vec a-seq)))))

(defn my-count [a-seq]
  (reduce #(if (identity %2) (inc %) (inc %)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(concat [%2] %1) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq)
   (reduce max a-seq)])

(defn insert [sorted-seq n]
  (seq (concat
         (vec (take-while #(< % n) sorted-seq))
         [n]
         (vec (drop-while #(< % n) sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce #(insert % %2) [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? % %2) 
             (disj % %2) 
             (conj % %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& params]
  (reduce * params))

(defn pred-and
  ([] (fn [x] true))
  ([p] #(p %))
  ([p1 p2 & preds] 
   #(loop [[f & r] (conj preds p1 p2)
           res true]
      (cond 
        (false? res) false
        (empty? r) (f %)
        :else (recur r (f %))))))

(defn my-map 
  ([f a-seq] (reduce #(conj % (f %2)) [] a-seq))
  ([f a-seq & s] 
   (reverse 
     (let [firsts (fn [x] (reduce #(conj % (first %2)) () x))
           rests (fn [x] (reduce #(conj % (rest %2)) () x))]
       (loop [ss (conj s a-seq)
              res ()]
         (if (empty? (first ss))
           res
           (recur (rests ss) (conj 
                               res 
                               (apply f (firsts ss))))))))))
