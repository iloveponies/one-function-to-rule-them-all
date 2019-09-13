(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))



(defn str-cat [a-seq]
  (if (empty? a-seq) ""
     (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq ) ()
    (reduce (fn [accum s2 ]  (if (empty? accum) [s2] (conj accum x s2))) () a-seq)))


(defn my-count [a-seq]
  (reduce (fn [counter x] (if (nil? x) counter (inc counter))) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [accum el] (cons el accum)) () a-seq))


(defn min-max-element [a-seq]
  (reduce (fn [[old-min old-max] x] [(min old-min x) (max old-max x)]) [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (let [frst (first sorted-seq)] (cond (empty? sorted-seq ) [n]
        (> n frst) (cons frst  (insert (rest sorted-seq) n))
        :else (cons n sorted-seq))))


(defn insertion-sort [a-seq]
  (reduce insert [(first a-seq)]  (rest a-seq)))


(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem ) (conj a-set elem)))]
  (reduce (fn [parity-set x] (toggle parity-set x)) #{} a-seq)))


(defn minus ([x]
  (- 0 x))
  ([ x y] (- x y)))

(defn count-params [ & x]
  (reduce (fn [acc x] (inc acc)) 0 x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & others] (fn [x] (reduce (fn [answer p] (and answer (p x))) (and (pred1 x) (pred2 x)) others)))
   )

(defn my-map-helper [f seq]
  (loop [accum  []  n seq]
    (if (empty? n ) accum
      (recur (conj  accum (f (first n)) ) (rest n)))))

(defn my-map [f a-seq & others]
  (if (zero? (count others))
    (my-map-helper f a-seq) ;; just run the 1 argument map function
    ;; need to get the 'first of each sequence
    ;; ie for (my-map + [1 2 3] [4 5 6] [7 8 9])
    ;; we want [ (+ 1 4 7) (+ 2 5 8) (+ 3 6 9)]
    (loop [accum () temp-seq (cons a-seq others)]
      (if (empty? (first temp-seq)) (reverse accum)
      (recur (conj accum (apply f (my-map-helper first temp-seq))) (my-map-helper rest temp-seq))))))



;;(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
;;(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
;;(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))

