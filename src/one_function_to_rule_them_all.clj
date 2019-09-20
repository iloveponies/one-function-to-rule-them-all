(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2)
            a-seq)))

(defn my-interpose [x a-seq]
  (cond (empty? a-seq) []
        (empty? (rest a-seq)) a-seq
        :else (cons (first a-seq)
                    (reduce #(conj %1 x %2 ) [] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [init x]
            (inc init))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
    (reduce (fn [x y]
              (vector (min (first x) y)
                      (max (second x) y)))

            [(first a-seq) (first a-seq)]
            a-seq)))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (list n)
        (< n (first sorted-seq))
        (cons n
              sorted-seq)
        :else (cons (first sorted-seq)
                    (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x (* 2 x)))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
    ([] (fn [pred1] true))
    ([pred1] pred1)
    ([pred1 pred2]
     (fn [x] (and (pred1 x) (pred2 x))))
    ([pred1 pred2 & more]
     (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  ([f a-seq] (reduce (fn [seq-1 elem]
                       (conj seq-1 (f elem)))
                     []
                     a-seq))
  ([f a-seq b-seq]
   (let [merge-seq (fn self [first-seq second-seq]
                     (if (and (empty? first-seq) (empty? second-seq)) '()
                       (cons (concat (flatten [(first first-seq)]) [(first second-seq)])
                             (self (rest first-seq) (rest second-seq)))))]
     (reduce (fn [seq-1 elem]
               (conj seq-1 (apply f elem)))
             []
             (merge-seq a-seq b-seq))))
  ([f a-seq b-seq & more]
   (reduce (fn [seq-1 elem]
             (my-map f seq-1 elem))
           (my-map f a-seq b-seq)
           more)))
