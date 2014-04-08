(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose2 [x a-seq]
  (flatten (reduce #(cons %1 (cons x (cons %2 nil))) a-seq)))

(defn my-interpose [x a-seq]
  (drop-last (reduce #(conj (conj %1 %2) x) [] a-seq)))

(my-interpose "-" ["foo" "bar"])

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [result value]
                 (cond
                   (< value (first result))
                     [value (second result)]
                   (> value (second result))
                     [(first result) value]
                   :else
                     result))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [result []
         sorted sorted-seq
         n n]
  (cond
   (empty? sorted)
     (concat result (cons n nil))
   (>= (first sorted) n)
     (concat result (cons n sorted))
   :else
     (recur (conj result (first sorted))
            (rest sorted) n))))

(insert [4 5 12 15] 14)

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(insertion-sort [1 4 3 2 2 5])

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & args]
   (reduce my-* (my-* x y) args)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & args]
   (reduce pred-and (pred-and p1 p2) args)))

(comment (defn my-map
  ([f a-seq]
   (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq b-seq]
   (let [pairs (fn pairs [a-seq b-seq]
                 (if (or (empty? (rest a-seq)) (empty? (rest b-seq)))
                   (list [(first a-seq) (first b-seq)])
                   (cons [(first a-seq) (first b-seq)] (pairs (rest a-seq) (rest b-seq)))))]
     (reduce #(conj %1 (f (first %2) (second %2))) [] (pairs a-seq b-seq))))
  ([f a-seq b-seq & args]
   (reduce #(my-map f %1 %2) (my-map f a-seq b-seq) args))))

(defn my-map
  ([f a-seq]
   (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq & args]
   (let [combine (fn [colls]
                   (partition (count colls) (apply interleave colls)))]
     (my-map #(apply f %) (combine (cons a-seq args))))))
