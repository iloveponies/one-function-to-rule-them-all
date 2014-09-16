(ns one-function-to-rule-them-all)

(defn reload [] (use 'one-function-to-rule-them-all :reload))

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (reduce #(conj %1 x %2) (vec [(first a-seq)]) (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [c e]
                  (inc c))]
    (reduce counter 0 a-seq)))


(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) (list) a-seq))

(defn min-max-element [a-seq]
  (let [store (fn [[mi ma] b]
                [(min mi b) (max ma b)])
        ]
    (reduce store [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (loop [input sorted-seq
           acc []
           found false
           ]
      (if (empty? input)
        (conj acc n)
        (if (= n (min (first input) n))
          (flatten (conj acc n input))
          (recur (rest input) (conj acc (first input)) (= n (min (first input) n))))))))

(defn insertion-sort [a-seq]
  (let [ins (fn [s b]
              (insert s b))
        ]
    (reduce ins [] a-seq)))

(defn parity [a-seq]
  ;; Todo was this supposed to be done with reduce?
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [s a-seq
           par #{}
           ]
      (if (empty? s)
        par
        (recur (rest s) (toggle par (first s)))))))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more] (reduce pred-and (pred-and x y) more))
  )

(defn my-map
  ([f a-seq] (map f a-seq))
  ;; Todo do this
  ([f a b] nil))
