(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (fn [a b] (conj (conj a x) b))]
      (reduce f [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [f (fn [count element] (inc count))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [some-seq element]
            (conj some-seq element))]
    (reduce f '() a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [[current-min current-max] element]
            [(min current-min element)
             (max current-max element)])]
    (reduce f [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [acc      []
         some-seq sorted-seq
         added    false]
    (cond
      (and (empty? some-seq) added) (seq acc)
      (empty? some-seq) (seq (conj acc n))
      (and (> (first some-seq) n) (not added)) (recur (conj acc n (first some-seq)) (rest some-seq) true)
      :else
      (recur (conj acc (first some-seq)) (rest some-seq) added))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [f (fn [x y]
            (if (contains? x y)
              (disj x y)
              (conj x y)))]
    (reduce f #{} a-seq)))

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  "negates or subtracts two numbers"
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  "Accepts any number of params and returns the param count"
  [& more]
    (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq]
     (reduce (fn [x y] (conj x (f y))) []  a-seq))
  ([f a-seq & other-seqs]
     (loop [all-seqs (cons a-seq other-seqs)
            acc  []]
       (if (empty? (first all-seqs) )
         acc
         (let [new-value (apply f (my-map first all-seqs))
               rest-seqs (my-map rest all-seqs) ]
           (recur rest-seqs (conj acc new-value)))))))
