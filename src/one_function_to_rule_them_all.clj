(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [s, f]
                 (str s " " f))]
    (if (empty? a-seq)
    ""
    (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [s, f]
                 (if (> (count s) 0)
                   (conj s x f)
                   (conj s f)))]
    (if (empty? a-seq)
      '()
      (reduce helper '() (reverse a-seq)))))

(defn my-count [a-seq]
  (let [helper (fn [s f]
                 (inc s))]
    (if (empty? a-seq)
      0
      (reduce helper a-seq))))

(defn my-reverse [a-seq]
  (loop [rev '()
         q a-seq]
    (if (empty? q)
      rev
      (recur (conj rev (first q)) (rest q)))))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [bf '()
         q sorted-seq
         nn n]
    (let [a (first q)]
      (cond
       (empty? q) (if (= nn nil)
                    (reverse bf)
                    (reverse (conj bf n)))
       (= nn nil) (recur (conj bf a) (rest q) nil)
       (<= n a) (recur (conj bf n a) (rest q) nil)
       :else (recur (conj bf a) (rest q) nn)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  [:-])

(defn minus
  ([a] (- a))
  ([a b] (- a b)))

(defn count-params
  ([& more]
   (count more)))

(defn my-*
  ([] 1)
  ([a] a)
  ([a & more]
   (reduce * a more)))

(defn muut [fs x]
  (let [pred (first fs)]
    (cond
     (empty? fs) true
     (pred x) (muut (rest fs) x)
     :else true)))

(defn pred-and
  ([] (fn [x] (true? true)))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (and (p1 x) (p2 x) (muut more x)))))

(defn my-map [f a-seq]
  [:-])
