(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " "a-seq))))

(defn my-interpose [x a-seq]
  (let [cat (fn [s e]
      (if (empty? s)
        (conj s e)
        (conj (conj s x) e)))]
  (reverse (reduce cat () a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                    (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [s e]
              (cons e s))]
  (reduce rev () a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n ())
    (if (< (first sorted-seq) n)
      (cons (first sorted-seq) (insert (rest sorted-seq) n))
      (cons n sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [par (fn [s e]
              (if (contains? s e)
       (disj s e)
       (conj s e)))]
  (reduce par #{} a-seq)))

(defn minus ([x]
  (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more]
   (let [my-inc (fn [s e]
                  (inc s))]
   (reduce my-inc 0 more))))

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
