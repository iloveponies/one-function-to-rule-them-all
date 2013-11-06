(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [intr (fn [e1 e2]
               (conj e1 x e2))]
    (drop 1 (reduce intr [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)       (list n)
   (> (first sorted-seq) n)  (cons n sorted-seq)
   :else                     (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [odds (fn [map e]
               (cond
                (contains? map e) (disj map e)
                :else             (conj map e)))]
    (reduce odds #{} a-seq)))

(defn minus
  ([x]   (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([]         0)
  ([x]        1)
  ([x & more] (+ 1 (count more))))

(defn my-*
  ([]           1)
  ([x]          x)
  ([x y]        (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([]             (fn [x] true))
  ([p1]           (fn [x] (p1 x)))
  ([p1 p2]        (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))


(defn my-map [f a-seq]
  [:-])

