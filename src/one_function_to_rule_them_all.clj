(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [funk (fn [x y] (str x " " y))]
   (if (empty? a-seq)
     ""
     (reduce funk a-seq))))

(defn my-interpose [x a-seq]
  (let [fudge (fn [z y] (conj x y))]
  (reduce fudge (first a-seq) (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [x e] (inc x))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
   (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(defn insert [sorted-seq n]
  nil)

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus ([x] (- 0 x))
             ([x y] (- x y)))

(defn count-params [x]
  (reduce inc 0 x))

(defn my-* ([] 1)
            ([x] x)
            ([x y] (* x y))
            ([x y & more] (reduce * (* x y) more))
  )

(defn pred-and
               ([] (fn [x] true))
               ([p] p)
               ([p b] (fn [x] (if (and (p x) (b x)) true false)))
               ([p b & more]
                (reduce pred-and
                       (fn [x] (if (and (p x) (b x))
                                   true
                                   false))
                        more)))


(defn my-map [f a-seq]
  [:-])
