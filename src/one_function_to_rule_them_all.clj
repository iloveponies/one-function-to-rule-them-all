(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq)
)

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce #(str %1 " " %2) a-seq)
))

(defn my-interpose [x a-seq]
  (let [interposer  (fn [a b] (conj (conj a x) b)) ]

    (if (empty? a-seq) []
        (rest (reduce interposer [] a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq)
)

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) [] a-seq)
)

(defn min-max-element [a-seq]
  (let [ min-max
         (fn [[mini maxi] b]
             [ (min b mini) (max b maxi)])]

  (if (empty? a-seq) []
      (reduce min-max [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  (loop [ heads []
          tails sorted-seq
          n n]
    (cond
     (empty? tails)       (conj heads n)
     (<= n (first tails)) (concat heads [n] tails)
     :else                (recur (conj heads (first tails)) (rest tails) n))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))


(defn parity [a-seq]
  (let [parcheck
        (fn [curset x]
            (if (contains? curset x)
              (disj curset x)
              (conj curset x)))]
  (reduce parcheck #{} a-seq)))


(defn minus
  ( [x] (- 0 x))
  ( [x y] (- x y))
)


(defn count-params
  ( [& more] (count more))
)

(defn my-*
  ( [] 1)
  ( [x] x)
  ( [x & more] (reduce #(* %1 %2) x more))
)

(defn p12 [p1 p2] (fn [x] (and (p1 x) (p2 x))))

(defn pred-and
  ( [] (fn [x] true))
  ( [pred] (fn [x]  (pred x)))
  ( [pred & more] (reduce p12 pred more))
)

(defn single-map [f a-seq]
  (loop [ result [] f f a-seq a-seq]
    (if (empty? a-seq) result
    (recur (conj result (f (first a-seq))) f (rest a-seq)))))


(defn pair [l1 l2]
    (loop [ result []
            l1 l1
            l2 l2
            ]
    (if (or (empty? l1) (empty? l2)) result
        (let [next-result (conj (first l1) (first l2))]
        (recur (conj result next-result) (rest l1) (rest l2))))))


(defn my-map
  (   [f a-seq]
      (single-map f a-seq))

  (   [f a-seq & more]
      (let [init   (single-map (fn [x] [x]) a-seq)
            paired (reduce pair init more)]
          (single-map (fn [x] (apply f x)) paired)
        )))


