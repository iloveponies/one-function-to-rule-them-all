(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc el] (str acc " " el)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (empty? (rest a-seq)) (seq a-seq)
   :else (reduce (fn [acc el] (concat acc [x el]))
                 [(first a-seq)]
                 (rest a-seq))))


(defn my-count [a-seq]
  (reduce (fn [acc el] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x]
            (cons x acc))
          '()
          a-seq))


(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (reduce
     (fn [acc x]
            [(min (first acc) x) (max (second acc) x)])
     [(first a-seq) (first a-seq)]
     a-seq)))


(defn insert [sorted-seq n]
  (loop [le []
         ri sorted-seq]
    (cond
     (empty? ri) (concat le [n])
     (< n (first ri)) (concat le [n] ri)
     :else (recur (concat le [(first ri)]) (rest ri)))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc x]
            (insert acc x))
          '()
          a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (reduce (fn [acc x]
            (toggle acc x))
          #{}
          a-seq))


(defn minus
  ([x]
   (* -1 x))
  ([x y]
   (- x y)))

(minus 2)
(minus 4 3)

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
   (reduce (fn [acc z] (inc acc)) 2 more )))

(defn my-*
  ([]    1)
  ([x]   x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
     (reduce (fn [acc p]
               (pred-and acc p))
             (pred-and p1 p2)
             more)))


(defn firsts [a-seq]
  (reduce (fn [acc x] (conj acc (first x)))
          []
          a-seq))

(defn rests [a-seq]
  (reduce (fn [acc x] (conj acc (rest x)))
          []
          a-seq))


(defn param-seq [a-seq & more]
  (loop [x-seq a-seq
         acc1 []
         acc2 (cons a-seq more)]
    (if (empty? x-seq)
      acc1
      (recur (rest x-seq)
             (conj acc1 (firsts acc2))
             (rests acc2)))))

(defn my-map

  ([f a-seq]
   (reduce (fn [acc x] (conj acc (f x)))
           []
           a-seq))

  ([f a-seq & more]
   (let [p-seq (apply param-seq (cons a-seq more))]
     (reduce (fn [acc x] (conj acc (apply f x)))
             []
             p-seq))))
