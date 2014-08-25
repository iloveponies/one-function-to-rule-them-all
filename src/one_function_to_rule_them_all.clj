(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(apply str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (let [coll (seq a-seq)]
    (cond
     (empty? coll) '()
     (empty? (next coll)) coll
     :else
     (seq (reduce #(conj %1 x %2) (vector (first coll)) (next coll))))))

(defn my-count [a-seq]
    (reduce (fn [acc item] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce
   (fn [[amin amax] item]    ; start with [min max] acc
     (let
       [newmin (if (< item amin) item amin)
        newmax (if (> item amax) item amax)]
       (vector newmin newmax)))
   [(first a-seq) (first a-seq)]
   (next a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)
   (list n)
   (< n (first sorted-seq))
   (cons n sorted-seq)
   :else
   (cons (first sorted-seq) (insert (next sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [aset item]
                 (if-not (aset item) (conj aset item) (disj aset item)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([& params]
   (fn [x]
     (loop [preds params]
       (cond
        (empty? preds)
        true
        (not ((first preds) x))
        false
        :else
        (recur (next preds)))))))

(defn my-map
  ([f a-seq]
   (if (empty? a-seq)
   '()
   (cons (f (first a-seq)) (my-map f (next a-seq))))))
;  ([f a-seq & more-seq])
;  )
