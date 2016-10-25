(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " "a-seq))))

(defn my-interpose [x a-seq]
(if (empty? a-seq)
    ()
  (let [func (fn [newVec item] (into () (conj newVec x item)))]
  (reduce func [(first a-seq)] (rest a-seq)))))


 (defn my-count [a-seq]
   (if (empty? a-seq)
     0
  (let  [addr (fn [counter item]
               (if (not (nil? num))
                 (inc counter)))]
    (reduce addr (first a-seq) (rest a-seq)))))


  (defn my-reverse [a-seq]
  (let [func (fn [newVec item] (cons item newVec))]
    (reduce func '() a-seq)))


 (defn min-max-element [a-seq]
  (let [func (fn [[smallest biggest] item]
               (cond
                (< item smallest) [item biggest]
                (> item biggest) [smallest item]
                :else [smallest biggest]))]
               (reduce func [(first a-seq) (first a-seq)] (rest a-seq))))


 (defn insert [sorted-seq n]
    (if (empty? sorted-seq)
      (cons n ())
      (if (< n (first sorted-seq))
                    (cons n sorted-seq)
                    (cons (first sorted-seq) (insert (rest sorted-seq) n)))))


  (defn insertion-sort [a-seq]
    (reduce insert () a-seq)
  )

 (defn parity [a-seq]
  (let [toggle (fn [set num]
                 (if (contains? set num)
                   (disj set num)
                   (conj set num)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
 ([& params] (count params)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))


(defn pred-and
   ([] (fn [n] true))
  ([pred] pred)
  ([pred1 pred2] (fn [n] (and (pred1 n) (pred2 n))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  ([func a-seq] (let [helper (fn [sequ item]  (conj sequ (func item)))]
                  (into '() (reduce helper () a-seq))))
  ([func a-seq & more]
   (if (empty? a-seq)
                      '()
(cons (apply func (my-map first (cons a-seq more)))
             (apply my-map func (my-map rest (cons a-seq more)))))))
