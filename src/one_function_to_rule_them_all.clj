(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
 (let [cat-str (fn [line word] (str line " " word))]
  (if (empty? a-seq)
    ""
    (reduce cat-str a-seq))))

(defn my-interpose [x a-seq]
  (let [inter (fn [a-vec el] (conj a-vec x el))]
   (if (< (count a-seq) 2)
     a-seq
     (reduce inter [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [ac v] (inc ac)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a-seq an-el] (cons an-el a-seq)) [] a-seq))

(defn min-max-element [a-seq]
 (let [reduc-func (fn [[mini maxi] el] [(if (< el mini) el mini) (if (> el maxi) el maxi)])]
  (if (empty? a-seq)
    nil
    (reduce reduc-func [(first a-seq) (first a-seq)] (rest a-seq)))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (let [reducing-func (fn [s-seq el] (insert s-seq el))]
    (reduce reducing-func [] a-seq)))

(defn toggle [a-set elem]
  (let [added (conj a-set elem) deleted (disj a-set elem)]
    (if (contains? a-set elem) deleted added)
  )
)

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [ac v] (inc ac)) 0 more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [c] true))
  ([x] x)
  ([x y] (fn [c] (and (x c) (y c))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map-one-element [f a-seq]
  (if (empty? a-seq)
    []
    (if (coll? (first a-seq))
      (cons (apply f (first a-seq)) (my-map-one-element f (rest a-seq)))
      (cons (f (first a-seq)) (my-map-one-element f (rest a-seq))))))

(defn reduce-to-single-collection [seq-so-far new-seq]
  (if (or (empty? seq-so-far) (empty? new-seq))
    []
    (cons (conj (first seq-so-far) (first new-seq)) 
          (reduce-to-single-collection (rest seq-so-far) (rest new-seq)))))

(defn my-map 
  ([f a-seq] (my-map-one-element f a-seq))
  ([g a-seq & more] 
   (let [trans-f (fn [s] (if (coll? s) s [s]))]
   (my-map-one-element g (reduce reduce-to-single-collection (map trans-f a-seq) more)))))

(defn reducing-map [f x]
  (if (coll? x)
    (fn [y] (apply f (conj x y)))
    (fn [y] (f x y))))

(defn combine [function-seq a-seq]
  (if (or (empty? function-seq) (empty? a-seq))
    []
    (cons ((first function-seq) (first a-seq)) 
          (combine (rest function-seq) (rest a-seq)))))

(defn my-map2 
  ([f a-seq] (map f a-seq))
  ([g a-seq b-seq] (let [reduced (my-map2 (fn [x] (reducing-map g x)) a-seq)]
                      (combine reduced b-seq)))
  ([h a-seq b-seq & more] 
   (reduce (fn [a b] (my-map2 h a b)) (my-map2 h a-seq b-seq) more)))