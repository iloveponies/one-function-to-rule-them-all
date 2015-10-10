(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce into (empty a-seq) a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
      (fn [acc s]
        (str acc " " s))
      a-seq)))

(defn my-interpose [x a-seq]
  (into ()
    (butlast
      (reduce
        (fn [e acc]
          (conj e x acc))
        ()
        a-seq))))

(defn my-count [a-seq]
  (reduce
    (fn [acc e]
      (inc acc))
    0
    a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [[min-el max-el] e]
      [(min min-el e) (max max-el e)])
    [(first a-seq) (first a-seq)]
    a-seq))

(defn insert [sorted-seq n]
  (if (or (empty? sorted-seq)
          (< n (first sorted-seq)))
    (cons n sorted-seq)
    (cons (first sorted-seq)
          (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce
    (fn [acc e]
      (if (some #{e} acc)
        (disj acc e)
        (conj acc e)))
    #{}
    a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce
    (fn [acc e]
      (inc acc))    
    0
    more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([]
   (fn [x] true))
  ([p]
   (fn [x] (p x)))
  ([p1 p2]
   (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f & more]
  (letfn [(map-first [acc e] (conj acc (first e)))
          (map-rest [acc e] (conj acc (rest e)))
          (reduce-with-map-first [a-seq] (reduce map-first () a-seq))
          (reduce-with-map-rest [a-seq] (reduce map-rest () a-seq))]
    (loop [acc () left (reverse more)]
      (if (some empty? left)
        (reverse acc)
        (recur
          (conj acc (apply f (reduce-with-map-first left)))
          (reduce-with-map-rest left))))))
