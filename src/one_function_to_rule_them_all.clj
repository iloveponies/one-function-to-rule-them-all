(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [str-space (fn [str1 str2]
                      (str str1 " " str2))]
      (reduce str-space a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [coll elem]
                  (conj coll x elem))]
    (rest (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if e
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[min max] elem]
                  (cond
                    (< elem min)
                      (vector elem max)
                    (> elem max)
                      (vector min elem)
                    :else
                      (vector min max)))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (cond
    (not n)
      sorted-seq
    (empty? sorted-seq)
      (list n)
    (>= (first sorted-seq) n)
      (cons n (insert sorted-seq nil))
    :else
      (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))
    #{}
    a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([]
    (fn [x] true))
  ([p]
    (fn [x] (p x)))
  ([p q]
    (fn [x] (and (p x) (q x))))
  ([p q & more]
    (reduce pred-and (pred-and p q) more)))

(defn my-map
  ([f a-seq]
    (seq (reduce (fn [seq x] (conj seq (f x))) [] a-seq)))
  ; ([f s1 s2]
  ;   (if (and (empty? s1) (empty? s2))
  ;     '()
  ;     (cons (f (first s1) (first s2))
  ;           (my-map f (rest s1) (rest s2)))))
  ([f s1 & coll]
    (let [colls (cons s1 coll)]
      (if (some empty? colls)
        '()
        (cons (apply f (my-map first colls))
              (apply my-map f (my-map rest colls)))))))
