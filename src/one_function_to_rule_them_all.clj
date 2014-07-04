(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 \space %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a _](inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max [a b]
  [(min a b) (max a b)])

(defn min-max-compare [[min1 max1] a]
  [(min min1 a) (max max1 a)])

(defn min-max-element [[f & a-seq]]
  (if (empty? a-seq)
    [f f]
    (reduce min-max-compare
            [f f]
            a-seq)))

(defn insert [sorted-seq n]
  (let [[lesser greater](split-with #(< % n) sorted-seq)]
    (concat lesser (list n) greater)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity-helper [parity-set a]
  (if (parity-set a)
    (disj parity-set a)
    (conj parity-set a)))

(defn parity [a-seq]
  (reduce parity-helper #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & all] (apply * (conj all x y))))

(defn pred-and
  ([] (constantly true))
  ([x] (constantly x))
  ([x y] #(and (x %) (y %)))
  ([x y & more] (fn [k]
                  (reduce #(if % (%2 k)) true (list* x y more)))))

(defn my-map [f & all-seq]
  (if (some empty? all-seq)
    ()))
