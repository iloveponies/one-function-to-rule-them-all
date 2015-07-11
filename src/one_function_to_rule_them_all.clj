(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat-helper [a-seq elem]
  (str a-seq " " elem))

(defn str-cat [a-seq]
  (str (reduce str-cat-helper (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (seq (reduce (fn [first second] (conj (conj first x) second)) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [first second]
            (cons second first))]
    (reduce f () a-seq)))

(defn min-max-element [a-seq]
  (if (empty? (rest a-seq))
    [(first a-seq) (first a-seq)]
  (let [f (fn [the-vec elem]
            (cond
             (< elem (first the-vec)) [elem (second the-vec)]
             (> elem (second the-vec)) [(first the-vec) elem]
             :else
             the-vec))]
    (reduce f [999 0] a-seq))))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params
  ([& params] (count params)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (let [f (fn [ret elem]
             (* ret elem))]
     (reduce f (* x y) more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([x y] (fn [z]
           (if (and (x z) (y z)) true false)))
  ([x y & more] (fn [elem]
                  (let [f (fn [pred1 pred2]
                            (and pred1 (pred2 elem)))]
                    (reduce f (and (x elem) (y elem)) more)))))

(defn my-map [f a-seq]
  [:-])



























