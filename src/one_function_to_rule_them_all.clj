(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose \space a-seq))))

(defn my-interpose [x a-seq]
 (reverse
  (drop-last
   (reduce (fn [new ele] (conj new x ele)) '() a-seq))))

(defn my-count [a-seq]
  (reduce (fn [counter ele] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [new ele] (conj new ele)) '() a-seq))

(defn min-max-element [a-seq]
  (let [placer (fn [[x y] ele]
                 (cond
                  (> x ele)
                    [ele y]
                  (< y ele)
                    [x ele]
                  :else
                    [x y]))]
  (reduce placer [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq)
    (list n)
   (< n (first sorted-seq))
    (conj (seq sorted-seq) n)
   :else
   (conj (insert (rest sorted-seq) n) (first sorted-seq))))

(defn insertion-sort [a-seq]
  (let [insert1 (fn [new-seq elem]
                  (insert new-seq elem))]
    (reduce insert1 '() a-seq)))

(defn parity [a-seq]
  (let [insert1 (fn [new-set elem]
                 (if (contains? new-set elem)
                   (disj new-set elem)
                   (conj new-set elem)))]
    (reduce insert1 #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] (* x))
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p z] (fn [x] (and (p x) (z x))))
  ([p z & more] (reduce pred-and (pred-and p z) more)))

(defn my-mapper [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
      (cons (f (first seq-1) (first seq-2))
          (my-mapper f (rest seq-1) (rest seq-2)))))

(defn my-map
  ([f x] (reverse (reduce (fn [x y] (conj x (f y))) '() x)))
  ([f x y] (my-mapper f x y))
  ([f x y & more] (let [ok (reduce (fn [z o] (my-map f z o))  (my-map f x y) more)
                        [x y z] ok]
                    (if (< (count (set ok)) 3)
                      ok
                      [(flatten x) (flatten y) (flatten z)]))))
