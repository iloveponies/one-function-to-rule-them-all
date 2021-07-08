(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq] (reduce concat '() a-seq))

(defn str-cat [a-seq] (if (empty? a-seq) "" (reduce (fn [v r] (str v " " r)) a-seq)))

(defn my-interpose [x a-seq] (drop-last (reduce (fn [interpose-vec elem] (conj (conj interpose-vec elem) x)) [] a-seq)))

(defn my-count [a-seq] (reduce (fn [size elem] (inc size)) 0 a-seq))

(defn my-reverse [a-seq] (reduce (fn [new-seq elem] (conj new-seq elem)) '() a-seq))

(defn min-max-element [a-seq] (reduce (fn [min-max elem] (if (or (< elem (get min-max 0)) (> elem (get min-max 1)))
                                                           (if (< elem (get min-max 0))
                                                             (assoc min-max 0 elem)
                                                             (assoc min-max 1 elem))
                                                           min-max)) [(first a-seq) (first a-seq)] (rest a-seq)))


(defn insert-helper [sorted-seq n new-vec bool_inserted] (if (empty? sorted-seq)
                                              (if bool_inserted
                                                new-vec
                                                (conj new-vec n))
                                              (if (and (> (first sorted-seq) n) (not bool_inserted))
                                                (insert-helper (rest sorted-seq) n (conj (conj new-vec n) (first sorted-seq)) true)
                                                (insert-helper (rest sorted-seq) n (conj new-vec (first sorted-seq)) bool_inserted))))

(defn insert [sorted-seq n] (if (empty? sorted-seq)
                              (cons n '())
                              (insert-helper sorted-seq n [] false)))

(defn insertion-sort [a-seq] (reduce (fn [sorted-vec elem] (insert sorted-vec elem)) [] a-seq))

(defn parity [a-seq] (let [toggle (fn [a-set elem] [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
                       (reduce (fn [odd-set elem] (toggle odd-set elem)) #{} a-seq)))

(defn minus ([x] (- 0 x)) ([x y] (- x y)))

(defn count-params ([] 0) ([x] 1) ([x & more] (reduce (fn [counter elem] (+ counter 1)) 1 more)))

(defn my-* ([] 1) ([x] x) ([x y] (* x y)) ([x y & more] (reduce (fn [product elem] (* product elem)) (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred?] (fn [x] (pred? x)))
  ([pred? pred-2?] (fn [x] (and (pred? x) (pred-2? x))))
  ([pred? pred-2? & more] (fn [x] (reduce (fn [state elem] (if state (elem x) false)) (and (pred? x) (pred-2? x)) more))))

(defn my-map [f a-seq]
  [:-])
