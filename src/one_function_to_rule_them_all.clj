(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (drop 1 (reduce (fn [x y] (concat x " " y)) "" a-seq)))))

(defn my-interpose [x a-seq]
  (drop 1 (reduce (fn [a b] (conj a x b) ) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (vec [(reduce (fn [left right] (if (> left right) right left)) (first a-seq) a-seq)
        (reduce (fn [left right] (if (< left right) right left)) (first a-seq) a-seq)]))

(defn insert [sorted-seq n]
  (sort (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce (fn [left right] (insert left right)) () a-seq))

(defn parity [a-seq]
  (let [tog (fn toggle [a e]
              (if (contains? a e)
                (disj a e)
                (conj a e)))]
                  (reduce (fn [left right] (tog left right)) #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (let [x (count more)]
    (cond
      (= x 0) 1
      (= x 1) more
      (= x 2) (* (first more) (last more))
      :else (reduce (fn [l r] (* l r)) more))))


(defn pred-and  ([& more]
      (let [x (count more)]
        (cond
          (= x 0) (fn [y] true )
          (= x 1) (fn [y] ((first more) y) )
          (= x 2) (fn [y] (and ((first more) y) ((last more) y)))
          (> x 2) (fn [y] (let [z (reduce (fn [left right] (if (or (keyword? y) (string? y))
                                                                (+ 0 left)
                                                                (if (right y)
                                                                  (inc left)
                                                                  (+ 0 left)))) 0 more)]
                            (if (= x z)
                              true
                              false)))))))


(defn my-map ([f & a-seqs]
                 (if (= 1 (count a-seqs))
                   (reverse (reduce (fn [left right] (conj left (f right))) () (first a-seqs)))
                   "To Be Implemented")))
