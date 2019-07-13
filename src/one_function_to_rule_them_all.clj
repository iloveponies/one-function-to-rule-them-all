;; http://iloveponies.github.io/120-hour-epic-sax-marathon/one-function-to-rule-them-all.html

(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (reverse (reduce #(cons %2 (cons x %1)) (take 1 a-seq) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [ct _](inc ct)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce
       (fn [[mn mx] n][(min n mn) (max n mx)])
       [(first a-seq) (first a-seq)]
       (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [[f & r] sorted-seq]
      (if (> f n)
        (cons n sorted-seq)
        (cons f (insert (rest sorted-seq) n))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& predicates]
  (fn [x]
    (boolean
     (reduce #(and %1 (%2 x)) true predicates))))

(defn my-map
  ([f a-seq]
    (->
     ((fn [accum the-seq]
       (if (empty? the-seq)
         accum
         (recur (cons (f (first the-seq)) accum) (rest the-seq))))
      '() a-seq)
     reverse))

  ([f a-seq & more]
    (->
     ((fn [accum seqs]
       (if (reduce #(or %1 (empty? %2)) false seqs)
         accum
         (let [firsts (my-map first seqs)
               rests (my-map rest seqs)]
           (recur (cons (apply f firsts) accum) rests)
           )))
      '() (cons a-seq more))
     reverse)))
