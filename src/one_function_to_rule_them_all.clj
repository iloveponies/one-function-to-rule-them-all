(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (cond (empty? a-seq) '()
        :else (seq (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [x1 x2] (+ x1 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x1 x2] (conj x1 x2)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x1 x2] (if (empty? x1)
                        [x2 x2]
                        [(min (nth x1 0) x2) (max (nth x1 1) x2)]))
          [] a-seq))

(defn insert [sorted-seq n]
  (let [ [x1 x2] (split-with #(< % n) sorted-seq)]
    (concat x1 (cons n x2))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2) (disj %1 %2) (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x] (reduce #(and %1 (%2 x)) true more)))

(defn my-map [f a-seq]
  [:-])
