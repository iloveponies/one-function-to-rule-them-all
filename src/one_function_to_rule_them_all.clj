(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) []
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c _] (+ c 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (reduce #(conj []
                 (min (first %1) %2)
                 (max (second %1) %2))
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (loop [left []
         right sorted-seq]
      (cond
        (empty? right) (conj left n)
        (>= (first right) n) (concat (conj left n) right)
        :else (recur (conj left (first right)) (rest right)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? % %2)
             (disj % %2) (conj % %2))
          #{} a-seq))

(defn minus
    ([x] (- x))
    ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (concat [x y] more))))

(defn pred-and
  ([] (fn [_] true))
  ([x] x)
  ([x y] (fn [p] (and (x p) (y p))))
  ([x y & more] (reduce pred-and (concat [x y] more))))

(defn my-map
  ([f x]
   (reduce
     (fn [acc cur] (conj acc (f cur)))
     [] x))
  ([f x & more]
   (loop [data (cons x more)
          hist []]
     (if (some empty? data) hist
       (recur (my-map #(rest %) data)
              (conj hist (apply f (my-map #(first %) data))))))))
