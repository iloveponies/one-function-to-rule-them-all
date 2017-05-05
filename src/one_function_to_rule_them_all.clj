(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [res el] (str res " " el)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (take (- (* 2 (count a-seq)) 1) (reduce (fn [res el] (conj res el x)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt el] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [rev el] (cons el rev)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [s sorted-seq
         n n
         first-half []]
    (cond
      (empty? s) (concat first-half [n])
      (<= n (first s)) (concat first-half (cons n s))
      :else (recur (rest s) n (conj first-half (first s))))))

(defn insertion-sort [a-seq]
  (reduce (fn [res el] (insert res el)) [] a-seq))

(defn parity [a-seq]
  (reduce (fn [res el] (if (contains? res el)
                         (disj res el)
                         (conj res el))) #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& rest]
  (reduce (fn [res el] (inc res)) 0 rest))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & rest] (reduce my-* (my-* x y) rest)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p p2] (fn [x] (and (p x) (p2 x))))
  ([p p2 & rest] (reduce pred-and (pred-and p p2) rest)))

(defn my-map
  ([f a-seq] (reduce (fn [res el] (conj res (f el))) [] a-seq))
  ([f a-seq & more]
   (loop [all (cons a-seq more)
          result []]
      (if (empty? (first all))
        result
        (recur (my-map rest all) (conj result (apply f (my-map first all))))))))
