(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [f (fn [ys y] (conj ys x y))]
    (rest (reduce f [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x _] (+ x 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [xs x] (concat [x] xs)) [] a-seq))

(defn min-max-element [a-seq]
  (let [fst (first a-seq)
        f (fn [min-max x] 
            (if (< x (first min-max)) 
              [x (second min-max)]
              (if (> x (second min-max))
                [(first min-max) x]
                min-max)))]
   (reduce f [fst fst] a-seq)))

(defn insert [sorted-seq n]
  (let [f (fn [x] (< x n))]
  	(concat (take-while f sorted-seq) [n] (drop-while f sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [s x] (if (contains? s x) (disj s x) (conj s x)))]
  (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and [& fs] 
   (fn [x] 
     (let [f (fn [b p] (and b (p x)))]
       (reduce f true fs))))

(defn my-map
  ([f a-seq]
   (let [g (fn [ys x] (conj ys (f x)))]
     (reduce g [] a-seq)))
  ([f a-seq & more]
   (let [all-seqs (cons a-seq more)]
     (my-map #(apply f %) 
          (loop [acc []
                 consume all-seqs]
            (if (not (empty? (filter empty? consume)))
              acc
              (recur (concat acc [(my-map first consume)])
                     (my-map rest consume))))))))