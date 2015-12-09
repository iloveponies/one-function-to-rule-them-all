(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq)
            )))

(defn my-interpose [x a-seq]
  (rest (apply concat (map (fn [elem] (conj (cons elem nil) x)) a-seq))))

(defn my-count [a-seq]
   (let [counter (fn [count _]
                   (inc count))]
     (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc elem]
                   (cons elem acc))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [minmaxpair (fn [acc elem]
                     (cond
                       (< elem (first acc)) [elem (second acc)]
                       (> elem (second acc)) [(first acc) elem] 
                       :else acc))]
    (reduce minmaxpair [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (defn insert-helper [acc seq1]
    (cond
      (empty? seq1) (concat acc (list n))
      (> n (first seq1)) (insert-helper (concat acc (list (first seq1))) (rest seq1))
      :else (concat acc (list n) seq1)
      ))
  (insert-helper '() sorted-seq))

(defn insertion-sort [a-seq]
 (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [parity-helper (fn [a-set elem]
                        (if (contains? a-set elem)
                          (disj a-set elem)
                          (conj a-set elem)))]
    (reduce parity-helper #{} a-seq)))
                        

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1) 
  ([x & more] (my-count (cons x more))))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & more]  (reduce * 1 (cons x more))))

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [elem] (x elem)))
  ([x y] (fn [elem] (and (x elem) (y elem))))
  ([x y & more] (reduce pred-and (pred-and x y) more))) 


(defn my-map [f a-seq]
  [:-])

