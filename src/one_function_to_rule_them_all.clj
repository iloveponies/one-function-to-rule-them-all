(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (rest (reduce #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [[min max] new]
                 [(if (< new min) new min)
                  (if (> new max) new max)])]
    (reduce helper (repeat 2 (first a-seq)) (rest a-seq))))

(defn insert [sorted-seq n]
  (concat (take-while #(< % n) sorted-seq)
          [n]
          (drop-while #(< % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& more]
   (reduce * more))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] #(and (pred1 %) (pred2 %)))
  ([pred1 pred2 & preds]
   (reduce pred-and (pred-and pred1 pred2) preds)))

(defn my-map
  ([f a-seq]
   (if (empty? a-seq)
     a-seq
     (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq & more]
   (let [seqs   (cons a-seq more)
         firsts (map first seqs)
         rests  (map rest  seqs)]
     (if (some empty? seqs)
       '()
       (cons (apply f firsts) (apply my-map f rests))))))
