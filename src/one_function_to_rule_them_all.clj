(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce #(concat %1 %2) '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
  (into '() (reverse
   (reduce #(conj %1 x %2)  [(first a-seq)] (rest a-seq))))))

(defn my-count [a-seq]
  (reduce (fn [par1 par2] (inc par1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
    (reduce #(vector (min (first %1) %2)
                     (max (second %1) %2))
            [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (concat
   (take-while #(< % n) sorted-seq)
   (conj
    (drop-while #(< % n) sorted-seq)
    n)))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) [(first a-seq)] (rest a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce #(toggle %1 %2) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& params]
   (reduce #(* %1 %2) 1 params))

(defn pred-and
  ([] (fn [par] true))
  ([& params]
   (reduce (fn [pred1 pred2] #(and (pred1 %) (pred2 %)))
           (first params)
           (rest params))))


(defn my-map [f & seqs]
  (reverse
  (reduce (fn [arg1 arg2] (conj
                           arg1
                           (if (seq? arg2)
                             (apply f arg2)
                             (f arg2))))
          '()
          (if (= 1 (count seqs)) (first seqs)
            (partition (count seqs) (apply interleave seqs))))))
