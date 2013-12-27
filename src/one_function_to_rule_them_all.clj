(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce (fn
            ([] "")
            ([x y] (str x " " y))) a-seq))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [coll y] (conj coll x y)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [sum _] (inc sum)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [coll y] (conj coll y)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[a b] y] [(min a y) (max b y)])
          [Double/MAX_VALUE Double/MIN_VALUE]
          a-seq))

(defn insert [sorted-seq n]
  (let [pred (fn [x] (< x n))]
    (concat (take-while pred sorted-seq)
            (list n)
            (drop-while pred sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted n] (insert sorted n)) () a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set n] (if (contains? a-set n)
                         (disj a-set n)
                         (conj a-set n)))
          #{} a-seq))

(defn minus
  ([x] (-  x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and [& preds]
  (reduce (fn ([p q] (fn [x] (and (p x) (q x)))))
          (fn [x] true)
          preds))

(defn apply-on-first [f seqs]
  (apply f (reduce (fn [coll x] (concat coll (list (first x))))
                   ()
                   seqs)))

(defn drop-first [seqs]
  (reduce (fn [coll x] (concat coll (list (rest x)))) () seqs))

(defn my-map [f & seqs]
  (loop [res ()
         seqs seqs]
    (if (some empty? seqs)
      res
      (recur (concat res (list (apply-on-first f seqs)))
             (drop-first seqs)))))
