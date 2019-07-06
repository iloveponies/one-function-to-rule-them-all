(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [y z] (if (empty? y)
                      (conj y z)
                      (conj (conj y x) z)))
          []
          a-seq))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) [] a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [x y] (if (empty? x)
                           [y y]
                           [(min (first x) y) (max (second x) y)]))]
    (reduce helper [] a-seq)))

(defn insert [sorted-seq n]
  (let [helper (fn [x] (< x n))]
    (concat (concat (take-while helper sorted-seq) [n]) (drop-while helper sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem))
  )

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more))
  )

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & params] (fn [el] (reduce (fn [bool-value another-pred] (and bool-value (another-pred el))) (and (x el) (y el)) params))))

(defn my-map [op & seqs]
  (let [firsts (fn [seqs] (loop [acc []
                                 copy-seqs seqs]
                            (if (empty? copy-seqs)
                              acc
                              (recur (conj acc (first (first copy-seqs))) (rest copy-seqs)))))
        rests (fn [seqs] (loop [acc []
                                copy-seqs seqs]
                           (if (empty? copy-seqs)
                             acc
                             (recur (conj acc (rest (first copy-seqs))) (rest copy-seqs)))))
        ]
  (loop [acc []
         copy-seqs seqs]
    (if (some empty? copy-seqs)
    acc
    (recur (conj acc (apply op (firsts copy-seqs))) (rests copy-seqs))))))
