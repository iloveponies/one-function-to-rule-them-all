(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (seq a-seq)
    (reduce (fn [acc s] (str acc " " s)) a-seq)
    ""))

(defn my-interpose [x a-seq]
  (if (seq a-seq)
    (rest (reduce (fn [acc y] (conj acc x y)) [] a-seq))
    '()))

(defn my-count [a-seq]
  (reduce (fn [cnt _] (inc cnt)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [[ls hs] (split-with (partial > n) sorted-seq)]
    (concat ls (cons n hs))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (letfn
      [(toggle [a-set e] (if (contains? a-set e) (disj a-set e) (conj a-set e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [& preds]
  (reduce (fn [pred1 pred2] (fn [x] (and (pred1 x)
                                         (pred2 x))))
          (fn [_] true)
          preds))

(defn zip2 [xss ys]
  (->
   (reduce (fn [[zss xss] y]
             (if (seq xss)
               [(conj zss (conj (first xss) y)) (rest xss)]
               [zss '()]
               ))
           [[] xss]
           ys)
   first))

(defn my-map [f & seqs]
  (if (seq seqs)
    (->> (reduce zip2 (repeat []) seqs)
         (reduce (fn [ys xx] (conj ys (apply f xx))) []))
    '()
    ))

