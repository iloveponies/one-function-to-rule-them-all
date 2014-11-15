(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n e] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [seq e] (conj seq e)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [head '()
         tail sorted-seq]
    (if (or (empty? tail) (< n (first tail)))
      (concat head (cons n tail))
      (recur (concat head (cons (first tail) '())) (rest tail)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [set e] (if (contains? set e) (disj set e) (conj set e))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (reduce (fn [n e] (inc n)) 0 x))

(defn my-* [& x]
  (reduce * x))

(defn pred-and [& p]
  (reduce (fn [a b] (fn [x] (and (a x) (b x)))) (fn [x] true) p))

(defn my-map [f & a-seq]
  (let [add-to-seq (fn [s e] (concat s (cons e '())))
        seperate-first-and-rest (fn [xs]
                                  (reduce (fn [s x] [(conj (get s 0) (first x)) (conj (get s 1) (rest x))])
                                          [[] []] xs))
        inverted (loop [inv []
                        seqs a-seq]
                   (if (empty? (first seqs))
                     inv
                     (let [[first-vec rest-vec] (seperate-first-and-rest seqs)]
                       (recur (conj inv first-vec) rest-vec))))]
        (reduce (fn [a p] (add-to-seq a (apply f p))) '() inverted)))
