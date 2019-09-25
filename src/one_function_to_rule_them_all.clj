(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [elem1 elem2] (str elem1 " " elem2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [vect elem] (conj (conj vect x) elem)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count elem] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [vect elem] (conj vect elem)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [min-max (fn [[min max] new-elem]
                    (if (< new-elem min)
                        [new-elem max]
                        (if (> new-elem max)
                            [min new-elem]
                            [min max])))]
  (reduce min-max (apply vector (repeat 2 (first a-seq))) (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [insert-flag false
         b-seq-result []
         b-seq-orig sorted-seq]
    (if (empty? b-seq-orig)
      (if insert-flag
        b-seq-result
        (conj b-seq-result n))
      (let [insert-ev (and (< n (first b-seq-orig)) (not insert-flag))
            conj-new (conj (if insert-ev
                             (conj b-seq-result n)
                             b-seq-result) (first b-seq-orig))]
      (recur (if insert-ev true insert-flag) conj-new (rest b-seq-orig))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [n elem] (inc n)) 0 more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (reduce (fn [f1 f2] (fn [k] (and (f1 k) (f2 k)))) (fn [k] true) more))

(defn my-map [f & more]
  (let [apply-first (fn [b-seq] (apply f (reduce (fn [vect a-seq] (conj vect (first a-seq))) [] b-seq)))
        seq-rest (fn [b-seq] (reduce (fn [vect a-seq] (conj vect (rest a-seq))) [] b-seq))]
    (loop [part-1 []
           part-2 more]
      (if (some empty? part-2)
        part-1
        (recur (conj part-1 (apply-first part-2)) (seq-rest part-2))))))





