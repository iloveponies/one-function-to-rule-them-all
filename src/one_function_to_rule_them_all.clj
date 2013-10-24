(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [spaced-concat
        (fn [s x]
          (str s (str " " x)))]
  (if (empty? a-seq)
    ""
    (reduce spaced-concat a-seq))))

(defn my-interpose [x a-seq]
  (let [f
        (fn [acc e]
          (conj acc x e))]
  (if (empty? a-seq)
    '()
    (reduce f [(first a-seq)] (rest a-seq)))))

(conj [] "a" "b")
(my-interpose 0 [1 2 3])

(defn my-count [a-seq]
  (let [counter (fn [cnt e]
                  (inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [acc e]
              (cons e acc))]
    (reduce rev '() a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [min-max e]
            [(min (first min-max) e)
             (max (first (rest min-max)) e)])]
    (reduce
     f
     [(first a-seq) (first a-seq)]
     a-seq)))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
    (cond
     (empty? tail)
      (conj head n)
     (< (first tail) n)
      (recur (conj head (first tail))
             (rest tail))
     :else
      (concat head [n] tail))))

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

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * more))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more]
   (reduce pred-and (pred-and p q) more)))

(defn first-of-each [a-seq]
  (loop [acc []
         tails a-seq]
    (if (empty? tails)
      acc
      (recur (conj acc (first (first tails)))
             (rest tails)))))

(defn first-of-each [a-seq]
  (loop [acc []
         tails a-seq]
    (if (empty? tails)
      acc
      (recur (conj acc (first (first tails)))
             (rest tails)))))

(defn rest-of-each [a-seq]
  (loop [acc []
         tails a-seq]
    (if (empty? tails)
      acc
      (recur (conj acc (rest (first tails)))
             (rest tails)))))


(first-of-each [[1 2 3] [4 5 6] [7 8 9]])

(rest-of-each [[1 2 3] [4 5 6] [7 8 9]])


(defn my-map [f & more]
  (loop [acc []
         tails more]
    (if (some empty? tails)
      acc
      (recur (conj acc (apply f (first-of-each tails)))
             (rest-of-each tails)))))

(my-map inc [1 2 3 4])

(my-map + [1 1 1] [1 1 1] [1 1 1])
(my-map vector [1 2 3] [1 2 3] [1 2 3])
