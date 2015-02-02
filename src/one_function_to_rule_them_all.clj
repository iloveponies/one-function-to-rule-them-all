(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
   (apply str [(str (first a-seq) " ")
               (reduce (fn [acc, x] (str acc " " x)) (rest a-seq))])))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
    (drop-last (reduce (fn [acc j] (conj (conj acc j) x)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc j] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc i] (cons i acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mi ma] i] [(min mi i) (max ma i)]) [9999 -1] a-seq))

(defn insert [sorted-seq n]
  (concat (take-while #(<= % n) sorted-seq) [n] (drop-while #(<= % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce (fn [acc n] (insert acc n)) [] a-seq))

(defn toggle [seq-1 eleme]
    (if (contains? seq-1 eleme)
          (disj seq-1 eleme)
          (conj seq-1 eleme)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (reduce (fn [acc x] (inc acc)) 2 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ;([x y] (* x y))
  ([x y & more] (reduce (fn [acc i] (* acc i)) (* x y) more)))

(defn pred-and
  ([] true)
  ([x] x)
  ([x y] (and x y))
  ([x y & more] (reduce (fn [acc i] (and acc i)) (and x y) more)))

(defn my-map [f a-seq]
  [:--])
