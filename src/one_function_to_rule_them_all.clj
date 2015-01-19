(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
    (if (empty? a-seq)
      '()
      (reduce (fn [b-seq item] (conj (conj b-seq x) item))
              [(first a-seq)]
              (rest a-seq))))

(defn my-count [a-seq]
    (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [b-seq e] (cons e b-seq)) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [first-item (first a-seq)
          min-max (fn [[a-min a-max] e]
                    (cond
                      (> a-min e) [e a-max]
                      (< a-max e) [a-min e]
                      :else [a-min a-max]))]
      (reduce min-max [first-item first-item] (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [acc []
         a-seq sorted-seq]
    (let [first-item (first a-seq)]
      (cond
        (empty? a-seq) (conj acc n)
        (< n first-item) (concat (conj acc n) a-seq)
        :else (recur (conj acc first-item) (rest a-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [c e] (inc c)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce (fn [product e] (* product e)) (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
    (reduce pred-and (pred-and pred1 pred2) more)))

(defn zip [seq1 seq2]
    (if (or (empty? seq1) (empty? seq2))
        '()
        (cons (vector (first seq1) (first seq2))
              (zip (rest seq1) (rest seq2)))))

(defn zip-n-conj [seq1 seq2]
    (if (or (empty? seq1) (empty? seq2))
        '()
        (cons (conj (first seq1) (first seq2))
              (zip-n-conj (rest seq1) (rest seq2)))))

(defn zip-many [seq1 seq2 & more]
  (reduce zip-n-conj (zip seq1 seq2) more))

(defn my-map
  ([func a-seq] (reduce (fn [b-seq e] (conj b-seq (func e))) [] a-seq))
  ([func seq1 seq2]
    (reduce (fn [b-seq e] (conj b-seq (apply func e))) [] (zip seq1 seq2)))
  ([func seq1 seq2 & more]
    (reduce (fn [b-seq e] (conj b-seq (apply func e))) []
            (apply zip-many seq1 seq2 more))))
