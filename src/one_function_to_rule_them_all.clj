(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [add-space
        (fn [initial x]
          (str initial " " x))]
  (cond
   (empty? a-seq) ""
   :else (reduce add-space a-seq))))

(defn my-interpose [x a-seq]
  (let [char-between
        (fn [initial y]
          (if (empty? initial)
            (conj initial y)
            (conj initial x y)))]
  (reverse (reduce char-between () a-seq))))

(defn my-count [a-seq]
  (let [counter
        (fn [count x]
          (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-seq
        (fn [initial x]
          (conj initial x))]
    (reduce reverse-seq () a-seq)))

(defn min-max-element [a-seq]
  (let [check-min-max
        (fn [initial x]
          (cond
           (nil? (get initial 0)) (assoc initial 0 x)
           (nil? (get initial 1)) (assoc initial 1 x)
           (> x (get initial 1)) (assoc initial 1 x)
           (< x (get initial 0)) (assoc initial 0 x)
           :else initial))]
    (cond
     (= (count a-seq) 1) (conj a-seq (first a-seq))
     :else (reduce check-min-max [] a-seq))))

(defn insert [sorted-seq n]
  (loop [sorted sorted-seq
         first-part []]
    (cond
     (empty? sorted) (conj first-part n)
     (< n (first sorted)) (concat (conj first-part n) sorted)
     :else (recur (rest sorted) (conj first-part (first sorted))))))

(defn insertion-sort [a-seq]
  (let [sort-each
        (fn [initial each]
          (if (empty? initial)
            (conj initial each)
            (insert initial each)))]
    (reduce sort-each [] a-seq)))

(defn parity [a-seq]
  (let [get-odd-elems
        (fn [odds each]
          (cond
           (odd? (count (filter
                         (fn [x]
                           (= x each)) a-seq))) (conj odds each)
           :else odds))]
    (reduce get-odd-elems #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] x))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

; Not done:
(defn my-map
  ([f a-seq1] f a-seq1)
  ([f a-seq1 a-seq2] )
  ([f a-seq1 a-seq2 & more] (conj a-seq1 a-seq2 more)))
