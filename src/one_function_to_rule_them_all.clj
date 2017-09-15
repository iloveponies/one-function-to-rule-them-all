(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (drop 1 (reduce
              (fn [acc elem] (conj acc x elem))
              []
              a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (cons elem acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] elem] [(min mn elem) (max mx elem)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (loop [acc []
         remaining sorted-seq]
    (cond
      (empty? remaining)
        (conj acc n)
      (< n (first remaining))
        (concat acc (list n) remaining)
      :else
        (recur (conj acc (first remaining)) (rest remaining)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [counts elem]
            (if (contains? counts elem)
              (disj counts elem)
              (conj counts elem)))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  [& args] (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & zs] (reduce * (concat [x y] zs))))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & pn]
    (fn [x]
      (reduce
        (fn [bool p] (and bool (p x)))
        (concat [p1 p2] pn)))))

(defn my-map
  ([f a-seq]
    (reduce (fn [processed elem]
              (conj processed (f elem)))
            []
            a-seq))
  ([f first-seq & other-seqs]
    (let [seqs (concat [first-seq] other-seqs)]
      (reduce (fn [processed index]
                (conj processed (apply f (my-map (fn [x] (nth x index)) seqs))))
              []
              (range (apply min (my-map count seqs)))))))
