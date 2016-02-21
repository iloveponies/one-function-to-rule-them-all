(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
 (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    "" 
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [prev cur] (conj prev x cur)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [count-elems
        (fn [count _ ]
          (inc count))]
    (reduce count-elems 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [one two]
                   (cons two one))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    [(apply min a-seq) (apply max a-seq)]))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [front [] back sorted-seq]
      (cond 
            (empty? back)
            (conj front n)
            (< n (first back))
            (concat front (cons n back))
            :else
            (recur (conj front (first back)) (rest back))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle
  "accepts a set and an element and returns a set with the element's membership toggled"
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (my-count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [& any] true))
  ([p] p)
  ([p & all] (fn [x]
             (loop [cur-pred p preds-remaining all]
               (cond (not  (cur-pred x))
                     false
                     (empty? preds-remaining)
                     true
                     :else
                     (recur (first preds-remaining) (rest preds-remaining)))))))

(defn my-map
  ([f a-seq] (loop [sq [] remaining a-seq]
               (if empty? remaining)
               sq
               (recur (cons (f (first remaining)) sq)
                      (rest remaining)))))

