(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (rest (reduce #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [a b] (inc a))]
  (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [a b] (cons b a))]
    (reduce reverser () a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [[a b] elem]
                 [(min a elem)
                  (max b elem)])]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [acc []
         a-seq sorted-seq
         ok false]
    (cond
     (and (empty? a-seq) ok) (seq acc)
     (empty? a-seq) (seq (conj acc n))
     (and (> (first a-seq) n) (not ok)) (recur (conj acc n (first a-seq)) (rest a-seq) true)
     :else (recur (conj acc (first a-seq)) (rest a-seq) ok))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x]
   (if (< 0 x)
     (- x)
     x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([a b] (fn [x] (and (a x) (b x))))
  ([a b & more] (reduce pred-and (pred-and a b) more)))

(defn my-map
  ([f col] (for [item col]
             (f item)))
  ([f col & more]
   (let [min-size (apply min (my-map count more))]
     (for [index (range min-size)]
       (apply f
              (my-map #(nth % index)
                  (conj more col)))))))
