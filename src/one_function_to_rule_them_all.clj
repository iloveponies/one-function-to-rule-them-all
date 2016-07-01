(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
   (rest (reverse (reduce (fn [result y] (conj (conj result x) y)) '() a-seq)))))

(defn my-count [a-seq]
    (reduce (fn [result y] (inc result)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [min-value (reduce min a-seq)
        max-value (reduce max a-seq)]
      [min-value max-value])))

(defn insert [sorted-seq n]
  (loop [result '()
         a-seq sorted-seq]
    (if (empty? a-seq)
      (reverse (conj result n))
      (if (> n (first a-seq))
        (recur (conj result (first a-seq)) (rest a-seq))
        (reverse (apply conj (conj result n) a-seq))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (reduce (fn [result x] (inc result)) 1 more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [elem] (x elem)))
  ([x & more] (fn [elem] (reduce (fn [result pred] (and result (pred elem))) (x elem) more))))

(defn my-map 
	([f a-seq] (reverse (reduce (fn[result x] (conj result (f x))) '() a-seq)))
	([f a-seq & more] 
    (let [inputs (cons a-seq more)]
      (if (some empty? inputs)
        '()
        (cons (apply f (my-map first inputs)) (apply my-map f (my-map rest inputs)))))))