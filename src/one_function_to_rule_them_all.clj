  (ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
  (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (conj a b)) (list) a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [minMax el]
                    (cond
                     (< el (first minMax)) [el (second minMax)]
                     (> el (second minMax)) [(first minMax) el]
                     :else
                     minMax))]
  (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [acc []
         n n
         s sorted-seq]
         (cond
          (empty? s)
          (conj acc n)
          (< n (first s)) (concat (conj acc n) s)
          :else
          (recur (conj acc (first s)) n (rest s)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc el]
    (if (contains? acc el)
      (disj acc el)
      (conj acc el)))]
  (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([& more]
    (let [plus1 (fn [acc el]
      (if (not(nil? el))
      (inc acc)
      acc))]
    (reduce plus1 0 more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] 
    (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f & a-seq]
   (cond
    (some empty? a-seq)
      '()
    :else
      (cons (apply f (map first a-seq))
            (apply (partial my-map f) (map rest a-seq)))))