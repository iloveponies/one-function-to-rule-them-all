(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (drop-last (reverse (reduce (fn [a b] (conj (conj a b) x)) '() a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [i e]
                  (inc i))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [current e]
                 (cons e current))]
    (reduce helper '() a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [v-minmax current-num]
                 (cond
                  (empty? v-minmax) (conj v-minmax current-num current-num)
                  (< current-num (first v-minmax)) (assoc v-minmax 0 current-num)
                  (> current-num (last v-minmax)) (assoc v-minmax 1 current-num)
                  :else v-minmax))] 
    (reduce helper [] a-seq)))

(defn insert [sorted-seq n]
  (concat
    (take-while (fn [x] (< x n)) sorted-seq)
    (cons n (drop-while (fn [x] (< x n)) sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [s x]
                 (if (contains? s x)
                  (disj s x)
                  (conj s x)))]
  (reduce helper #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] 
    (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [e] true))
  ([x] (fn [e] (x e)))
  ([x y] (fn [e] (and (x e) (y e))))
  ([x y & more] 
    (reduce pred-and (pred-and x y) more)))

(defn my-map [f & more]
  (let [get-firsts (fn [s] (reduce (fn [acc x] (conj acc (first x))) [] s))
        get-rest (fn [s] (reduce (fn [acc x] (conj acc (rest x))) [] s))]
    (loop [acc '()
           s (get-firsts more)
           r (get-rest more)]
      (if (or (some nil? s) (empty? s))
        (reverse acc)
        (recur (conj acc (apply f s)) (get-firsts r) (get-rest r))))))

