(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [eka toka] (str eka " " toka))]
    (if
      (empty? a-seq)
      ""
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (let [conjurer (fn [vect n] (conj vect x n))]
    (if
      (empty? a-seq)
      '()
      (reduce conjurer [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [h (fn [acc x] (inc acc))]
    (reduce h 0 a-seq)))

(defn my-reverse [a-seq]
  (let [h (fn [b-seq x] (cons x b-seq))]
    (reduce h '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [m-m n]
                  (cond
                   (< n (get m-m 0)) (assoc m-m 0 n)
                   (> n (get m-m 1)) (assoc m-m 1 n)
                   :else m-m))]
    (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (lazy-seq (loop [start []
         rests sorted-seq
         add n]
    (cond
     (empty? rests) (conj start add)
     (< add (first rests)) (concat (conj start add) rests)
     :else (recur (conj start (first rests)) (rest rests) add))) ))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [par (fn [a-set x]
              (if
                (contains? a-set x)
                (disj a-set x)
                (conj a-set x)))]
    (reduce par #{} a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [a] (and (x a)(y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map [f a-seq]
  [:-])
