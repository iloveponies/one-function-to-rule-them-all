(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (loop [acc []
         sequence a-seq]
    (if (empty? sequence)
      acc
      (recur
        (reduce conj acc (first sequence))
        (rest sequence)))))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [vector element] (conj vector x element)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [[min max] element]
                  [(if (< element min) element min)
                   (if (> element max) element max)])]
    (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [acc []
         sequence sorted-seq]
    (if (empty? sequence)
      (conj acc n)
      (if (< n (first sequence))
        (conj acc n (first sequence))))))

(defn my-concat [s1 s2]
  (if (empty? s2)
    s1
    (recur (conj s1 (first s2)) (rest s2))))

(defn insert [sorted-seq n]
  (loop [new-seq []
         sequence sorted-seq]
    (cond 
      (empty? sequence) (conj new-seq n)
      (> n (first sequence)) (recur (conj new-seq (first sequence)) (rest sequence))
      :else (my-concat (conj new-seq n) sequence))))

(defn insertion-sort [a-seq]
 (reduce insert [] a-seq)) 

(defn parity [a-seq]
 (let [toggle (fn [a-set element]
                 (let [operation (if (contains? a-set element) disj conj)]
                    (operation a-set element)))]
   (reduce toggle #{} a-seq))) 

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (my-count params))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([predicate] (fn [element] (predicate element)))
  ([p1 p2] (fn [element] (and (p1 element) (p2 element))))
  ([p1 p2 & predicates] (reduce pred-and (pred-and p1 p2) predicates)))

(defn my-map
  ([f & sequences]
   (loop [acc []
          sequences sequences]
     (if (empty? (first sequences))
       acc
       (let [firsts (reduce (fn [coll sequence]
                              (conj coll (first sequence))) [] sequences)
             rests (reduce (fn [coll sequence]
                             (conj coll (rest sequence))) [] sequences)]
         (recur (conj acc (apply f firsts)) rests))))))