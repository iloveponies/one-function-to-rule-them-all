(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat (list) a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc elem] (str acc " " elem)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) (list)
   (empty? (rest a-seq)) (seq a-seq)
   :else (seq (reduce (fn [acc elem] (conj acc x elem)) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [add (fn [count e] (inc count))]
    (reduce add 0 a-seq)))

(defn my-reverse [a-seq]
  (let [push (fn [reversed elem] (cons elem reversed))]
    (reduce push (list) a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [v elem]
                  (let [[curr-min curr-max] v]
                    (vector (min curr-min elem) (max curr-max elem))))]
    (let [fst (first a-seq)]
      (reduce min-max (vector fst fst) (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [head (vector)
         tail sorted-seq]
    (if (empty? tail)
      (conj head n)
      (let [fst (first tail)]
        (if (<= n fst)
          (concat (conj head n) tail)
          (recur (conj head fst) (rest tail)))))))

(defn insertion-sort [a-seq]
  (reduce insert (vector (first a-seq)) (rest a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
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
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq & more]
  nil)
