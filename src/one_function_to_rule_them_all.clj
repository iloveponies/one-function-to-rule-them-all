(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
     ""
     (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
      ()
  (let [combiner (fn [tot e]
                (conj tot x e))]
  (reduce combiner [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
 (let [counter (fn [count e]
                  (if e
                    (inc count)
                    count))]
     (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [a seq]
                    (cons seq a))]
  (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
      nil
      (let [f-elem (first a-seq)
            min-max (fn [[min max] elem-seq]
                        [(if (< elem-seq min) elem-seq min) (if (> elem-seq max) elem-seq max)] )]
      (reduce min-max [f-elem f-elem] a-seq))))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
         (if (or (empty? tail) (<= n (first tail)))
            (concat (conj head n)  tail)
            (recur (conj head (first tail)) (rest tail) ) )))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [check (fn [new-set elem]
                  (if (contains? new-set elem)
                       (disj new-set elem)
                       (conj new-set elem)) )]
  (reduce check #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& values]
  (if (empty? values)
     0
     (count values)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1, p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1, p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])