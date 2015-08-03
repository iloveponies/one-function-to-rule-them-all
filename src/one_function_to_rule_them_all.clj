(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))
;5

(defn str-cat [a-seq]
  (if (empty? a-seq)
      ""
      (reduce (fn[x y] (str x " " y)) a-seq)))
;9

(defn my-interpose [x a-seq]
  (drop 1 (reduce (fn[z w] (conj z x w)) [] a-seq)))
;14

(defn my-count [a-seq]
  (let [counter (fn [countr e]
                  (inc countr))]
    (reduce counter 0 a-seq)))
;17

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) [] a-seq))
;22

(defn min-max-element [a-seq]
  (let [indexer (fn [lst e]
                  [(min (first lst) e)
                    (max (second lst) e)])]
    (reduce indexer [(first a-seq) (first a-seq)] a-seq)))
;25

(defn insert [sorted-seq n]
  (loop [i 0
         a-seq []]
    (if (== (count sorted-seq) i)
        (conj sorted-seq n)
        (let [elem (get sorted-seq i)]
          (if (< n elem)
            (vec
              (concat
                (conj a-seq n)
                (drop i sorted-seq)))
            (recur (inc i) (conj a-seq elem)))))))
;29

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))
;32

(defn toggle [a-set elem]
   (if (contains? a-set elem)
       (disj a-set elem)
       (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn[a-set x] (toggle a-set x)) (set []) a-seq))
;35

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))
;37

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (+ 2 (count more))))
;40

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))
;43

;(defn pred-and [pred1 pred2]
;   (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-and
  ([] (fn[x] true))
  ([pred1] (fn[x] (pred1 x)))
  ([pred1 pred2] (fn[x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (fn[x] (reduce (fn[preda predb] (and preda (predb x))) (and (pred1 x) (pred2 x)) more))))
;46

(defn my-map
  ([f a-seq] (reduce (fn[b-seq x] (conj b-seq (f x))) [] a-seq))
  ([f a-seq & more]
    (loop [i 0
           final-seq []]
      (if (== (count a-seq) i)
        final-seq
      ; need to call f on the ith element of every sequence
      (let [nextVal (reduce (fn[b-seq x] (conj b-seq (get x i))) [(get a-seq i)] more)]
       (recur (inc i) (conj final-seq (apply f nextVal))))))))
;50
