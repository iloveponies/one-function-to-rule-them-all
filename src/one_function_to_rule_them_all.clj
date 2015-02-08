(ns one-function-to-rule-them-all)

;E1
(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

;E2
(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y))
            a-seq)))
;E3 WIP!
(defn my-interpose [a a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [x y] (concat (conj (seq [a]) x) y))
            a-seq)))
;(my-interpose [0] [])
;(seq [1])
;(conj (seq [1]) 0)

;E4
(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

;E5 WIP iseq fro long hv error
(defn my-reverse [a-seq]
  (let [reverser (fn [a b]
                   (concat a [(first b)]))]
    (reduce reverser [] a-seq)))
;(my-reverse [1 2])
; (concat  (rest [1 2 3]) [(first [1 2 3])])

;E6
(defn min-max-element [a-seq]
  (let [helper (fn [v elem]
                 (cond (< elem (get v 0)) (assoc v 0 elem)
                       (> elem (get v 1)) (assoc v 1 elem)
                       :else v))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))


; (conj [1 2] 1)

;E7
(defn insert [sorted-seq n]
  (loop [acc []
         r sorted-seq]
    (cond (empty? r) (conj acc n)
          (< n (first r)) (concat (conj acc n) r)
          :else (recur (conj acc (first r)) (rest r)))))
;(insert [1 3 4] 5)
(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

;E8 WIP iseq bs
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (cons a-set elem)
    (conj a-set elem)))
(defn parity [a-seq]
  (reduce toggle #{} a-seq))


;E9
(defn minus
  ([x] (- x))
  ([x y] (- x y))
  )

;E10
(defn count-params [& more] (count more))

;E11
(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

;E12
(defn pred-and
  ([] (fn [v] true))
  ([x] (fn [v] (x v))) ;totuusarvo?
  ([x y] (fn [v] (and (x v) (y v))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more))
    )

;E13 wip
(defn ekat
  ([] ())
  ([x] [(first x)])
  ([x y] [(first x) (first y)])
  ([x y & more]
   (reduce ekat more))
    )

(ekat [3] [5 2 3] [12 2 4])

(defn my-map [f a-seq]
  [:-])
