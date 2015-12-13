(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq)
)

(defn str-cat [a-seq]
  (let [insert-space (fn [s1 s2] (str s1 " " s2))]
    (if (empty? a-seq) ""
      (reduce insert-space a-seq)
    ))
)

(defn my-interpose [x a-seq]
  (let [insert-x (fn [e1 e2] (conj e1 x e2))]
    (if (empty? a-seq) []
      (reduce insert-x [(first a-seq)] (rest a-seq))))

)

(defn my-count [a-seq]
  (let [helper (fn [cnt e] (inc cnt))]
    (reduce helper 0 a-seq))
)

; musi tam byt [rest]-> odebiraji se to po prvku, ne cely zbytek
(defn my-reverse [a-seq]
  (let [helper (fn [actual rest] (concat [rest] actual))]
    (reduce helper [] a-seq))
)

; muzeme ukladat jen lokalne, protoze ono to vzdycky vrati
; a tak udrzuje tu max/min nalezenou hodnotu
(defn min-max-element [a-seq]
  (let [helper (fn [[min max] elem]
                 (if (< elem min) [elem max]
                   (if (> elem max) [min elem]
                   [min max])))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq))
)

; we defined these functions as my-take in the previous ..
(defn insert [sorted-seq n]
  (let [smaller (fn [x] (< x n))]
    (concat
     (take-while smaller sorted-seq)
     [n]
     (drop-while smaller sorted-seq))
))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)
)

(defn parity [a-seq]
  (let [helper (fn [a b] (if (contains? a b) (disj a b) (conj a b)))]
    (reduce helper #{} a-seq))
)

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y))
)

(defn count-params [& more]
  (let [helper (fn [cnt e] (inc cnt))]
   (reduce helper 0 more))
;;   (str more)
  )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more))
)

(defn pred-and
  ([] (fn [seq] seq))
  ([x] (fn [seq] (x seq)))
  ([x y] (fn [seq] (and (x seq) (y seq))))
  ([x y & more] (reduce pred-and (pred-and x y) more))
  )

(defn my-map
  ([f a-seq]
    (let [helper (fn [results elem]
                 (concat results [(f elem)])
;;                  (cons (f elem) results)
;;                  (apply f elem)
                 )]
      (reduce helper [] a-seq)))
  ([f a-seq & more]
   (let [helper (fn [sub elem]
                  (concat sub [(apply f elem)])
;;                   (reduce my-map [] elem)
                  )]
   (vec (reduce helper [(apply f a-seq)] more))))
;;    (reduce helper (my-map f a-seq) more)))
)
