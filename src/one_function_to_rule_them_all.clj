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
;E3
(defn my-interpose [a a-seq]
  (if (empty? a-seq)
    []
    (let [helper (fn [acc
                      elem]
                   (concat (concat acc [a]) [elem]))]
     (reduce helper [(first a-seq)] (rest a-seq)))))


;E4
(defn my-count [a-seq]
  (let [counter (fn [n elem]
                  (inc n))]
    (reduce counter 0 a-seq)))

;E5
(defn my-reverse [a-seq]
  (let [reverser (fn [a b]
                   (concat [b] a))]
    (reduce reverser [] a-seq)))


;E6
(defn min-max-element [a-seq]
  (let [helper (fn [v elem]
                 (cond (< elem (get v 0)) (assoc v 0 elem)
                       (> elem (get v 1)) (assoc v 1 elem)
                       :else v))]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))



;E7
(defn insert [sorted-seq n]
  (loop [acc []
         r sorted-seq]
    (cond (empty? r) (conj acc n)
          (< n (first r)) (concat (conj acc n) r)
          :else (recur (conj acc (first r)) (rest r)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

;E8
(defn parity [a-seq]
  (let [toggle (fn [a-set
                    elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (reduce toggle #{} a-seq)))

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
(defn my-map

  ([f a-seq]
  (let [helper (fn [mmap
                    elem]
                 (concat mmap [(f elem)]))]
   (reduce helper [] a-seq)))
  ([f a-seq b-seq]
   (if (and (seq a-seq) (seq b-seq))
     (cons (f (first a-seq) (first b-seq))
           (my-map f (rest a-seq) (rest b-seq)))
     []))
  ([f a-seq b-seq c-seq]
   (if (and (seq a-seq) (seq b-seq) (seq c-seq))
     (cons (f (first a-seq) (first b-seq) (first c-seq))
           (my-map f (rest a-seq) (rest b-seq) (rest c-seq)))
     []))
  ([f a b c & more]
   ;(map-okay f (concat [a b c] more)) ;testit ei diggaa?
   ))

(defn map-okay
  "Apufunktio mymapille"
  [f a]
  (let [helper (fn [fun]
                 (fn [acc
                      elem]
                   (concat acc [(fun elem)])))
        firsts (reduce (helper first) [] a)
        rests (reduce (helper rest) [] a)]
    (if (some empty? a)
      []
      (cons (apply f firsts)
      (map-okay f rests))
    )))




