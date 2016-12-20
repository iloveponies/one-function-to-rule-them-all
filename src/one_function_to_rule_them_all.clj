(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq ) ""
   (reduce (fn [a b](str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) []
  (reduce (fn [a b](conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (+ a 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b](cons b a)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [a b](
     list (cond
        (nil? (first a)) b
        (< (first a) b) (first a)
        (< b (first a)) b :else b)
      (cond
        (nil? (second a)) b
        (> (second a) b) (second a)
        (> b (second a)) b :else b))
      ) [nil nil] a-seq))

(defn insert [sorted-seq n]
  (let [helper (fn [lista alkup ins]
     (cond (empty? alkup) (conj lista ins)
      (and (not (.contains lista ins)) (< ins (first alkup))) (concat lista (cons ins alkup))
      :else (recur (conj lista (first alkup)) (rest alkup) ins)))] (helper [] sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) [] a-seq))

(defn lisaa-frekvenssi [frekvenssisetti item]
  (if (= (get frekvenssisetti item) nil) (assoc frekvenssisetti item 1) (assoc frekvenssisetti item (+ 1 (get frekvenssisetti item)))) )


((defn parity [a-seq] (loop [lista a-seq frekvenssisetti {}]
    (cond
      (empty? lista) (into #{} (keys (filter #(odd? (mod (second %) 2)) frekvenssisetti)))
      :else (recur (rest lista) (lisaa-frekvenssi frekvenssisetti (first lista)) ) ))) [1 1 2])

(defn minus  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
 ([x] 1)                         ; one parameter
  ([x y] 2)        ; two parameters
  ([x y & more]                   ; more than two parameters
    (reduce (fn [a b] (+ 1 a)) 2 more)))



(defn my-*
  ([] 1)
 ([x] x)                         ; one parameter
  ([x y] (* x y))        ; two parameters
  ([x y & more]                   ; more than two parameters
    (reduce * (* x y) more) ))

(defn pred-and
  ([] (fn [x] true ))
 ([pred1] (fn [x] (pred1 x)))                         ; one parameter
  ([pred1 pred2] (fn [x] (if (and (pred1 x) (pred2 x)) true false)))        ; two parameters
  ([pred1 pred2 & more]                   ; more than two parameters
    (fn [x] (reduce (fn[a b](and a (b x))) (and (pred1 x) (pred2 x)) more)) ))

(defn my-map
  ([f a-seq] (reduce (fn [a b] (conj a (f b))) [] a-seq) )
  ([f a-seq & more] (reduce (fn [x y] (reduce (fn [a b] (conj a (f b))) [] y))
                (reduce (fn [a b] (conj a (f b))) [] a-seq) more)))
