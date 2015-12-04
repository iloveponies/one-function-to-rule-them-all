(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [elem1 elem2]
              (if (not (empty? elem1))
                (conj (conj elem1 x) elem2)
                (conj elem1 elem2)))
            []
            a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse (fn [e1 e2]
                  (conj e1 e2))]
    (reduce reverse '() a-seq)))

(defn min-max-element [a-seq]
  (let [checker (fn [min-max el]
                  (cond
                   (or (nil? min-max)
                       (and (< el (first min-max)) (> el (second min-max))))
                     [el el]
                   (< el (first min-max))
                     [el (second min-max)]
                   (> el (second min-max))
                     [(first min-max) el]
                   :else
                     min-max))]
    (reduce checker nil a-seq)))

(defn insert [sorted-seq n]
  (loop [sorted  []
         sekv    sorted-seq
         visitor n]
    (cond
     (and (empty? sekv)
          (nil? visitor))
       (seq sorted)
     (or (empty? sekv)
         (and (not (nil? visitor))
              (< visitor (first sekv))))
       (recur
        (conj sorted visitor)
        sekv
        nil)
     :else
       (recur
        (conj sorted (first sekv))
        (rest sekv)
        visitor))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [odds el]
                 (if (contains? odds el)
                   (disj odds el)
                   (conj odds el)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (let [counter (fn [cnt el]
                  (inc cnt))]
    (reduce counter 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred?] (fn [x] (pred? x)))
  ([pred1? pred2?] (fn [x] (and (pred1? x) (pred2? x))))
  ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))

(defn my-map [f a-seq]
  [:-])
