(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (let [concat-helper (fn [new-seq left-seq]
                        (if (empty? left-seq)
                          new-seq
                          (recur (concat new-seq (first left-seq)) (rest left-seq))))]
    (concat-helper [] a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (drop 1 (reduce (fn [v y] (conj v x y)) [] a-seq))))


(defn my-count [a-seq]
  (let [counter (fn [cnt e] (inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [make-first (fn [a-seq elem]
                     (cons elem a-seq))]
    (reduce make-first [] a-seq)))


(defn min-max-element [a-seq]
  (let [m-m (fn [[min-x max-x] x]
              (cond (> x max-x) [min-x x]
                    (< x min-x) [x max-x]
                    :else [min-x max-x]))]
    (reduce m-m [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [front-seq []
         tail-seq sorted-seq]
    (cond (empty? tail-seq) (conj front-seq n)
          (< (first tail-seq) n) (recur (conj front-seq (first tail-seq)) (rest tail-seq))
          :else (apply conj front-seq n tail-seq))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [some-set elem]
                 (if (contains? some-set elem)
                   (disj some-set elem)
                   (conj some-set elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [x y] (inc x)) 0 params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred & more] (let [two-pred-and (fn [pred1 pred2]
                                      (fn [x] (and (pred1 x) (pred2 x))))]
                   (reduce two-pred-and pred more))))

(defn my-map [f a-seq]
  [:-])
