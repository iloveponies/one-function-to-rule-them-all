(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [spaced-seq (interpose " " a-seq)]
    (reduce str spaced-seq)))

(defn my-interpose [x a-seq]
  (let [x-seq (interpose x a-seq)]
    (reduce conj () (reverse x-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (if (= elem nil)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [rev-seq elem]
                   (if (= elem nil)
                     rev-seq
                     (cons elem rev-seq)))]
    (reduce reverser () a-seq)))

(defn min-max-element [a-seq]
  (let [counter (fn [min-max elem]
                  (if (= elem nil)
                    min-max
                    (vector (min elem (first min-max)) (max elem (get min-max 1)))))]
    (reduce counter [(first a-seq) 0] a-seq)))

(defn insert [sorted-seq n]
  (loop [sequ ()
         rest-sorted sorted-seq]
    (cond
     (empty? rest-sorted) (reverse (cons n sequ))
     (< n (first rest-sorted)) (concat (reverse sequ) (cons n rest-sorted))
     :else (recur (cons (first rest-sorted) sequ) (rest rest-sorted)))))


(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [picker (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce picker #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (let [helper (fn [truthines pred]
                                 (if truthines
                                 (pred x)
                                  false))]
                            (reduce helper (and (p1 x) (p2 x)) more)))))

(defn my-map [f a-seq]
  [:-])
