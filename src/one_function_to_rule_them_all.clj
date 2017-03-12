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
    (if (> (count a-seq) 1)
      (reduce (fn [z y] (if (and (vector? z) (> (count z) 1))
                      (conj z x y)
                      (conj [z] x y)))
             a-seq)
      a-seq)))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [x y] (inc x)) a-seq)))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [x y]
              (if (seq? x)
                (concat [y] x)
                (concat [y] [x])))
            a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (loop [seq1 sorted-seq
         counter 0]
    (if (empty? seq1)
      (conj sorted-seq n)
      (if (< n (first seq1))
        (vec (concat (take counter sorted-seq) [n] (drop counter sorted-seq)))
        (recur (rest seq1) (inc counter))))))

(defn insertion-sort [a-seq]
  (reduce (fn [x y] (if (vector? x)
                      (insert (vec x) y)
                      (insert [x] y)))
          a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (fn [x] (reduce (fn [z y] (and z (y x))) (and (pred1 x) (pred2 x)) more))))

(defn my-map
  ([f a-seq] (reduce (fn [x y] (concat x [(f y)])) [(f (first a-seq))] (rest a-seq)))
  ([f a-seq & more] (let [inner (fn [inner-seq inner-counter]
                                  (loop [seq1 inner-seq
                                         result []]
                                    (if (empty? seq1)
                                      result
                                      (recur (rest seq1) (conj result (nth (first seq1) inner-counter))))))
                          outer (fn [func outer-seq]
                                  (loop [counter 0
                                         result []]
                                    (if (== counter (count outer-seq))
                                      result
                                      (recur (inc counter) (conj result (apply f (inner outer-seq counter)))))))]
                      (outer f (concat [a-seq] more)))))

