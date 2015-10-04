(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (clojure.string/join " " a-seq))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest (mapcat #(vector x %) a-seq))))

(defn my-count [a-seq]
  (let [f (fn [a b]
            (inc a))]
  (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce #(vector (min (first %1) %2)
                      (max (second %1) %2)) 
             [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (list n)
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))
    ))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) [] a-seq))

(defn parity [a-seq]
  (set (keys (filter #(odd? (second %)) (frequencies a-seq)))))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& args]
  (let [c (count args)]
    (cond 
      (= c 0) 1
      (= c 1) (first args)
      (= c 2) (* (first args) (second args))
      :else (reduce * args))))

(defn pred-and [& args]
  (let [c (count args)]
    (cond
      (= c 0) (fn [x] x)
      (= c 1) (first args)
      (= c 2) (fn [x]
                (let [f1 (first args)
                      f2 (second args)]
                  (and (f1 x) (f2 x))))
      :else (fn [x]
              (every? true? (map #(%1 %2) args (repeat x)))))))

(defn map-help [method args]
  (reduce #(conj %1 (method %2)) [] args))

(defn my-map [f & args]
  (if (some empty? args)
    ()
    (cons (apply f (map-help first args)) (apply my-map f (map-help rest args)))))
