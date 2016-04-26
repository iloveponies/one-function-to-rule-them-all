(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce (fn [acc e] (concat e acc)) '() (reverse a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [acc s] (str acc " " s)) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
    (reverse (reduce (fn [acc e] (list* e x acc)) (list (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mini maxi] n] [(min mini n) (max maxi n)]) 
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [left '(), right sorted-seq]
    ;;; (println left right)
    (cond (empty? right) (reverse (cons n left))
          (> n (first right)) (recur (cons (first right) left) (rest right))
          :else (concat (reverse left) (cons n right)))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted n] (insert sorted n)) '() a-seq))

(defn parity [a-seq]
  (reduce (fn [acc e] (if (contains? acc e) (disj acc e) (conj acc e))) #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (count more))

(defn my-* 
  ([] 1)
  ([x & more] (reduce * x more)))

(defn pred-and 
  ([] (fn [_] true))
  ([pred & more]
   (fn [x]
     (loop [res (pred x), others more]
       (cond 
         (not res) false
         (empty? others) true
         :else (recur ((first others) x) (rest others)))))))

(defn my-map [f a-seq]
  [:-])
