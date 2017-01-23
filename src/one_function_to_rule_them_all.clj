(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [z a] (str z " " a)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [z a] (concat z [x a])) [(first a-seq)] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [z a] (cons a z)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[mn mx] a] 
            [(min mn a) (max mx a)])
    [(first a-seq) (first a-seq)] (rest a-seq))) 

(defn insert [sorted-seq n]
  (loop [acc []
         sq sorted-seq]
    (cond
      (empty? sq) (reverse (cons n acc))
      (>= (first sq) n) (concat (reverse acc) (cons n sq))
      :else (recur (cons (first sq) acc) (rest sq)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [set a]
            (if (contains? set a)
              (disj set a)
              (conj set a))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))
  

(defn count-params [& args]
  (count args))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & rest] (reduce my-* (my-* x y) rest)))

(defn pred-and
  ([] (fn [_] true))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & rest] (reduce pred-and (pred-and x y) rest)))

(defn my-map
  ([f coll] (reverse (reduce (fn [acc y] (cons (f y) acc)) '() coll)))
  ([f coll & arg-seq]
    (loop [args []
           sq (cons coll arg-seq)]
      (if (some empty? sq)
        (reverse (my-map (fn [as] (apply f as)) args))
        (recur (cons (my-map first sq) args) (my-map rest sq))))))
