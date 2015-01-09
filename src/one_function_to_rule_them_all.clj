(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [b-seq y] (conj b-seq x y)) 
            [(first a-seq)] 
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [i b-seq] (inc i)) 0 a-seq))


(defn my-reverse [a-seq]
  (reduce (fn [have i] (conj have i)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [nil nil]
    (reduce (fn [[mi ma] i] [(min i mi) (max i ma)]) 
            [(first a-seq) (first a-seq)] 
            a-seq)))

(defn insert [sorted-seq n]
  (loop [have []
         left sorted-seq]
    (cond (empty? left) (conj have n)
          (<= n (first left)) (concat have (cons n left))
          :else (recur (conj have (first left)) (rest left))))) 

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [have i] (insert have i)) [(first a-seq)] (rest a-seq))))

(defn parity [a-seq]
  (let [toggle (fn [a-set x] (if (a-set x) (disj a-set x) (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more] (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (reduce (fn [p q] (fn [x] (and (p x) (q x)))) p more)))

(defn szip [as bs]
  (loop [sa-seq as
         b-seq bs
         have []]
    (if (or (empty? sa-seq) (empty? b-seq))
      have
      (recur (rest sa-seq) 
             (rest b-seq) 
             (conj have (conj (first sa-seq) (first b-seq)))))))

(defn encaps [as]
  (reduce (fn [have i] (conj have [i])) [] as))

(defn zip [a-seq & more]
  (reduce szip (encaps a-seq) more))

(defn my-map [f & more]
  (let [args (apply zip more)]
    (reduce (fn [have x] (conj have (apply f x)))
            []
            args)))
