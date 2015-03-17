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
    (reduce (fn [acc y] (conj acc x y)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc a]
            [(min (first acc) a) (max (second acc) a)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [a []
         b sorted-seq]
    (cond (empty? b) (conj a n)
          (<= n (first b)) (concat a [n] b)
          :else (recur (conj a (first b)) (rest b)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (reduce (fn [acc x] (+ acc 1)) 0 xs))

(defn my-* [& xs]
  (reduce * 1 xs)) 

(defn pred-and [& ps]
  (fn [x] (reduce (fn [acc p] (and acc (p x))) true ps)))

(defn my-map
  ([f a-seq]
   (if (empty? a-seq)
     '()
     (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f x & a-seq]
   (let [s (cons x a-seq)]
         (if (some empty? s)
           '()
           (cons (apply f (my-map first s))
                 (apply my-map f (my-map rest s)))))))
  
