(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reverse (reduce #(cons %2 (cons x %1)) '() a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [minmax (fn [x y]
                 [(min (first x) y)
                  (max (second x) y)])]
    (vec (reduce minmax (repeat 2 (first a-seq)) (rest a-seq)))))

(defn insert [sorted-seq n]
  (loop [lst sorted-seq
         new '()]
    (cond
     (empty? lst) (concat (reverse new) (cons n nil))
     (< n (first lst)) (concat (reverse new) (cons n lst))
     :else (recur (rest lst) (cons (first lst) new)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2)
             (disj %1 %2)
             (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & ys] (reduce * (* x y) ys)))

(defn pred-and
  ([] (fn [x] true))
  ([fx] fx)
  ([fx fy] (fn [x] (and (fx x) (fy x))))
  ([fx fy & fns]
   (fn [x]
     (let [allfns (cons fx (cons fy fns))]
       (loop [fns allfns]
         (cond
          (empty? fns) true
          ((first fns) x) (recur (rest fns))
          :else false))))))

(defn my-map
  ([f & seqs] (loop [result '()
                     lsts seqs]
               (cond
                (some empty? lsts) (reverse result)
                :else (recur (cons (apply f (map first lsts)) result)
                             (map rest lsts))))))
