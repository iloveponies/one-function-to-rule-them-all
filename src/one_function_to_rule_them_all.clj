(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [concat-with-space (fn [a b] (str a " " b))]

    (if (empty? a-seq)
      ""
      (reduce concat-with-space a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [interpose-pair (fn [a b] (conj a x b))]
      (reduce interpose-pair (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [n next] (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max-next (fn [x next]
                       (let [prev-min (first x)
                             prev-max (second x)]
                         [(min prev-min next) (max prev-max next)]))
        ]
                      
    (reduce min-max-next [(first a-seq) (first a-seq)] (rest a-seq))))
                       
(defn insert [sorted-seq n]
  (loop [prefix []
         others sorted-seq
         ]
    (cond
     (empty? others) (conj prefix n)
     (> (first others) n) (concat (conj prefix n) others)
     :default (recur (conj prefix (first others)) (rest others)))))

(defn insertion-sort [a-seq]
  (reduce insert (vector (first a-seq)) (rest a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) ;remove if contains
    (conj a-set elem) ;add if does not
    ))

(defn parity [a-seq]
  (reduce toggle #{(first a-seq)} (rest a-seq)))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& many]
  (let [counter (fn [acc next] (inc acc))]
    (reduce counter 0 many)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & ys] (reduce my-* (my-* x y) ys)))  

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & ps] (reduce (fn [acc next] (fn [x] (and (acc x) (next x)))) p ps)))

(defn my-map [f a-seq]
  [:-])
