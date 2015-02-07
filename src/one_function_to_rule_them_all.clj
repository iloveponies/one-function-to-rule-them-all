(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn[a b] (str a " " b) )  a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [a b] (conj a  x b)) (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn[a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) '() a-seq))

(defn min-max-element [a-seq]
  (let [min-ele (reduce (fn [a b] (min a b)) a-seq)
        max-ele (reduce (fn [a b] (max a b)) a-seq)]
    [min-ele max-ele]))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (cons n '())
        (<= n (first sorted-seq)) (cons n sorted-seq)
        :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) '()  a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [a b] (toggle a b)) #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more] (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (fn [x] (reduce (fn [a b] (and a (b x))) true more)))

(defn simple-map [f a-seq]
    (if (empty? a-seq)
     '()
     (cons (f (first a-seq)) (simple-map f (rest a-seq)))))

(defn helper [comb]
  (if (every? empty? comb)
  '()
   (cons (simple-map first comb) (helper (simple-map rest comb)))))

(defn my-map
  ([f a-seq] (simple-map f a-seq))
  ([f a-seq & more]
   (let [seqs (helper (cons a-seq more))]
     (simple-map (fn [x] (apply f x)) seqs))))
                 
        
