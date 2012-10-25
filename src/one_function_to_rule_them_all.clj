(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat []  a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str "" (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (drop-last (reduce (fn [acc e] (conj (conj acc e) x)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc x] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc x] (cond
                             (< x (first acc)) (assoc acc 0 x)
                             (> x (second acc)) (assoc acc 1 x)
                             :else acc))]
    (cond
     (empty? a-seq) [nil nil]
     (== 1 (count a-seq)) [(first a-seq) (first a-seq)]
     :else (reduce min-max [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) [n]
   (<= n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (reduce (fn [acc e] (and acc (e x))) (p x) more))))

(defn my-map 
  ([] nil)
  ([f s] (if (empty? s)
           []
           (cons (f (first s)) (my-map f (rest s)))))
  ([f s & more] (let [seqs (cons s more)]
                  (if (empty? (filter (fn [x] (not (empty? x))) seqs))
                    []
                    (cons (apply f (my-map first seqs))
                          (apply my-map f (my-map rest seqs)))))))