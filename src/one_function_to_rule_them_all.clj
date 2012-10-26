(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if(empty? a-seq)
    ""
    (reduce (fn [acc x] (str acc " " x)) a-seq)))

(defn my-interpose [x a-seq]
  (let [f (fn [acc a] (conj acc x a))]
    (rest(reduce f [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc a] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn[acc a] (cons a acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn[[x y] a][(min x a) (max y a)]) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [p []
         r sorted-seq
         n n]
    (cond
      (empty? r)
        (conj p n)
      (<= n (first r))
        (concat p [n] r)
      :else 
        (recur (conj p (first r)) (rest r) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
    (reduce (fn [acc a] (if (contains? acc a)
                          (disj acc a)
                          (conj acc a))) #{} a-seq))

(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params 
  ([& more] (my-count more)))

(defn my-* 
  ([] 1)
  ([& more] (let[rec (fn rec [coll](if (empty? coll)
   								 		1
    							 (* (first coll) (rec (rest coll)))))]
              (rec more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (reduce (fn [acc a] (and acc (a x))) (p x) more))))

(defn my-map [f & a-seq]
  (cond
    (some empty? a-seq)
      []
    :else 
      (cons 
       (apply f (map first a-seq)) 
       (apply (partial my-map f ) (map rest a-seq)))))