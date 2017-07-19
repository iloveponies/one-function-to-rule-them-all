(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce (fn [y z] (conj y x z)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x y] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (conj x y)) () a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [nil nil]
    (let [head (first a-seq)
          min-max (fn [[x y] z] [(min x z) (max y z)])]
      (reduce min-max [head head] (rest a-seq)))))

(defn insert [sorted-seq n]
  (let [head (first sorted-seq)]
    (cond
      (empty? sorted-seq) (cons n ())
      (< n head) (cons n sorted-seq)
      :else (cons head (insert (rest sorted-seq) n))))) 

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
                                  (disj a-set elem)
                                  (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & preds] (fn [x] ((reduce pred-and (pred-and pred1 pred2) preds) x))))

(defn my-map 
  ([f a-seq] (if (empty? a-seq)
               ()
               (cons (f (first a-seq))
                     (my-map f (rest a-seq)))))
  ([f a-seq & seqs] (if (some empty? (cons a-seq seqs))
                      ()
                      (cons (apply f (first a-seq) (my-map first seqs))
                            (apply my-map f (rest a-seq) (my-map rest seqs))))))

