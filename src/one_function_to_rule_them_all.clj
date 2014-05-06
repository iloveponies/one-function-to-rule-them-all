(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2) a-seq
    (reverse (reduce (fn [a b] (conj (conj a x) b)) (take 1 a-seq) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [acc elem] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [f (fn [a b] (cons b a))]
    (reduce f '() a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [a b]
            (let [min (if (< b (first a)) b (first a))
                  max (if (> b (second a)) b (second a))]
              [min max]))]
    (reduce f [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) (seq [n])
    (if (> (first sorted-seq) n)
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce (fn [a b] (insert a b)) (take 1 a-seq) (rest a-seq)))

(defn parity [a-seq]
  (reduce (fn [a b] (if (contains? a b) (disj a b) (conj a b))) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce (fn [a b] (* a b)) (* x y) more)))

(defn pred-and
  ([] (fn [i] true))
  ([& more] (fn [i] (reduce (fn [a b] (and a (b i))) true more))))

(defn take-first [a-seq]
  (reduce (fn [a b]
              (let [frst (cons (first b) (first a))
                    rst (cons (rest b) (second a))]
                [frst rst]))
            (vector nil nil) a-seq))

(defn mapper [f a-seq]
  (if (contains? (set a-seq) '()) nil
    (let [splitted (take-first a-seq)]
      (cons (apply f (first splitted)) (mapper f (second splitted))))))

(defn my-map [f & more]
  (mapper f more))
