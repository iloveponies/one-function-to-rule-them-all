(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [strspace (fn [e f]
                     (str e " " f))]
      (reduce strspace a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (if (empty? (rest a-seq))
      (vector (first a-seq))
      (let [inter (fn [e f]
                    (if (not (or (list? e) (vector? e) (seq? e)))
                      (vector e x f)
                      (conj e x f)))]
        (reduce inter a-seq)))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (let [counter (fn [r _]
              (inc r))]
      (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    '()
    (let [conser (fn [s e]
                   (cons e s))]
      (reduce conser '() a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [checker (fn [[mini maxi] e]
                    (cond
                       (= mini nil) [e e]
                       (< e mini) [e maxi]
                       (> e maxi) [mini e]
                       :default [mini maxi]))]
      (reduce checker [nil nil] a-seq))))


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n '())
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq)
    '()
    (reduce insert '() a-seq)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (let [toggle (fn [a-set elem]
                   (if (contains? a-set elem)
                     (disj a-set elem)
                     (conj a-set elem)))]
      (reduce toggle #{} a-seq))))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [& args]
  (my-count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [e] true))
  ([p] p)
  ([p & more]
    (fn [e]
      (let [ander (fn [acc pre]
                    (and acc (pre e)))]
        (reduce ander (p e) more)))))

(defn my-map [f a-seq]
  [:-])
