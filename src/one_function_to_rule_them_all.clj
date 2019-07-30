(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc elt] (if (empty? acc) [elt] (conj acc x elt))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elt] (cons elt acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc elt] (if (empty? acc)
                          [elt elt]
                          (let [[cur-min cur-max] acc]
                            [(min cur-min elt) (max cur-max elt)]))) [] a-seq))

; When in doubt, use brute force
(defn insert [sorted-seq n]
  (let [lower (filter (fn [x] (< x n)) sorted-seq)
        higher (filter (fn [x] (> x n)) sorted-seq)]
    (concat lower (list n) higher)))

(defn insertion-sort [a-seq]
  (reduce (fn [acc n] (insert acc n)) '() a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [acc elt] (toggle acc elt)) #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& args]
  (reduce * 1 args))

; It seems like there should be an easier way to do this
(defn pred-and [& pred-list]
  (fn [x]
    (let [results (map (fn [pred] (pred x)) pred-list)]
      (every? (fn [p] p) results))))

; Was I supposed to use reduce for my-map?

(defn my-map-help [f seq-list acc]
  (if (some empty? seq-list)
    (reverse acc)
    (let [args (map first seq-list)]
      (recur f (map rest seq-list) (cons (apply f args) acc)))))

(defn my-map [f & seq-list]
  (my-map-help f seq-list '()))
  