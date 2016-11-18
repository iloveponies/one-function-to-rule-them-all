(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [first-elem (first a-seq)
        rest-seq (rest a-seq)]
    (if (empty? rest-seq)
      (str first-elem)
      (reduce #(str %1 " " %2) first-elem rest-seq))))

(defn my-interpose [x a-seq]
  (let [a (first a-seq)
        b-seq (rest a-seq)]
    (cond
      (empty? a-seq) '()
      (empty? b-seq) (list a)
      :else (cons a (reduce #(concat %1 (list x %2)) '() b-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    [nil nil]
    (let [a (first a-seq)
          b-seq (rest a-seq)
          update-min-max (fn [[min-elem max-elem] elem]
                           [(min min-elem elem) (max max-elem elem)])]
      (reduce update-min-max [a a] b-seq))))

(defn insert [sorted-seq n]
  (let [[before after] (split-with #(< % n) sorted-seq)]
    (concat before (list n) after)))

(defn insertion-sort [a-seq]
  (reduce #(insert %1 %2) '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set a]
                 (if (contains? a-set a)
                   (disj a-set a)
                   (conj a-set a)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [_] true))
  ([pred] pred)
  ([pred1 pred2 & more]
    (fn [x] (reduce #(and %1 (%2 x)) (and (pred1 x) (pred2 x)) more))))

(defn my-map
  ([f a-seq]
    (when (not (empty? a-seq))
      (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq & more-seqs]
    (let [ss (conj more-seqs a-seq)]
      (when (every? (complement empty?) ss)
        (cons (apply f (my-map first ss)) (apply my-map f (my-map rest ss)))))))
