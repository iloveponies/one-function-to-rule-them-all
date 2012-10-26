(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
  (seq (reduce (fn [new-seq elem] (if (empty? new-seq)
                       (conj new-seq elem)
                       (conj (conj new-seq x) elem))) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc elem]
            (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [min-max elem]
            (let [[mi ma] min-max]
              (cond
               (< elem mi)
               [elem ma]
               (> elem ma)
               [mi elem]
               :else
               min-max)))
          [(first a-seq) (first a-seq)] a-seq))

(defn insert [sorted-seq n]
  (seq (loop [sorted-seq sorted-seq
              new-seq []]
         (cond
          (empty? sorted-seq)
          (conj new-seq n)
          (> (first sorted-seq) n)
          (concat (conj new-seq n) sorted-seq)
          :else
          (recur (rest sorted-seq) (conj new-seq (first sorted-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))
          #{} a-seq))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [elem] true))
  ([x?] (fn [elem]
         (x? elem)))
  ([x? y?] (fn [elem]
           (and (x? elem) (y? elem))))
  ([x? y? & more] (reduce pred-and (pred-and x? y?) more)))

(defn my-map 
  ([f a-seq]
   (seq (reduce (fn [acc elem] (conj acc (f elem))) [] a-seq)))
  ([f a-seq & more] (seq (let [tails (fn [a-seq]
                           (my-map rest a-seq))
                    heads (fn [a-seq]
                            (my-map first a-seq))]
                (loop [tails-seq (conj more a-seq)
                       mapped-seq []
                       n (count (first tails-seq))]
                  (if (= n 0)
                    mapped-seq
                    (recur (tails tails-seq)
                           (conj mapped-seq (apply f (heads tails-seq)))
                           (dec n))))))))