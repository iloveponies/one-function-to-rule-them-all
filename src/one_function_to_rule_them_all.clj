(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]  ;<
  (if (empty? a-seq)
    ""
    (clojure.string/join " " a-seq)))

(defn my-interpose [x a-seq]
  (let [f (fn [x]
            (fn [elem-1 elem-2]
              (conj elem-1 x elem-2)))]
    (if (seq a-seq)
      (reduce (f x) [(first a-seq)] (rest a-seq))
      a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc))
          0
          a-seq))

(defn my-reverse [a-seq]
  (let [f (fn [a b]
            (cons b a))]
    (reduce f [] a-seq)))

(defn min-max-element [a-seq]
  (let [f (fn [min-max x]
            (let [[min-x max-x] min-max]
              (let [new-min-x (if (or (nil? min-x)
                                  (< x min-x))
                                x
                                min-x)
                    new-max-x (if (or (nil? max-x)
                                  (> x max-x))
                                x
                                max-x)]
                [new-min-x new-max-x])))]
    (reduce f [] a-seq)))

(defn insert [sorted-seq n]  ;<
  (if (empty? sorted-seq)
    (cons n '())
    (let [elem (first sorted-seq)
          rest-of-seq (rest sorted-seq)]
      (if (< n elem)
        (cons n (cons elem rest-of-seq))
        (cons elem (insert rest-of-seq n))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [f (fn [a-map elem]
            (if (contains? a-map elem)
              (disj a-map elem)
              (conj a-map elem)))]
    (reduce f #{} a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))


(defn count-params [& xs]
  (my-count xs))

(defn my-* [& xs]
  (let [f (fn [x y]
            (loop [x x
                   y y
                   acc 0]
              (if (zero? y)
                acc
                (recur x (dec y) (+ acc x)))))]
    (reduce f 1 xs)))

(defn pred-and [& fs]
  (fn [x] (loop [x x
                 fs fs]
            (cond
             (empty? fs)
               true
             ((first fs) x)
               (recur x (rest fs))
             :else
               false))))

(defn my-map
  ([f a]
    (reverse
     (loop [f f
            a a
            acc '()]
       (if (empty? a)
         acc
         (recur f (rest a) (cons (if (coll? (first a))
                                       (apply f (first a))
                                       (f (first a)))
                                 acc))))))
  ([f a b]
    (my-map f
            (reverse
             (loop [a a
                    b b
                    acc nil]
               (if (or (empty? a) (empty? b))
                 acc
                 (recur (rest a) (rest b) (conj acc [(first a) (first b)])))))))
  ([f a b & seqs]
   (my-map f
    (reduce (fn [a b] (my-map conj a b)) (my-map vector a b) seqs))))

