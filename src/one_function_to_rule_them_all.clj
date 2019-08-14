(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str str1 " " str2))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (rest (reduce (fn [acc elem] (conj (conj acc x) 
                                       elem))
             []
             a-seq))))

(defn my-count [a-seq]
  (reduce (fn [cnt elem] (inc cnt))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) 
          () 
          a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [min-max-vec elem]
                  [(min elem (first min-max-vec))
                   (max elem (second min-max-vec))])
        fst (first a-seq)]
    (reduce min-max [fst fst] a-seq)))

(defn insert [sorted-seq n]
  (loop [init (take-while #(< % n) sorted-seq)
         tail (drop-while #(< % n) sorted-seq)]
    (concat init
            (cons n tail))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- 0 x))
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
  ([pred] pred)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x] (and ((pred-and p1 p2) x)
                               (every? true? (map #(% x) more))))))

(defn head-taker [xss]
  (cond (empty? xss) ()
        (some empty? xss) ()
        :else (cons (first (first xss))
                    (head-taker (rest xss)))))

(defn removed-heads [xss]
  (cond (empty? xss) ()
        (some empty? xss) ()
        :else (cons (rest (first xss))
                    (removed-heads (rest xss)))))

(defn my-map
  ([f xs] (loop [ys xs
                 acc []]
            (if (empty? ys)
              acc
              (recur (rest ys)
                     (conj acc (f (first ys)))))))
  ([f xs & more] (loop [ys xs
                        tail more
                        acc []]
                   (if (empty? ys)
                     acc
                     (recur (rest ys)
                            (removed-heads tail)
                            (conj acc
                                  (apply f (head-taker (cons ys tail)))))))))
