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
 
 (parity [1 2 31 3 2])
 
 (defn minus
   ([x] (- 0 x))
   ([x y] (- x y)))
 
 (minus 2 1)
 
 (defn count-params
   ([& more]
     (reduce (fn [acc x] (inc acc)) 0 more)))
 
 (count-params :a)
 (count-params)


(defn my-*
   ([] 1)
   ([& more]
      (reduce (fn [acc y] (* acc y)) 1 (seq more))))

(my-* 2 3)

(defn pred-and
   ([] (fn [x] true))
   ([pred1?] pred1?)
   ([pred1? pred2?] (fn [x] (and (pred1? x) (pred2? x))))
   ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))

(pred-and (filter (pred-and) [1 0 -2]))

(defn my-map
   ([f & more]
    (loop [result []
           left more]
      (if (some empty? left)
        (seq result)
        (recur (conj result (apply f (map first left))) (map rest left))))))
