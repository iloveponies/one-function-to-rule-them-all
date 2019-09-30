(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" 
;    (reduce str (interpose \space a-seq))))
    (reduce (fn [str-1 str-2] (str str-1 \space str-2)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce 
    (fn [a-vec elem] 
      (conj 
        (if (empty? a-vec) [] (conj a-vec x)) 
        elem))
    []
    a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [seq-1 elem] (cons elem seq-1)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [min-max elem]
      [(min elem (or (first min-max) elem))
       (max elem (or (second min-max) elem))])
    [nil nil] a-seq))

(defn insert [sorted-seq n]
  (let [beg (filter (fn [e] (<= e n)) sorted-seq)
        end (drop (count beg) sorted-seq)]
    (concat beg (cons n end))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce 
    (fn [a-set elem] 
      ((if (contains? a-set elem) disj conj) a-set elem)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([] 0)
  ([& more] (reduce (fn [acc _] (inc acc)) 0 more)))

(defn my-*
  ([] 1)
  ([& more] (reduce * 1 more)))

(defn pred-and
  ([] (fn [_] true))
  ([& more] 
   (fn [x] 
     (reduce (fn [acc pred] (and acc (pred x))) true more))))

(defn my-map
  ([f & seqs]
     (loop [acc '()
            lefts seqs]
       (let [firsts (reduce (fn [a-seq elem] (cons (first elem) a-seq)) '() lefts)
             rests (reduce (fn [a-seq elem] (cons (rest elem) a-seq)) '() lefts)]
         (if (every? nil? firsts) (reverse acc)
           (recur (cons (apply f firsts) acc) rests))))))

