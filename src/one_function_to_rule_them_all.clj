(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce (fn [ac x] (concat ac x)) () a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    (empty? (rest a-seq)) ""
    :else (reduce (fn [ac next] (str ac " " next)) a-seq)))

(defn my-interpose [x a-seq]
  (if
    (empty? a-seq)
    '()
    (cons (first a-seq) (reverse (reduce (fn [ac el] (cons el (cons x ac))) () (rest a-seq))))))
    

(defn my-count [a-seq]
  (reduce (fn [ac x] (inc ac)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [ac x] (cons x ac)) () a-seq))

(defn min-max-element [a-seq]
  (let [f (fn [ac x]
           (if (nil? ac)
             [x x]
             (let [mi (first ac) ma (second ac)]
               (cond
                 (< x mi) [x ma]
                 (> x ma) [mi x]
                 :else ac)
               )))]
    (reduce f nil a-seq)))

(defn insert [sorted-seq n]
  (loop [r-head [] tail sorted-seq]
    (if (empty? tail)
      (reverse (cons n r-head))
      (if (< n (first tail))
        (concat (reverse r-head) (cons n tail))
        (recur (cons (first tail) r-head) (rest tail))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  ((if
    (contains? a-set elem)
    disj
    conj) a-set elem))

(defn parity [a-seq]
  (reduce (fn [ac x] (toggle ac x)) #{} a-seq))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more))
  
  )

(defn pred-and 
  ([] (fn [x] true))
  ([f] f)
  ([f & fs] (fn [x] (reduce (fn [ac f] (and ac (f x))) (f x) fs))))

(defn my-map 
  ([f & a-seqs]
    (if (empty? (first a-seqs))
      ()
      (loop [res () f-args (map first a-seqs) f-rests (map rest a-seqs)]
        (let [next-res (cons (apply f f-args) res)]
          (if (empty? (first f-rests))
            (reverse next-res)
            (recur next-res (map first f-rests) (map rest f-rests))))))))

