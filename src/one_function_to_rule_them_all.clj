(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [elem1 elem2] (apply str (interpose " " [elem1 elem2]))) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    (into '() a-seq)
    (reduce (fn [interposed-seq elem] (concat interposed-seq [x elem]))
            [(first a-seq)]
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [count e] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [reverse-seq e] (conj reverse-seq e)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce (fn [[min max] e]
              (cond
               (< e min) [e max]
               (> e max) [min e]
               :else [min max]))
            [(first a-seq) (first a-seq)]
            a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (cons n '())
   (<= n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))


(defn insertion-sort [a-seq]
    (reduce (fn [sorted-seq e]
              (insert sorted-seq e))
            '()
            a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (reduce (fn [parity-set e]
            (toggle parity-set e))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more] (+ 2 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
    (reduce (fn [comb e] (fn [x] (and (comb x) (e x))))
            (fn [x] (and (pred1 x) (pred2 x)))
            more)))

(defn my-map
  ([f a-seq] (reverse (reduce (fn [res e]
                       (conj res (f e))) '() a-seq)))
  ([f a-seq & more]
   (let [multi-seq (cons a-seq more)
         loop-first (fn [seqs]
                      (loop [firsts '()
                             seqs seqs]
                        (if (empty? seqs)
                          firsts
                          (recur (conj firsts (first (first seqs))) (rest seqs)))))
         loop-rest (fn [seqs]
                      (loop [rests '()
                             seqs seqs]
                        (if (empty? seqs)
                          rests
                          (recur (conj rests (rest (first seqs))) (rest seqs)))))
         any-empty? (fn [seqs] (reduce (fn [b e] (if b true (empty? e))) false seqs))]
     (reverse (loop [res '()
            multi-seq multi-seq]
       (if (any-empty? multi-seq)
         res
         (recur (conj res (apply f (reverse (loop-first multi-seq)))) (reverse (loop-rest multi-seq)))))))))
