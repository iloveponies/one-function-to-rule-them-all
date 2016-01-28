(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str 
            ""
            (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (rest (reduce (fn [y z]
                    (conj y x z))
                   []
                   a-seq))))

(defn my-count [a-seq]
  (reduce + 0 (map (fn [x] 1) a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (list n)
        (<= n (first sorted-seq)) (cons n sorted-seq)
        :else (cons (first sorted-seq)
                    (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                (if (contains? a-set elem)
                  (disj a-set elem)
                  (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))
  

(defn my-* [& args]
  (reduce * 1 args))

(defn pred-and [& fns]
  (reduce (fn [x y]
            (fn [z] (and (x z) (y z))))
          (fn [x] true)
          fns))

(defn my-map [f & seqs]
  (let [firsts (fn firsts [x]
                 (if (empty? x)
                   '()
                   (cons (first (first x))
                         (firsts (rest x)))))
        rests (fn rests [x]
                 (if (empty? x)
                   '()
                   (cons (rest (first x))
                         (rests (rest x)))))]

    (if (some empty? seqs)
      '()
      (cons (apply f (firsts seqs))
            (apply my-map f (rests seqs))))))
