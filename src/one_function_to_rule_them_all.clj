(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a-word words]
               (str a-word " " words))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (cons (first a-seq)
          (reduce (fn [initial item] 
                    (conj (conj initial x) item))
                  []
                  (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [initial item] (inc initial)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [initial item] (cons item initial)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq)
          (cons n [])
        (<= n (first sorted-seq))
          (concat (cons n []) sorted-seq)
        :else
          (concat (cons (first sorted-seq) []) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& more] 
  (reduce * more))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f & more]
  (if (some empty? more)
    []
    (cons (apply f (map first more)) (apply (partial my-map f) (map rest more)))))