(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce str "" (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (cons (first a-seq)
          (reduce (fn [initial item]
                      (conj (conj initial x) item))
                  []
                  (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [cal aseq]
                    (inc cal))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [initial item] (cons item initial)) [] a-seq))

(defn min-max-element [a-seq]
  [(reduce min a-seq) (reduce max a-seq)])

(defn insert [sorted-seq n]
  (let [element1 (first sorted-seq)
         element-rest (rest sorted-seq)]
    (if (empty? sorted-seq)
      (cons n [])
      (if (<= n element1)
        (concat [n] sorted-seq)
        (cons element1 (insert element-rest n))))))

(defn insertion-sort [a-seq]
  (reduce insert [(first a-seq)] (rest a-seq)))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [initial x] (toggle initial x)) #{} a-seq))

(defn minus
  ([x] (- x (* 2 x)))
  ([x y] (- x y))
  )

(defn count-params
  ([] 0)
  ([x & more] (inc (count more)))
  )

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x](fn [x] x))
  ([x1 y1] (fn [x] (and (x1 x) (y1 x))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f a-seq] (reduce (fn [initial x] (conj initial (f x))) [] a-seq))
  ([f a-seq & more]
   (loop [new-seq (cons a-seq more)
          init []]
     (if (some nil? (my-map first new-seq))
       init
      (recur (my-map rest new-seq) (conj init (apply f (my-map first new-seq))))))))
