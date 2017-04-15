(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reverse (reduce
              (fn [i1 i2]
                (cons i2 (cons x i1)))
              (seq [(first a-seq)])
              (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [init _] (inc init)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [init x] (cons x init)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [init x]
            (let [min-item (first init)
                  init-with-min (if (< x min-item)
                                 (assoc init 0 x)
                                 init)
                  max-item (second init)
                  init-with-min-max (if (> x max-item)
                                      (assoc init 1 x)
                                      init)]
              init-with-min-max))
          (vector (first a-seq) (first a-seq))
          a-seq))

(defn insert [sorted-seq n]
  (loop [before '()
         after sorted-seq]
    (if (or (empty? after) (< n (first after)))
      (concat (reverse (cons n before)) after)
      (recur (cons (first after) before) (rest after)))))

(defn insertion-sort [a-seq]
  (reduce (fn [init x] (insert init x))
          []
          a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce (fn [init x] (toggle init x))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
   ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [i] true))
  ([x] x)
  ([x y] (fn [i] (and (x i) (y i))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f params]
   (for [param params]
     (f param)))
  ([f params & more]
   (let [params (cons params more)]
     (loop [results '()
            params params]
       (if (empty? (first params))
         (reverse results)
         (let [firsts (for [item params] (first item))
             rests (for [item params] (rest item))
             result (apply f firsts)]
           (recur (cons result results) rests)))))))
