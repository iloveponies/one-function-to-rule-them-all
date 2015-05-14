(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [acc item]
            (if (empty? acc)
              (seq [item])
              (concat acc [x item])))
          '()
          a-seq))

(defn my-count [a-seq]
  #_(reduce + (map #(if % 1 0) a-seq)) ;0.365214msec for [1 2 3]
  (reduce (fn [counter item] (inc counter)) 0 a-seq)) ;0.068367msec for [1 2 3]

(defn my-reverse [a-seq]
  (reduce (fn [acc item] (conj acc item)) '()  a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[acc-min acc-max] item] [(min (or acc-min item) item) (max (or acc-max item) item)])
          [nil nil]
          a-seq))

(defn insert [sorted-seq n]
  (cond (nil? n) sorted-seq
        (empty? sorted-seq) (list n)
        (> (first sorted-seq) n) (cons n (insert sorted-seq nil))
        :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set item] 
            (if (contains? a-set item)
              (disj a-set item)
              (conj a-set item)))
          #{}
          a-seq))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map 
  ([f a-seq] (cond (empty? a-seq) '()
                   :else (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f col1 col2]
   (let [a (first col1)
         b (first col2)]
     (cond (or (empty? col1) (empty? col2)) '()
           :else (conj (my-map f (rest col1) (rest col2)) (f a b)))))
  ([f col1 col2 & more]
   (let [step (fn step [cs]
                (let [ss (my-map seq cs)]
                  (when (every? identity ss)
                    (cons (my-map first ss) (step (my-map rest ss))))))]
     (my-map #(apply f %) (step (conj more col2 col1))))))
