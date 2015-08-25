(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce 
      (fn [a b]
        (str a " " b))
      a-seq)))

(defn my-interpose [x a-seq]
  (let [result 
        (butlast 
          (reduce 
            (fn [a b]
              (conj 
                (conj a b) 
                x))
          []
          a-seq))]
    (if (nil? result) [] result)))

(defn my-count [a-seq]
  (let [counter 
        (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser
       (fn [vec-so-far e] (cons e vec-so-far))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (let [extremums 
       (fn [[minimum maximum] e]
         (cond
           (and (nil? minimum) (nil? maximum)) [e e]
           (< e minimum) [e maximum]
           (< maximum e) [minimum e]
           :else [minimum maximum]))]
    (reduce extremums [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (cond 
    (empty? sorted-seq) (vector n)
    (>= n (first sorted-seq))
      (cons (first sorted-seq) (insert (rest sorted-seq) n))
    :else 
      (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle 
       (fn [a-set elem]
         (set (if (contains? a-set elem)
                (disj a-set elem)
                (conj a-set elem))))]
    (reduce toggle #{} a-seq)))

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
  ([x y & more] 
    (reduce * (* x y) more)))

(defn pred-and 
  ([] 
    (fn [x] true))
  ([p] 
    (fn [x] p))
  ([p1 p2]
    (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
    (fn [x] 
        (reduce 
          (fn [a b] (and a (b x)))
          (and (p1 x) (p2 x))
          more))))

(defn my-map 
  ([f a-seq] 
    (if (empty? a-seq)
      '()
      (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq b-seq]
    (if (or (empty? a-seq) (empty? b-seq))
      '()
      (cons 
        (f (first a-seq) (first b-seq)) 
        (my-map f (rest a-seq) (rest b-seq)))))
  ([f a-seq b-seq & more]
    (if (or 
          (empty? a-seq) 
          (empty? b-seq) 
          (some empty? more))
      '()
      (cons
        (apply f (cons (first a-seq) (cons (first b-seq) (map first more))))
        (my-map f (rest a-seq) (rest b-seq) (map rest more))))))
