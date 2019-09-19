(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc s] (apply str (concat acc " " s))) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [acc s] (conj acc x s)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [min-max x]
                 (let [f (first min-max)
                       s (second min-max)]
                   (if (< x f)
                     [x s]
                     (if (> x s)
                       [f x]
                       min-max))))
        f (first a-seq)
        r (rest a-seq)]
    (reduce helper [f f] r)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (let [f (first sorted-seq)]
      (if (< n f)
        (cons n sorted-seq)
        (cons f (insert (rest sorted-seq) n))))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [helper (fn [res x]
                 (if (contains? res x)
                   (disj res x)
                   (conj res x)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([f] f)
  ([f g] (fn [x] (and (f x) (g x))))
  ([f g & more] (reduce pred-and (pred-and f g) more)))

(defn my-map
  ([f a-seq] (reduce (fn [res x] (conj res (f x))) [] a-seq))
  ([f a-seq b-seq] (if (or (empty? a-seq) (empty? b-seq))
                     []
                     (cons (f (first a-seq) (first b-seq)) (my-map f (rest a-seq) (rest b-seq)))))
  ([f a-seq b-seq & more] (reduce (fn [res s] (my-map f res s)) (my-map f a-seq b-seq) more)))

