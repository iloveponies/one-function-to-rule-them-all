(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b](str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b]
              (if (empty? a)
                 (conj a b)
                 (conj (conj a x) b))) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count e] (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[a b] elem] [(min a elem)(max b elem)]) [Integer/MAX_VALUE 0] a-seq))

(defn insert [sorted-seq n]
  (loop [a-seq ()
         b-seq sorted-seq]
    (cond
      (empty? b-seq)(concat a-seq (cons n ()))
      (< (first b-seq) n) (recur (concat a-seq (cons (first b-seq) ())) (rest b-seq))
      :else (concat a-seq (cons n ()) b-seq))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted-seq elem](insert sorted-seq elem)) () a-seq))

(defn parity [a-seq]
  (reduce (fn [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem))) #{} a-seq))

(defn minus
  ([x] ( - 0 x))
  ([x y] ( - x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more] (reduce (fn [count elem](inc count)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce (fn [a b](* a b)) (* x y) more)))


(defn pred-and
  ([] (fn[x] true))
  ([p] (fn[x] (p x)))
  ([p1 p2] (fn[x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn[x] (reduce (fn [a b](and a (b x))) (and (p1 x) (p2 x)) more))))


(defn combine [a-seq b-seq]
;; combine () (a1 a2 a3) => ((a1)(a2)(a3))
;; combine ((a1) (a2) (a3))(b1 b2 b3) => ((b1 a1) (b2 a2) (b3 a3))
  (loop [res ()
         a a-seq
         b b-seq]
    (cond
     (empty? b) res
     (empty? a-seq) (recur (concat res (cons (cons (first b) ())())) a (rest b))
     :else (recur (concat res (cons(cons (first b) (first a))())) (rest a)(rest b)))))

(defn my-map
  ([f a-seq] (map f a-seq))
  ([f a-seq & more]
    (map (fn[a-seq] (apply f a-seq)) (reduce combine (combine () a-seq) more))))

