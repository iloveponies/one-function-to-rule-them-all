(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (== 1 (count a-seq)) (cons (first a-seq) '())
   :else (rest (reduce (fn [z y] (conj (conj  z x) y)) [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [x y] (inc x))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) '() a-seq))

(defn min-max-element [a-seq]
  (let [minmax (fn [[x y] z]
                 (cond
                  (> x z) [z y]
                  (< y z) [x z]
                  :else [x y]))
        eka (first a-seq)]
    (reduce minmax [eka eka] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [eka (first sorted-seq)]
    (cond
     (empty? sorted-seq) (cons n '())
     (> eka n) (cons n sorted-seq)
     (< (last sorted-seq) n) (concat sorted-seq (cons n '()))
     :else (loop [ekat (cons eka '())
                  loput (rest sorted-seq)]
             (cond
              (< (last ekat) n (first loput)) (concat ekat (cons n loput))
              (empty? loput) (reverse (cons n (reverse loput)))
              :else (recur (reverse (cons (first loput) (reverse ekat))) (rest loput)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set e]
                 (if (contains? a-set e)
                   (disj a-set e)
                   (conj a-set e)))]
    (reduce toggle #{} a-seq)))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [ & more]
  (let [counter (fn [x y] (inc x))]
    (reduce counter 0 more)))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and [& x]
  (if (empty? x)
    (fn [y] true)
    (fn [y] (every? true? (map (fn [pred] (pred y)) x)))))

(defn my-map [f & a-seq]
  (if (empty? (first a-seq))
    '()
    (cons (apply f (map first a-seq))
          (apply map (cons f (my-map rest a-seq))))))

(defn my-map
  ([f a-seq] (reverse (loop [vals a-seq
                             answer '()]
                        (if (empty? vals)
                          answer
                          (recur (rest vals) (cons (f (first vals)) answer))))))
  ([f a-seq & more] (reverse (loop [vals (cons a-seq more) ans '()]
                               (if (empty? (first vals))
                                 ans
                                 (recur (my-map rest vals)
                                        (cons (apply f (my-map first vals)) ans)))))))
