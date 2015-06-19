(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [previous elem] (str previous (str " " elem))) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [previous elem]
              (conj (conj previous x) elem))
            (vector (first a-seq)) (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [current-count elem] (inc current-count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [previous elem] (cons elem previous)) [] (seq a-seq)))

(defn min-max-element [a-seq]
  (reduce (fn [[current-min current-max] elem]
            (cond
             (and (< elem current-min) (> elem current-max)) (vector elem elem)
             (< elem current-min) (vector elem current-max)
             (> elem current-max) (vector current-min elem)
             :else (vector current-min current-max)))
          [(first a-seq) (first a-seq)]
          (rest a-seq)))


(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (cons n sorted-seq)
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& every] (count every)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x]
     (reduce (fn [current p]
               (if (= false current)
                 false
                 (p x))) (and (p1 x) (p2 x)) more))))

(defn my-map
  ([function a-seq] (reduce (fn [acc elem]
                              (conj acc
                                    (function elem)))
                            [] a-seq)))
