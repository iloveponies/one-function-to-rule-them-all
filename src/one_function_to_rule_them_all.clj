(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [mstr (fn [x y] (str x " " y))]
    (if (empty? a-seq)
      ""
      (reduce mstr a-seq))))

(defn my-interpose [x a-seq]
  (let [mstr (fn [y z] (conj y x z))]
    (if (empty? (rest a-seq))
      a-seq
      (reduce mstr [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [count (fn [count e]
                (if e
                  (inc count)
                  count))]
    (reduce count 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rvrs (fn [sq e]
               (if e
                 (cons e sq)
                 sq))]
    (reduce rvrs '() a-seq)))

(defn min-max-element [a-seq]
  [(apply min a-seq) (apply max a-seq)])

(defn insert [sorted-seq n]
  (sort (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (let [red (fn [sq e]
              (if e
                (insert sq e)
                sq))]
    (reduce red '() a-seq)))

(defn parity [a-seq]
  (set (map key (filter (fn [x] (odd? (val x))) (frequencies (insertion-sort a-seq))))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& x]
   (let [red (fn [cnt e]
               (if e
                 (inc cnt)
                 cnt))]
     (reduce red 0 x))))

(defn my-*
  ([] 1)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & ps] (reduce pred-and (fn [x] (and (p1 x) (p2 x))) ps)))

(defn my-map
  ([f x] (map f x))
  ([f x & more]
   (let [srt (fn [sq x]
                (if (every? empty? x)
                  sq
                  (recur
                    (conj sq (map first x)) (map rest x))))]
     (map (fn [x] (apply f x)) (my-reverse (srt '() (cons x more)))))))

;(map (fn [x] (apply + x)) '((1 1 1 1) (2 2 2 2) (3 3 3 3)))





















