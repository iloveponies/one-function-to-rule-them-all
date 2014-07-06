(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [ss s] (str ss " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
      (cons (first a-seq)
            (reduce (fn [ss s] (concat ss (cons x (list s)))) '() (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [ss s] (inc ss)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [ss s] (cons s ss)) '() a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
      (reduce (fn [v item]
                (let [min (v 0)
                      max (v 1)]
                  (cond
                   (> item max) (assoc v 1 item)
                   (< item min) (assoc v 0 item)
                   :else v)))
              [(first a-seq) (first a-seq)]
              (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) (list n)
      (if (< n (first sorted-seq))
        (cons n sorted-seq)
        (cons (first sorted-seq)
              (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) '()
      (reduce insert (list (first a-seq)) (rest a-seq))))

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

(defn my-* [& xs]
  (let [helper (fn [ss n]
                 (if (empty? ss) n
                     (recur (rest ss) (* n (first ss)))))]
      (if (empty? xs) 1
          (* (first xs) (helper (rest xs) 1)))))

(defn pred-and [& xs]
  (if (empty? xs)
    (fn [x] true)
    (fn [x] (every? boolean (map (fn [pred] (pred x)) xs)))))

(defn my-map [f & a-seq]
  (if (empty? (first a-seq))
    '()
    (cons (apply f (map first a-seq))
          (apply my-map  (cons f (map rest a-seq))))))
