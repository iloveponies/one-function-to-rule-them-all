(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [acc x] (str acc " " x))
              a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
      (reduce (fn [acc elem] (concat acc x elem)) a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc x] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (conj acc x)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce  (fn [[mmin mmax] num] [(min mmin num) (max mmax num)]) [0 0] a-seq))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (< n (first sorted-seq)) (cons n sorted-seq)
   :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc x] (insert acc x))
          [(first a-seq)]
          (rest a-seq)))

(defn parity [a-seq]
  (reduce (fn [acc x] ((if (contains? acc x) disj conj) acc x))  #{} a-seq))

(defn minus
  [x] (* -1 x)
  [x y] (- x y))

(defn count-params [& x]
  (count x))

(defn my-*
  [& x] (reduce * 1 x))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (fn [x]
              (reduce (fn [acc y] (and acc (y x))) true more))))

(defn my-map [f a-seq]
  (reverse (reduce
            (fn [acc x] (conj acc (f x)))
            '()
            a-seq)))
