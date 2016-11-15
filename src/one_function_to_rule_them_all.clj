(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (reduce #(concat %1 " " %2) a-seq))))

;; any more elegant solution without dropping last element?
(defn my-interpose [x a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [f (fn [elements el](conj elements el x))]
      (drop-last (reduce f [] a-seq)))))

;; it is possible to define anonymous function with unused param using
;; the reader macro #()?
(defn my-count [a-seq]
  (let [f (fn [counter _] (inc counter))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

;; will be much better not to rely on order, but using map keys instead
;; like #{:min x :max y}
(defn min-max-element [a-seq]
  (let [f (fn [result el] [(min el (get result 0 el)) (max el (get result 1 el))])]
    (reduce f [] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (let [insert-between (fn [prevs x nexts]
                           (concat prevs (cons x nexts)))
          append (fn [prevs x y]
                   (conj prevs x y))]
      (loop [c (first sorted-seq)
            former []
            latter sorted-seq]
       (cond
         (<= n c) (insert-between former n latter)
         (empty? (rest latter)) (append former c n)
         :else (recur (first (rest latter)) (conj former c) (rest latter)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [s el] (if (contains? s el)
                            (disj s el)
                            (conj s el)))]
    (reduce toggle #{} a-seq)))

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
  ([] (fn [_] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

;; small pieces to compose a my-map solution
(defn heads [[a-seq & more]]
  "Extracts first elements from one or more sequences bound by the & operator of a caller."
  (if (empty? more)
    (first a-seq)
    (loop [acc []
          el (first a-seq)
          tail more]
     (cond
       (nil? el) '()
       (empty? tail) (conj acc el)
       :else (recur (conj acc el) (first (first tail)) (rest tail))))))

(defn tails [[a-seq & more]]
  "Extracts tails of a multiple sequences bound by the & operator of a caller."
  (if (empty? more)
    (rest a-seq)
    (loop [acc []
          r (rest a-seq)
          remainings more]
     (cond
       (empty? r) '()
       (empty? remainings) (conj acc r)
       :else (recur (conj acc r) (rest (first remainings)) (rest remainings))))))

(defn my-map-1 [f a-seq]
  "Unary map operation on a single sequence."
  (loop [acc []
         r (f (first a-seq))
         tail (rest a-seq)]
    (if (empty? tail)
      (conj acc r)
      (recur (conj acc r) (f (first tail)) (rest tail)))))

(defn my-map [f & seqs]
  (if (empty? (rest seqs))
    (my-map-1 f (first seqs))
    (loop [acc []
          h (apply f (heads seqs))
          remainings (tails seqs)]
     (if (empty? remainings)
       (conj acc h)
       (recur (conj acc h) (apply f (heads remainings)) (tails remainings))))))


