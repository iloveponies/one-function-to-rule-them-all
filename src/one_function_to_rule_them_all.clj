(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
  (reduce (fn [x y]
            (str x " " y))
          a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) []
    (reduce (fn [a b]
              (conj a x b))
            [(first a-seq)] (rest a-seq))))



(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (nil? e) count
                  (inc count)))]
    (reduce counter 0 a-seq)))



(defn my-reverse [a-seq]
  (let [rev (fn[a b]
        (cons b a))]
    (reduce rev [] a-seq)))



(defn min-max-element [a-seq]
  (let [minimi (fn [a b]
                 (if (> a b) b a))
        maksimi (fn [a b]
                 (if (< a b) b a))]
    [(reduce minimi a-seq)
     (reduce maksimi a-seq)]))



(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) (list n)
   (> (first sorted-seq) n) (cons n sorted-seq)
   :else (conj
          (insert (rest sorted-seq) n)
          (first sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))



(defn minus
  ([x] (- x))
  ([x y] (- x y)))



(defn count-params
  ([] 0)
  ([x & more]
  (inc (count more))))


(defn my-*
  ([] 1)
  ([x] x)
  ([x & kertoimet]
   (let [kerro (fn [a b]
                 (* a b))]
     (reduce kerro x kertoimet))))



(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [z]
           (and (x z) (y z))))
  ([x y & more] (reduce pred-and (pred-and x) more)))




(defn my-map [f a-seq]
  [:-])




