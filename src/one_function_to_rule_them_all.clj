(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat "" a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (rest (reduce (fn [acc cur] (concat acc " " cur)) "" a-seq)))))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [acc cur] (conj acc x cur)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc cur] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (let [n (count a-seq)]
    (letfn [(f [a-seq i]
                (if (and (< i (/ n 2)) (> n 0))
                  (let [j (- n i 1)
                        v1 (nth a-seq i)
                        v2 (nth a-seq j)]
                    (f
                      (assoc (assoc a-seq i v2) j v1)
                      (inc i)))
                  (seq a-seq)))]
      (if (empty? a-seq)
        '()
        (f a-seq 0)))))

(defn min-max-element [a-seq]
  [(reduce (fn [acc cur] (if (or (nil? acc) (> acc cur)) cur acc)) (first a-seq) a-seq)
   (reduce (fn [acc cur] (if (or (nil? acc) (< acc cur)) cur acc)) (first a-seq) a-seq)
   ])

(defn insert-i [sorted-seq n i]
  (cond
    (>= i (count sorted-seq)) (concat sorted-seq [n])
    (<= n (nth sorted-seq i)) (concat (take i sorted-seq) [n] (take-last (- (count sorted-seq) i) sorted-seq))
    :else (insert-i sorted-seq n (inc i))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (insert-i sorted-seq n 0)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (letfn [(toggle [a-set elem]
            (if (contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred1] pred1)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (fn [x]
                          (reduce
                            (fn [acc pred] (and acc (pred x)))
                            (and (pred1 x) (pred2 x))
                            more))))

(defn my-map
  ([f a-seq] (map f a-seq))
  ([f a-seq & more] (apply map f (cons a-seq more))))
