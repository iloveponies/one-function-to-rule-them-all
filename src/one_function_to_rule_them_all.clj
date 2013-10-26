(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn my-str [s1 s2]
  (str s1 " " s2))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce my-str a-seq)))

(defn my-interpose [x a-seq]
  (let [int-helper (fn [e1 e2] (concat e1 (vector x) (vector e2)))]
    (cond (empty? a-seq) ()
          (== 1 (count a-seq)) (seq a-seq)
          :else (reduce int-helper (vector (first a-seq)) (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [c e]
                  (if e
                    (inc c)
                    c))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [revert (fn [s e]
                  (conj s e))]
    (reduce revert '() a-seq)))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq) (reduce max a-seq)))

(defn ins-helper [n a-seq i]
  (if (>= n (get (vec a-seq) (dec (count a-seq))))
    (seq (conj (vec a-seq) n))
    (if (< n (get (vec a-seq) i))
      (concat (take i a-seq) (cons n (drop i a-seq)))
      (ins-helper n a-seq (inc i)))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq (vector n))
    (ins-helper n sorted-seq 0)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (let [helper (fn [s e]
                 (toggle s e))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce my-* (* x y) more)))

(defn pred-and [x]
  )

(defn my-map [f a-seq]
  [:-])
