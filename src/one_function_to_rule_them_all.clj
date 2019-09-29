(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '[] a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [acc1 acc2] (if (= acc1 "") (str acc2) (str acc1 " " acc2)))]
  (reduce helper  "" a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [acc1 acc2] (conj (conj acc1 acc2) x))]
    (if (empty? a-seq) '() (reverse(rest(reduce helper '() a-seq))))))

(defn my-count [a-seq]
  (let [helper (fn [acc1 acc2] (inc acc1))]
  (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [acc1 acc2] (cons acc2 acc1))]
    (reduce helper '[] a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [acc1 acc2] [(min (get acc1 0) acc2) (max (get acc1 1) acc2)])]
    (if (empty? a-seq) '[] (reduce helper [(first a-seq) (first a-seq)] a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) [n] (if (<= n (first sorted-seq)) (cons n sorted-seq) (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert '[]  a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))
        toggle-seq (fn [b-seq elem] (toggle (set b-seq) elem))]
    (reduce toggle-seq #{} a-seq)))

(defn minus
  ([x] (- 0 x))
   ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (let [helper (fn [acc1 acc2] (fn [x] (and (acc1 x) (acc2 x))))
        helper1 (fn [x] true)]
  (reduce helper helper1 more)))

(defn my-map [f a-seq & more]
  ("/D:\\"))

