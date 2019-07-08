(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (let [interposer (fn [coll elem] (if (empty? coll) [elem] (conj (conj coll x) elem)))]
    (reduce interposer () a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [acc e] (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc e] (cons e acc))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (let [minmax (fn [[mn mx] elem] [(min elem mn) (max elem mx)])
        f (first a-seq)]
    (reduce minmax [f f] a-seq)))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (< n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc elem] (if (contains? acc elem) (disj acc elem) (conj acc elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q & more] (let [reducer (fn [p q] (fn [x] (and (p x) (q x))))]
                  (reduce reducer (reducer p q) more))))

(defn my-map [f & seqs]
  (if (empty? (first seqs))
    []
    (let [f-on-seqs (fn [f seqs] (reduce (fn [acc elem] (conj acc (f elem))) [] seqs))]
      (cons (apply f (f-on-seqs first seqs)) (apply my-map f (f-on-seqs rest seqs))))))

