(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str % " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [interp (fn [base elem]
                   (if (empty? base)
                     (conj base elem)
                     (conj base x elem)))]
      (seq (reduce interp [] a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [count _] (+ count 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %) () a-seq))

(defn min-max-element [a-seq]
  (let [d (first a-seq)
        min-max (fn [[low high] elem]
                  [(min low elem) (max high elem)])]
    (reduce min-max [d d] a-seq)))

(defn insert [sorted-seq n]
  (loop [ans ()
         sorted-seq sorted-seq]
    (cond (empty? sorted-seq)
          (reverse (cons n ans))
          (>= (first sorted-seq) n)
          (concat (reverse (cons n ans)) sorted-seq)
          :else
          (recur (cons (first sorted-seq) ans) (rest sorted-seq)))))

(defn insertion-sort [a-seq]
  (reduce #(insert % %2) [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (fn [x] ((reduce pred-and (pred-and p q) more) x))))

(defn my-map [f & more]
  (let [my-map' (fn [f a-seq] (let [f (fn [res elem] (cons (f elem) res))]
                               (reverse (reduce f () a-seq))))
        firsts (fn [seqs] (my-map' first seqs))
        rests (fn [seqs] (my-map' rest seqs))
        apply' (fn [f lst] (eval (cons f lst)))]
    (loop [fst (firsts more)
           rst (rests more)
           acc ()]
      (if (nil? (first fst))
        (reverse acc)
        (recur (firsts rst) (rests rst) (cons (apply' f fst) acc))))))
