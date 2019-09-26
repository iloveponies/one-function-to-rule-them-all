(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [fcn (fn [sx sy] (apply str sx " " sy))]
      (reduce fcn a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [fcn (fn [acc y] (conj acc x y))]
      (seq (reduce fcn (vector (first a-seq)) (rest a-seq))))))

(defn my-count [a-seq]
  (reduce (fn [x _] (+ x 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (if (empty? a-seq)
    ()
    (let [fcn (fn [acc x] (cons x acc))]
      (reduce fcn () a-seq))))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    []
    (let [f   (first a-seq)
          fcn (fn [acc x]
                (let [[mini maxi] acc]
                  [(min mini x) (max maxi x)]))]
      (reduce fcn [f f] (rest a-seq)))))


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n ())
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq) (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [fcn (fn [x y] (if (contains? x y)
                        (disj x y)
                        (conj x y)))]
    (reduce fcn #{} a-seq)))

(defn minus
  ([x]   (* x -1))
  ([x y] (- x  y)))


(defn count-params [& x]
  (let [fcn (fn [acc _] (inc acc))]
    (reduce fcn 0 x)))

(defn my-* [& x]
  (reduce * 1 x))

(defn pred-and [& args]
  (let [base (fn [x] true)
        fcn  (fn [accp p] (fn [x] (and (accp x)
                                       (p x))))]
    (reduce fcn base args)))

(defn my-helper [f seqs]
  (if (not-any? empty? seqs)
    (let [heads (map first seqs)
          tails (map rest  seqs)]
      (cons (apply f heads) (my-helper f tails)))
    ()))

(defn my-map [f & seqs]
  (if (empty? seqs)
    ()
    (my-helper f seqs)))





