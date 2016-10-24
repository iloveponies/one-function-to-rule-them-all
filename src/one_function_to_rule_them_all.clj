(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
   (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (let [helper (fn [acc e]
                (if (empty? acc)
                 (conj acc e)
                 (conj acc x e)))]
    (reduce helper [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc b] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc n]
            (cond
              (< n (first acc))  [n (second acc)]
              (> n (second acc)) [(first acc) n]
              :else acc))
          [(first a-seq) (first a-seq)]
          a-seq))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) [n]
    (<= n (first sorted-seq)) (cons n sorted-seq)
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [acc x] (if (contains? acc x) (disj acc x) (conj acc x))) #{} a-seq))

(defn minus
  ([x]   (* -1 x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([]    1)
  ([x]   x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([]      (fn [x] true))
  ([p]     (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f & seqs]
  (loop [acc []
         remaining seqs]
    (if (empty? (first remaining))
      acc
      ; Need to apply f to elements in same position. All args are of same length.
      ; e.g. for + [1 1 1] [1 1 1] [1 1 1]
      ; 0: acc: [],      remaining: [1 1 1] [1 1 1] [1 1 1]
      ; 1: acc: [3],     remaining: [1 1] [1 1] [1 1]
      ; 2: acc: [3 3],   remaining: [1] [1] [1]
      ; 3: acc: [3 3 3], remaining: [] [] []
      (recur
        (conj acc (apply f (map first remaining)))
        (map rest remaining)))))
