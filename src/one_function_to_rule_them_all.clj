(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (apply str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
  '()
  (rest (reduce #(conj %1 x %2) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (concat [b] a)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [accum elem]
            (let [num1 (get accum 0)
                  num2 (get accum 1)]
              (cond (< elem num1) [elem num2]
                    (> elem num2) [num1 elem]
                    :else accum))) [(first a-seq) (first a-seq)]
                                    (rest a-seq)))

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) [n]
        (> n (first sorted-seq)) (cons (first sorted-seq)
                                       (insert (rest sorted-seq) n))
        :else (cons n sorted-seq)))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce (fn [the-set the-elem]
            (if (contains? the-set the-elem)
              (disj the-set the-elem)
              (conj the-set the-elem))) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & others]
   (reduce my-* (my-* x y) others)))


(defn pred-and
  ([] (fn [& x] true))
  ([p] p)
  ([o p] (fn [x] (and (o x) (p x))))
  ([o p & others] (fn [x] (and (o x) (p x) ((apply pred-and others) x)))))


(defn my-map
  ([f] (f))
  ([f & others] (let
                  [g (fn [acc seq1] (if (empty? (first seq1)) acc
         (recur (cons (apply f (map first seq1))
                      acc) (map rest seq1))))]
                  (reverse (g [] others)))))
