(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    (sequence a-seq)
    (sequence (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [len _] (inc len)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [xs x] (conj xs x)) () a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [[mn mx] x]
                  [(min mn x) (max mx x)])
        fst (first a-seq)
        value (if (empty? a-seq) [] [fst fst])]
    (reduce min-max value (rest a-seq))))

(defn insert [sorted-seq n]
  (let [[s l] (split-with #(<= % n) sorted-seq)]
    (concat s (seq [n]) l)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (let [odd-freqs (filter #(odd? (val %)) (frequencies a-seq))]
    (set (keys odd-freqs))))

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & xs] (reduce my-* (my-* x y) xs)))

(defn pred-and
  ([] (fn [_] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & preds]
   (reduce pred-and (pred-and p1 p2) preds)))

(defn my-map [f & colls]
    (let [y (fn [res cs]
              (if (some empty? cs)
                res
                (let [reducer (fn [fun]
                                (reduce #(conj %1 (fun %2)) [] cs))
                      firsts (reducer first)
                      rests (reducer rest)]
                  (recur (conj res (apply f firsts)) rests))))]
      (y [] colls)))
