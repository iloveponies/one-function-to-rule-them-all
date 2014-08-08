(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce (fn ([] "") ([a] a) ([a b] (str a " " b))) a-seq))

(defn my-interpose [x a-seq]
  (drop 1 (reduce (fn [acc val] (conj acc x val)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [[x & xs]]
  (reduce (fn [[min max] x] (cond (< x min) [x max] (> x max) [min x] :else [min max])) [x x] xs))

(defn insert [sorted-seq n]
  (loop [less [] [x & xs :as coll] sorted-seq]
    (cond
      (empty? coll) (conj less n)
      (< n x) (concat less [n] coll)
      :else (recur (conj less x) xs))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn parity [a-seq]
  (reduce (fn [s x] ((if (contains? s x) disj conj) s x)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (constantly true))
  ([p] p)
  ([p1 p2] #(and (p1 %) (p2 %)))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))


(defn accumulate [acc acc-fn item-fn coll]
  (reduce #(acc-fn %1 (item-fn %2)) acc coll))

(def heads (partial accumulate [] conj first))

(def tails (partial accumulate [] conj rest))

(defn my-map [f & seqs]
  (if (some empty? seqs)
    ()
    (cons (apply f (heads seqs)) (apply my-map f (tails seqs)))))
