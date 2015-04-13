(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
      (reduce concat a-seq))

(defn str-cat [a-seq]
      (if (= (count a-seq) 0)
      (str "")
      (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
      (drop 1 (interleave (repeat x) a-seq)))

(defn my-count [a-seq]
      (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
      (reduce (fn [n m] (into [m] n)) [] a-seq))

(defn min-max-element [a-seq]
      (let [min-m
        (fn [[cmin cmax] x]
          [(min cmin x) (max cmax x)])
        f (first a-seq)]
    (reduce min-m [f f] (rest a-seq))))

(defn insert [sorted-seq n]
    (loop [b [] a sorted-seq]
    (let [next (first a)]
    (if (or (empty? a) (< n next))
        (concat (conj b n) a)
        (recur (conj b next) (rest a))))))

(defn insertion-sort [a-seq]
      (reduce insert [] a-seq))

(defn parity [a-seq]
      (let [fr (fn [frq x]
      (assoc frq x
      (if (contains? frq x)
      (inc (frq x)) 1)))]
      (set (keys (filter #(odd? (val %))
      (reduce fr {} a-seq))))))

(defn minus
     ([x] (- x))
     ([x y] (- x y)))

(defn count-params
     ([& x] (count x)))

(defn my-*
    ([] 1)
    ([x y] (* x y))
    ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
    ([] (fn [x] true))
    ([x] #(x %))
    ([x & more] #(reduce (fn [a b]
    (and a (b %))) (x %) more)))

(defn my-map [f & a-seq]
    (cond
    (some empty? a-seq)
      '()
    :else
      (cons (apply f (map first a-seq))
      (apply (partial my-map f) (map rest a-seq)))))
