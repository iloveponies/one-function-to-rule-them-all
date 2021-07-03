(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
    "Teht1"
  (reduce concat a-seq))

(defn str-cat [a-seq]
    "Teht2"
    (if (empty? a-seq)
        ""
      ;http://clojure.github.io/clojure/clojure.string-api.html#clojure.string/join
      (reduce #(clojure.string/join [%1 " " %2]) a-seq)))

(defn my-interpose [x a-seq]
    "Teht3"
  (interpose x a-seq))

(defn my-count [a-seq]
    "Teht4"
   (if (empty? a-seq) 0
    (+ 1 (my-count (rest a-seq)))))

(defn my-reverse [a-seq]
    "Teht5"
    (if (empty? a-seq)
      []
      (conj (my-reverse (rest a-seq)) (first a-seq) )))

(defn min-max-element [a-seq]
    "Teht6"
  (vector (apply min a-seq) (apply max a-seq)))

(defn insert [sorted-seq n]
    "Teht7"
    ;http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/split-with
    (let [[smaller-ones bigger-ones] (split-with #(< % n) sorted-seq)]
       (concat smaller-ones [n] bigger-ones)))

(defn insertion-sort [a-seq]
    "Teht7"
    (loop [list a-seq result '()]
      (if (empty? list) result
          (recur (rest list) (insert result (first list))))))

(defn parity [a-seq]
    "Teht8"
  (set
    ;http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/keep
   (keep
      (fn [[element occurences]]
          ;https://clojuredocs.org/clojure.core/when
          (when (odd? occurences) element))
    ;http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/frequencies
      (frequencies a-seq))))

(defn minus
    "Teht9"
    ([x] (- x))
    ([x y] (- x y)))

(defn count-params
    "Teht10"
    ([] 0)
    ([x] 1)
    ([x y] 2)
    ([x y & the-rest]
        (+ (count the-rest) 2)))

(defn my-*
    "Teht11"
    ([] 1)
    ([x] x)
    ([x y] (* x y))
    ([x y & the-rest]
        (* x y (reduce * the-rest))))

(defn pred-and
    "Teht12"
    ([] (fn [x] true))
    ([pred1]
        (fn [x] (pred1 x)))
    ([pred1 pred2]
        (fn [x] (and (pred1 x) (pred2 x))))
    ([pred1 pred2 & the-rest]
        (reduce pred-and (pred-and pred1 pred2) the-rest)))

(defn my-map [f a-seq]
    "Encore"
  [:-])
