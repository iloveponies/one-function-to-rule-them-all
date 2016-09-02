(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (clojure.string/join " " a-seq))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [a b] (conj a x b)) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [acc e]
                    (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [lst e]
                    (concat [e] lst))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  [(apply min a-seq) (apply max a-seq)])

(defn insert [sorted-seq n]
  (apply concat [(filter (fn [e] (< e n)) sorted-seq) [n] (filter (fn [e] (> e n)) sorted-seq)]))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn count-elem [elem a-seq]
  (let [counter (fn [count e]
                  (if (= e elem)
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn parity [a-seq]
  (set (filter (fn [e] (== 1 (mod (count-elem e a-seq) 2))) a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params [& x]
  (my-count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [& x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and  (pred-and  p1 p2) more)))

(defn my-map [f a-seq]
  [:-])
