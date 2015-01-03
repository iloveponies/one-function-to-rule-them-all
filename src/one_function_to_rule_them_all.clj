(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) a-seq
    (= 1 (count a-seq)) a-seq
    :else
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (cond
    (empty? a-seq) a-seq
    (= 1 (count a-seq)) a-seq
    :else
    (reduce #(conj %1 %2) (list (first a-seq)) (rest a-seq))))

(defn min-max-element [a-seq]
  (let [f (first a-seq)
        limiter (fn [limits e] 
                  (let [[mini maxi] limits]
                    (cond
                      (< e mini) [e maxi]
                      (> e maxi) [mini e]
                      :else [mini maxi])))]
    (reduce limiter [f f] a-seq)))

;; insert n at the right place in sorted-seq
(defn insert [sorted-seq n]
  (loop [so-far []
         sq sorted-seq]
    (let [[h & t] sq]
      (cond
        (empty? sq) (conj so-far n)
        (> h n) (concat so-far [n] sq)
        (= 1 (count sq)) (concat so-far [h n])
        :else 
        (recur (conj so-far h) t)))))

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
