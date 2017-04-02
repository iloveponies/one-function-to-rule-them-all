(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (let [interpose-space (fn [x y]
                          (apply str (concat x " " y)))]
    (if (empty? a-seq)
      ""
      (reduce interpose-space (first a-seq) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (let [interpose-x (fn [y z]
                      (conj (conj y x) z))]
    (if (empty? a-seq)
      []
      (reduce interpose-x [(first a-seq)] (rest a-seq)))))


(defn my-count [a-seq]
  (let [seq-counter (fn [count elem]
                      (if (nil? elem)
                        count
                        (inc count)))]
    (reduce seq-counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [prev nextelem]
                   (cons nextelem prev))]
    (if (empty? a-seq)
      a-seq
      (reduce reverser [(first a-seq)] (rest a-seq)))))

(defn min-max-element [a-seq]
  (let [min-max (fn [[curr-min curr-max] elem]
                  (cond (nil? curr-min) [elem elem]
                        (< curr-max elem) [curr-min elem]
                        (< elem curr-min) [elem curr-max]
                        :else [curr-min curr-max]))]
  (reduce min-max [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (let [find-index (fn [sorted-seq n curr-index]
                     (cond (empty? sorted-seq) curr-index
                           (nil? (get sorted-seq curr-index)) curr-index
                           (< n (get sorted-seq curr-index)) curr-index
                           :else (recur sorted-seq n (inc curr-index))))
        index (find-index sorted-seq n 0)]
    (if (empty? sorted-seq)
      [n]
      (concat (concat (take index sorted-seq) [n]) (drop index sorted-seq)))))

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
