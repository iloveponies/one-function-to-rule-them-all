(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce str (interpose " " a-seq)))

(defn my-interpose [x a-seq]
  (let [combine (fn [acc elem]
                  (if (empty? acc)
                    (conj acc elem)
                    (conj acc x elem)))]
    (reduce combine [] a-seq)))

(defn my-count [a-seq]
  (let [helper (fn [init elem]
                 (inc init))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [helper (fn [acc elem]
                 (if (empty? acc)
                   [elem elem]
                   [(min elem (first acc)) (max elem (second acc))]))]
    (reduce helper [] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (loop [res []
           seq1 sorted-seq]
      (if (empty? seq1)
        (conj res n)
        (if (<= n (first seq1))
          (concat res [n (first seq1)] (rest seq1))
          (recur (conj res (first seq1)) (rest seq1)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [acc elem] (if (contains? acc elem)
                                (disj acc elem)
                                (conj acc elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (* x -1))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (fn [val] (every? true? (map (fn [pred] (pred val)) more)))))

(defn my-map [f a-seq]
  [:-])