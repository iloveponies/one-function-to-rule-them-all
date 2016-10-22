(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [myconcat (fn [newstr some-str]
                     (str newstr " " some-str))]
    (if (empty? a-seq)
      ""
      (reduce myconcat (first a-seq) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (let [i-pose (fn [new-seq elem]
                 (conj new-seq x elem))]
    (if (empty? a-seq)
      ()
      (reduce i-pose [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [length-count (fn [counter elem]
                       (inc counter))]
    (reduce length-count 0 a-seq)))

(defn my-reverse [a-seq]
  (let [my-con (fn [new-seq elem]
                 (cons elem new-seq))]
      (reduce my-con () a-seq)))

(defn min-max-element [a-seq]
  (let [find (fn [min-max elem]
               (if (< elem (first min-max))
                 (assoc min-max 0 elem)
                 (if (> elem (second min-max))
                   (assoc min-max 1 elem)
                   min-max)))]
    (reduce find [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (loop [new-seq []
         rest-a-seq sorted-seq]
    (if (empty? rest-a-seq)
      (conj new-seq n)
      (if (> (first rest-a-seq) n)
        (concat (conj new-seq n) rest-a-seq)
        (recur (conj new-seq (first rest-a-seq)) (rest rest-a-seq))))))

(defn insertion-sort [a-seq]
  (let [insert-one (fn [s-seq elem]
                     (insert s-seq elem))]
    (reduce insert-one [] a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (let [one-parity (fn [parity-seq elem]
                     (toggle parity-seq elem))]
    (reduce one-parity #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& params]
  (let [product (fn [acc elem]
                  (* acc elem))]
    (reduce product 1 params)))

(defn pred-and
  ([] (fn [x] x))
  ([pred] (fn [x] (pred x)))
  ([pred & more] (fn [x]
                   (reduce (fn [a b]
                             (and a (b x))) (pred x) more))))

(defn my-map [f a-seq]
  [:-])
