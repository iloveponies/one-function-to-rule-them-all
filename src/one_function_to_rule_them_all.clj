(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [merged s] (str merged " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '[]
    (concat
      [(first a-seq)]
      (reduce
        (fn [elements e] (conj elements x e))
        '[]
        (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc n] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [reversed-seq e] (conj reversed-seq e)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[small, big] element]
            [(min (or small element) element)
             (max (or big element) element)]) '[] a-seq))

(defn insert [sorted-seq n]
  (loop [remaining sorted-seq
         processed '[]]
    (let [first-element (first remaining)]
      (cond
        (empty? remaining)
          (concat processed [n])
        (>= first-element n)
          (concat processed [n first-element] (rest remaining))
        :else
          (recur (rest remaining) (conj processed first-element))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (reduce (fn [appeared-once, element]
            (if (contains? appeared-once element)
              (disj appeared-once element)
              (conj appeared-once element))) #{} a-seq))

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
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(def any?
  (comp boolean some))

(defn my-map
  ([f a-seq]
   (loop [a-rest a-seq
          mapped '[]]
     (if (empty? a-rest)
       mapped
       (recur (rest a-rest) (conj mapped (f (first a-rest)))))))
  ([f a-seq b-seq]
   (loop [a-rest a-seq
          b-rest b-seq
          mapped '()]
     (if (or (empty? a-rest) (empty? b-rest))
       mapped
       (recur (rest a-rest) (rest b-rest) (conj mapped (f (first a-rest) (first b-rest)))))))
  ([f a-seq b-seq & more]
   (let [groups
         (loop [sequences (concat [a-seq] [b-seq] more)
                mapped '()]
           (if (any? empty? sequences)
             mapped
             (recur (my-map rest sequences) (conj mapped (my-map first sequences)))))]
     (loop [groups-rest groups
            mapped '()]
       (if (empty? groups-rest)
         mapped
          (recur (rest groups-rest) (conj mapped (apply f (first groups-rest)))))))))