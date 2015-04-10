(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  ;; Is this cheating?
  ;; (reduce str (interpose " " a-seq)))
  (if (empty? a-seq)
    ""
    (reduce (fn [acc next-str] (str acc " " next-str)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (cons (first a-seq)
          (reduce (fn [acc item] (conj (conj acc x) item)) [] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  ;; Feels like cheating again...
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [acc elem] [(min (first acc) elem) (max (second acc) elem)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [head []
         tail sorted-seq]
    (cond
      (empty? tail) (conj head n)
      (<= n (first tail)) (concat (conj head n) tail)
      :else (recur (conj head (first tail)) (rest tail)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
                                  (disj a-set elem)
                                  (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& a-seq] (count a-seq)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [_] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce pred-and (pred-and p q) more)))

(defn my-map [f & seqs]
  (loop [mapped []
         remaining-seqs seqs]
    (if (every? empty? remaining-seqs)
      mapped
      (let [split
            (reduce (fn [acc elem]
                      [(conj (first acc) (first elem))
                       (conj (second acc) (rest elem))])
                    [[] []]
                    remaining-seqs)]
        (recur (conj mapped (apply f (first split))) (second split))))))
