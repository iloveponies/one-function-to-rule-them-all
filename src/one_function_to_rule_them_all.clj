(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (reduce
    (fn [acc elem]
      (if (empty? elem)
        acc
        (str acc " " elem)))
    (str (first a-seq))
    (rest a-seq)))

(defn my-interpose [x a-seq]
  (reduce
    (fn [acc elem]
      (if (nil? elem)
        acc
        (if (empty? acc)
          (conj acc elem)
          (conj (conj acc x) elem))))
    []
    a-seq))

(defn my-count [a-seq]
  (reduce (fn [acc elem] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc elem] (cons elem acc)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce
    (fn [acc elem]
      [(min (first acc) elem)
       (max (second acc) elem)])
    [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (seq [n])
    (< n (first sorted-seq)) (cons n sorted-seq)
    (> n (last sorted-seq)) (concat sorted-seq [n])
    :else (loop [head (seq [(first sorted-seq)])
                 tail (rest sorted-seq)]
            (if (< (last head) n (first tail))
              (concat (concat head [n]) tail)
              (recur (concat head [(first tail)]) (rest tail))))))

(defn insertion-sort [a-seq]
  (reduce insert (seq []) a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and
  ([& more]
   (fn [x]
     (reduce
       (fn [current nxt]
         (and
           current
           (nxt x)))
       true
       more))))

(defn apply-map
  [f a-seq]
   (if (empty? (flatten a-seq))
     '()
     (cons
       (apply f (map first a-seq))
       (apply-map f (map rest a-seq)))))

(defn my-map
  [f & more]
   (apply-map f (seq more)))
