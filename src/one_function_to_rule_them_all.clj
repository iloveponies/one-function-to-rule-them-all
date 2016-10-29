(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x1 x2] (str x1 " " x2)) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) ()
    (= (count a-seq) 1) (seq a-seq)
    :else (reverse (reduce (fn [x1 x2] (if (seq? x1) (conj x1 x x2) (seq [x2 x x1]))) a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [i s] (inc i))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [x1 x2] (cons x2 x1)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x1 x2]
            (cond
              (nil? x2) x1
              (< x2 (first x1)) (assoc x1 0 x2)
              (> x2 (last x1)) (assoc x1 1 x2)
              :else x1
            )
          ) (vector (first a-seq) (first a-seq)) a-seq))

(defn insert [sorted-seq n]
  (cond
    (or (empty? sorted-seq))
      (conj () n)
    (< n (first sorted-seq))
      (cons n sorted-seq)
    (< (last sorted-seq) n)
      (seq (conj sorted-seq n))
    :else
      (loop [i 1]
        (let [pre (take i sorted-seq)
              post (drop i sorted-seq)]
        (if (< (last pre) n (first post))
          (concat pre (conj post n))
          (recur (inc i)))))))

(defn insertion-sort [a-seq]
  (reduce (fn [x1 x2] (insert x1 x2)) [] (reverse (sort a-seq))))

(defn parity [a-seq]
  (let [toggle (fn [b-seq elem] (if (some #{elem} b-seq) (disj b-seq elem) (conj b-seq elem)))]
    (if (empty? a-seq)
      #{}
      (reduce toggle #{} a-seq))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (if (empty? more)
    1
    (reduce * more)))

(defn pred-and [& more]
  (cond
    (empty? more) (fn [x] (boolean true))
    (= (count more) 1) (fn [x] ((first more) x))
    (= (count more) 2) (fn [x] (and ((first more) x) ((last more) x)))
    :else (fn [x]
            (reduce (fn [retval, pred] (and retval (pred x))) more))))

(defn my-map [f & more]
  (let [my-map-helper (fn [a-seq]
                        (seq (reduce (fn [retval e] (conj retval (f e))) #{} a-seq)))]
    (cond
      (empty? more) nil
      (= 1 (count more)) (my-map-helper (first more))
      :else (seq (reduce my-map-helper #{} more)))))

