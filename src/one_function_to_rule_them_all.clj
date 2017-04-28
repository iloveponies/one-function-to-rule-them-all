(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [nice-fun (fn [c s]
                   (if (= nil s)
                     c
                     (inc c)))]
    (reduce nice-fun 0 a-seq)))

(defn my-reverse [a-seq]
  (let [nice-fun (fn [c s]
                   (cons s c))]
    (reduce nice-fun [] a-seq)))

(defn min-max-element [a-seq]
  (if (> (count a-seq) 1)
    (let [eka (first a-seq)
          toka (first (rest a-seq))
          minmax [eka toka]
          nice-fun (fn [mm elem]
                     (if (< elem (get mm 0))
                       (assoc mm 0 elem)
                       (if (> elem (get mm 1))
                         (assoc mm 1 elem)
                         mm)))]
      (reduce nice-fun minmax a-seq))
    (if (== (count a-seq) 1)
      [(first a-seq) (first a-seq)]
      [])))

(defn insert [sorted-seq n]
  (loop [ind 0
         loppu sorted-seq
         alku []]
    (if (empty? loppu)
      (conj sorted-seq n)
      (if (>= (first loppu) n)
        (apply conj alku n loppu)
        (recur (inc ind) (rest loppu) (conj alku (first loppu)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * more))

(defn pred-and [& jea]
  (if (== (count jea) 0)
    (fn [x] true)
    (fn [x] (loop [funs jea
                   fun (fn [x] true)]
              (if (fun x)
                  (if (empty? funs)
                    true
                    (recur (rest funs) (first funs)))
                  false)))))

(defn my-map [f a-seq]
  [:-])
