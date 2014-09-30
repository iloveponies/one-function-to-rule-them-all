(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (rest(reduce (fn [s1 s2]
                 (conj s1 x s2))
               []
               a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count s1]
                  (if (nil? s1)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [s1 s2]
            (cons s2 s1))
          () a-seq))

(defn min-max-element [a-seq]
  (conj (vector (reduce min a-seq)) (reduce max a-seq)))

(defn insert [sorted-seq n]
  (sort (conj sorted-seq n)))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

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
  ([] 0)
  ([x] 1)
  ([x y & more]
    (+ 2 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y & more]
    (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [z] (and (x z) (y z))))
  ([x y & more]
   (fn [z] (reduce (fn [p1 p2]
                     (and p1 (p2 z)))
                   (and (x z) (y z))
                   more))))

(defn my-map
  ([f a-seq] (reverse (reduce (fn [x y] (cons (f y) x)) () a-seq)))
  ([f a-seq b-seq]
   (let [mymap (fn [s1 s2]
                 (loop [seq-3 ()
                        seq-1 s1
                        seq-2 s2]
                        (cond
                         (or (empty? seq-1)(empty? seq-2)) (reverse seq-3)
                         :else (recur (cons (f (first seq-1) (first seq-2)) seq-3)
                                      (rest seq-1)
                                      (rest seq-2)))))]
   (mymap a-seq b-seq)))
  ([f a-seq b-seq & more]
   (let [helpr (fn [x y]
                 (loop [seq-3 ()
                        seq-1 x
                        seq-2 y]
                        (cond
                         (or (empty? seq-1)(empty? seq-2)) (reverse seq-3)
                         :else (recur (cons (apply f (flatten (cons (first seq-1) [(first seq-2)]))) seq-3)
                                      (rest seq-1)
                                      (rest seq-2)))))]
   (reduce helpr (my-map f a-seq b-seq) more))))


