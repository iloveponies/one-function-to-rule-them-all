(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [str-set (fn [x y] (str x " " y))]
  (if (empty? a-seq)
    ""
    (reduce str-set a-seq))))

(defn my-interpose [x a-seq]
  (let [new-list []
        set-interpose (fn [y z] (conj (vec y) x z))]
    (if (empty? a-seq)
        ()
      (lazy-seq (reduce set-interpose (vector (first a-seq)) (rest a-seq))))))

(defn my-count [a-seq]
  (let [counter (fn [n useless] (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [x y] (cons y x))]
    (if (empty? a-seq)
      ()
    (reduce helper (list (first a-seq)) (rest a-seq)))))

(defn min-max-element [a-seq]
  (let [begin (first a-seq)
        helper (fn [x y]
                 (cond
                  (< y (get x 0))
                    (assoc x 0 y)
                  (> y (get x 1))
                     (assoc x 1 y)
                  :else x))]
    (reduce helper (vector begin begin) a-seq)))

(defn insert [sorted-seq n]
  (loop [begin []
         others sorted-seq]
    (cond
     (empty? others)
       (lazy-seq (conj begin n))
      (< n (first others))
         (concat (conj begin n) others)
     :else
       (recur (conj begin (first others))
              (rest others)))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & y] (reduce * x y)))

(defn pred-and
  ([] (constantly true))
  ([x] x)
  ([x y] (fn [j] (and (x j) (y j))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f a-seq]
   (loop [new []
          original a-seq]
     (if (empty? original)
       (lazy-seq new)
       (recur (conj new (f (first original))) (rest original)))))
  ([f a-seq & more]
   (reverse (loop [new '()
          original (cons a-seq more)]
     (if (empty? (first original))
       new
       (recur (cons (apply f (my-map first original)) new) (my-map rest original)))))))

(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
