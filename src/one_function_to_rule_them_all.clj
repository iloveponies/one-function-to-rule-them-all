(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [string token]
                 (str string " " token))]
    (if (empty? a-seq)
      ""
      (reduce helper a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [seq token]
                 (if (empty? seq)
                    (conj seq token)
                    (conj seq x token)))]
    (if (empty? a-seq)
      ()
      (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [helper (fn [cnt token]
        (if (nil? token)
          cnt
          (inc cnt)
          ))]
    (reduce helper 0 a-seq)
    ))

(defn my-reverse [a-seq]
  (let [helper (fn [seq token]
                 (if (nil? token)
                   seq
                   (cons token seq)
                 ))]
    (reduce helper [] a-seq)))


(defn min-max-element [a-seq]
  (let [helper (fn [minmax value]
                    (cond
                     (nil? value) minmax
                     (empty? minmax) [value value]
                     (> value (second minmax)) [(first minmax) value]
                     (< value (first minmax)) [value (second minmax)]
                     :else minmax
                     ))]
    (reduce helper [] a-seq)))


(defn insert [sorted-seq n]
  (loop [inserted 0
         new-seq ()
         seq sorted-seq]
    (cond
     (and (empty? seq) (== inserted 1)) (reverse new-seq)
     (and (empty? seq) (== inserted 0)) (recur 1 (cons n new-seq) seq)
     (and (< n (first seq)) (== inserted 0)) (recur 1 (cons n new-seq) seq)
     :else (recur inserted (cons (first seq) new-seq) (rest seq)))))

(defn insertion-sort [a-seq]
  (if (empty? a-seq) []
    (reduce insert [] a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [& more]
  (let [helper (fn [cnt token]
                 (if (nil? token)
                   cnt
                   (inc cnt)))]
    (reduce helper 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [n] true))
  ([pred] (fn [n] (pred n) ))
  ([pred1 pred2] (fn [n] (and (pred1 n)
                              (pred2 n))))
  ([pred1 pred2 & more] (fn [n]
                            (let [helper (fn [result pred]
                                           (cond
                                            (= result false) false
                                            (pred n) true
                                            :else false
                                            ))]
                             (reduce
                              helper
                              (and (pred1 n) (pred2 n))
                              more)))))


(defn my-map
  ([f a-seq]
              (loop [out-seq ()
                     in-seq a-seq]
                (if (empty? in-seq)
                  (reverse out-seq)
                  (recur (cons (f (first in-seq)) out-seq) (rest in-seq))
                  )))
  ([f a-seq & more]
   (let [helper (fn [result seq]
                        (loop [output ()
                               data result
                               input seq]
                          (cond
                           (empty? input) output
                           :else (recur (cons (f (first data) (first input)) output)
                                        (rest data)
                                        (rest input)))))]
     (reduce helper (my-map f a-seq) more))))
