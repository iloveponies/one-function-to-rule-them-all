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


(defn new-vector [pos old-vectors]
  (let [helper (fn [new-vector token]
                 (cond
                  (nil? token) new-vector
                  :else (cons (token pos) new-vector))
                  )]
  (reduce helper [] old-vectors)))


(defn my-map
  ([f & more]
    (loop [pos 0
           limit (count (first more))
           in-param more
           new-seq []]
      (cond
       (>= pos limit) (reverse new-seq)
       :else (recur (inc pos) limit in-param (cons (apply f (new-vector pos in-param)) new-seq))))))
