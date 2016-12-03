(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [hlp (fn [a b]
                (str a " " b))]
      (reduce hlp a-seq)
    )))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (let [acc (conj [] (first a-seq))
          rs  (rest a-seq)
          hlp (fn [a b]
                (conj a x b))]
      (reduce hlp acc rs)
    )))


(defn my-count [a-seq]
  (let [counter (fn [acc e]
                  (inc acc))]
    (reduce counter 0 a-seq)
    ))

(defn my-reverse [a-seq]
  (let [bwd (fn [a b]
              (cons b a))]
    (reduce bwd '() a-seq)
    ))

(defn min-max-element [a-seq]
  (let [solo (first a-seq)
        rng [solo solo]
        adj  (fn [[l h] s]
               (cond
                 (< s l)
                 [s h]
                 (> s h)
                 [l s]
                 :else
                 [l h]))]
    (reduce adj rng a-seq)
    ))


(defn insert [sorted-seq n]
  (let [lower? (fn [el]
                 (< el n))]
    (concat (take-while lower? sorted-seq) (conj nil n) (drop-while lower? sorted-seq))
    ))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [tog (fn [odd el]
              (if (contains? odd el)
                (disj odd el)
                (conj odd el)))]
    (reduce tog #{} a-seq)
    ))

(defn minus ([x]
             (- 0 x))
            ([x y]
             (- x y))
  )

(defn count-params [& par]
  (reduce (fn [cnt _] (inc cnt)) 0 par))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more))
  )

(defn pred-and
  ([] (fn[x] x))
  ([pred1] (fn [x] (pred1 x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more))
  )

(defn my-map
  ([f a-seq]
   (loop [v []
          iseq a-seq]
     (if (empty? (rest iseq))
       (conj v (f (first iseq)))
       (recur (conj v (f (first iseq))) (rest iseq))
       )))
   ([f a-seq b-seq]
    (loop [r 1
           v []
           row (vector (first a-seq) (first b-seq))]
      (if (empty? row)
        v
        (recur (inc r)
               (conj v r)
               (vector (first (drop r a-seq)) (first (drop r b-seq)))
               ))))

  )


(vector (first []) (first []))


