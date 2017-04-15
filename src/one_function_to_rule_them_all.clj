(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [fst snd] (str fst " " snd)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (let [init (cons (first a-seq) '())
          rst (rest a-seq)
          interposer (fn [init-seq elem]
                      (seq (conj (vec init-seq) x elem)))]
     (reduce interposer
             init
             rst))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [init elem]
            (conj init elem))
          '()
          a-seq))

(defn min-max-element [a-seq]
  (let [checker (fn [min-max elem]
                  (cond
                   (< elem (get min-max 0)) (assoc min-max 0 elem)
                   (< (get min-max 1) elem) (assoc min-max 1 elem)
                   :else min-max))]
    (reduce checker [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [smaller-than-n? (fn [x]
                         (< x n))
        smaller (take-while smaller-than-n? sorted-seq)
        rest (drop-while smaller-than-n? sorted-seq)]
    (concat smaller (cons n rest))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [init #{}
        toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                  (disj a-set elem)
                  (conj a-set elem)))]
    (reduce toggle init a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map [f & more]
  (let [first-elements (reduce
                        (fn [init elem]
                          (reverse (cons (first elem)
                                         (reverse init))))
                        '()
                        more)
        rest-of-elements (reduce
                          (fn [init elem]
                            (reverse (cons (rest elem)
                                           (reverse init))))
                          '()
                          more)]
    (if (some empty? more)
      '()
      (cons (apply f first-elements) (apply my-map (cons f rest-of-elements))))))










