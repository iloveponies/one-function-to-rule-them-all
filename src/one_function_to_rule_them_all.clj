(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (let [strcat (fn [a b]
                (str a (str " " b)))]
  (if (empty? a-seq)
    ""
    (reduce strcat (cons (first a-seq) (rest a-seq))))))


(defn my-interpose [x a-seq]
  (let [posify (fn [a b]
                 (conj a x b))]
    (if (empty? a-seq)
      []
    (reduce posify [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [counta b]
                  (if (nil? b)
                     counta
                    (inc counta)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [reversed b]
                   (if (nil? b)
                     reversed
                     (cons b reversed)))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  (let [minmaxer (fn [listed b]
                    ;(cond
                    ;(nil? b) listed
                    ;(empty? listed) (conj listed b b)
                    ;(< b (get listed 0)) (conj listed (reverse (conj (drop 1 listed) b))
                    ;(> b (get listed 1)) (conj (drop-last 1 listed) b)
                   ; ))]

                      (if (nil? b)
                        [(get listed :min) (get listed :max)]
                        (if (empty? listed)
                          (assoc listed :min b :max b)
                          (if (< b (get listed :min))
                            (assoc listed :min b)
                            (if (> b (get listed :max))
                              (assoc listed :max b)
                              '()))))
                    )]
    (reduce minmaxer {} a-seq)))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) '()
    (and (< (first sorted-seq) n) (> (first (rest sorted-seq)) n)) (cons (list (first sorted-seq n)) (rest sorted-seq))
    :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  (let [pariter (fn [lis elem]
                  (cond
                    (nil? (get lis elem)) (conj lis elem)
                    (not (nil? (get lis elem))) (disj lis elem)))]
    (reduce pariter #{} a-seq)))

(defn minus ([x] (- x))
  ([x y] (- x y)))


(defn count-params
  ([] 0 )
  ([x & more] (+ (count more) 1)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
