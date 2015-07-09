(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [combine (fn [tulos teksti]
                    (if (empty? tulos)
                      teksti
                      (str tulos " " teksti)))]
      (reduce combine a-seq))))

(defn my-interpose [x a-seq]
  (let [jelp (fn [tulos uusi]
               (if (empty? tulos)
                 (conj tulos uusi)
                 (conj tulos x uusi)))]
    (reduce jelp [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [sofar s]
                  (if (nil? s)
                    sofar
                    (inc sofar)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [pakki (fn [tulos alkup]
                (if (nil? alkup)
                  tulos
                  (conj tulos alkup)))]
    (reduce pakki '() a-seq)))

(defn min-max-element [a-seq]
  (let [etsi-rajat (fn [rajat numero]
                     (cond (empty? a-seq) []
                           (nil? numero) rajat
                           (and (> (first rajat) numero) (< (last rajat) numero)) [numero numero]
                           (> (first rajat) numero) [numero (last rajat)]
                           (< (last rajat) numero) [(first rajat) numero]
                           :else rajat))]
    (reduce etsi-rajat [Integer/MAX_VALUE Integer/MIN_VALUE] a-seq)))

(defn insert [sorted-seq n]
  (loop [result []
         sorted sorted-seq]
    (cond (empty? sorted) (conj result n)
          (and (or (empty? result) (< (last result) n)) (< n (first sorted))) (concat (conj result n) sorted)
          :else (recur (conj result (first sorted)) (rest sorted)))))

(defn insertion-sort [a-seq]
  (let [i-sort (fn [result number]
               (if (nil? number)
                 result
                 (insert result number)))]
    (reduce i-sort [] a-seq)))

(defn parity [a-seq]
  (let [hjelp (fn [result piece]
                (cond (empty? a-seq) result
                      (nil? piece) result
                      (contains? result piece) (disj result piece)
                      :else (conj result piece)))]
    (reduce hjelp #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  ([f a-seq] (loop [s a-seq
                    result []]
               (if (empty? s)
                 result
                 (recur (rest s) (conj result (f (first s)))))))
  ([f a-seq b-seq] (loop [aseq a-seq
                          bseq b-seq
                          result ()]
                     (if (or (empty? aseq) (empty? bseq))
                       result
                       (if (coll? (first aseq))
                         (recur (rest aseq) (rest bseq) (concat result (list (apply f (first bseq) (first aseq)))))
                         (recur (rest aseq) (rest bseq) (concat result (list (f (first aseq) (first bseq)))))))))
  ([f a-seq b-seq & more] (reduce (fn [aseq bseq] (my-map f aseq bseq))
                                  (my-map f a-seq b-seq)
                                  more)))
