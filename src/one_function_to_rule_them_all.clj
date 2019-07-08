(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [word1 word2]
              (str word1 " " word2))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [acc elem]
              (conj acc x elem))
            (conj [] (first a-seq))
            (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [accumulator element] (+ accumulator 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [accumulator element] (conj accumulator element)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[acc-min acc-max] element]
            (vector (min acc-min element)
                    (max acc-max element)))
          (vector (first a-seq) (first a-seq))
          (rest a-seq)))

(defn insert [sorted-seq n]
  (let [helper (loop [counter 0]
                 (if (or (= counter (count sorted-seq))
                         (< n (nth sorted-seq counter)))
                   counter
                   (recur (inc counter))))]
    (concat (take helper sorted-seq)
            (conj (drop helper sorted-seq) n))))

(defn insertion-sort [a-seq]
  (let [insertion (fn [sorted-seq
                       elem]
                    (insert sorted-seq elem))]
    (reduce insertion [] a-seq)))

(defn parity [a-seq]
  (let [seq-frequencies (frequencies a-seq)
        odd-keys (fn [odd-seq elem]
                    (if (odd? (val elem))
                      (conj odd-seq (key elem))))]
    (set (reduce odd-keys [] seq-frequencies))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& more]
  (my-count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and
  ([] (fn [x] true))
  ([pred1?] (pred1?))
  ([pred1? pred2?] (fn [x] (and (pred1? x)
                                (pred2? x))))
  ([pred1? pred2? & more] (reduce pred-and (pred-and pred1? pred2?) more)))

(defn my-map
  ([f a-seq]
   (reduce (fn [processed elem]
             (conj processed (f elem)))
           []
           a-seq))
  ([f a-seq & rest-seqs]
   (let [seqs (concat [a-seq] rest-seqs)]
     (reduce (fn [processed index]
               (conj processed
                     (apply f (my-map (fn [elem] (nth elem index)) seqs))))
             []
             (range (apply min (my-map count seqs)))))))
