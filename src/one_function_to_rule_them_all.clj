(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [prev cur] (str prev " " cur)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '()
    (reverse (conj (vec (reduce (fn [prev cur] (conj (conj prev x) cur))
                                '()
                                (rest a-seq)))
                   (first a-seq)))))

(defn my-count [a-seq]
    (reduce (fn [count elem] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [r-seq elem] (conj r-seq elem)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [mm a]
            [(min (first  mm) a)
             (max (second mm) a)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (loop [new-seq '() the-seq sorted-seq]
      (if (empty? the-seq) (seq (conj (vec new-seq) n)) ; All is looped through
        (if (<= n (first the-seq))
          (concat (conj (vec new-seq) n) the-seq) ; Found the place, insert here and return
          (recur  (conj (vec new-seq) (first the-seq)) (rest the-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [add-if-odd
        (fn [new-seq item]
          (if (odd? (second item))
            (conj (vec new-seq) (first item))
            new-seq))]
    (set (reduce add-if-odd #{} (frequencies a-seq)))))

(defn minus
  ([x]   (- x  ))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [sum x] (inc sum)) 0 more))

(defn my-*
  ([]  1)
  ([x] x)
  ([x & more] (* x (reduce (fn [mul i] (* mul i)) more))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p & more] (fn [x] (and (p x)
                           (every? true? (map (fn [y] (y x)) more))))))

(defn my-map
  ([f a-seq]
   (reduce (fn [new-seq a] (conj (vec new-seq) (f a))) '() a-seq))
  ([f a-seq & more]
   (seq (loop [new-seq '() seqs (conj more a-seq)]
          (if (every? empty? seqs) new-seq ; else
            (recur (conj
                    (vec new-seq)
                    (apply f (my-map first seqs)))
                   (my-map rest seqs)))))))

