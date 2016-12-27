(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) '()
    (= 1 (count a-seq)) (seq [(first a-seq)])
    :else (seq (reduce (fn [a b]
                    (conj
                      (if (and (vector? a) (= 1 (count a)))
                        (conj
                            (vector a)
                            x)
                        (conj
                            (if (vector? a) a [a])
                            x))
                          b))
                  a-seq))))

(defn my-count [a-seq]
  (cond
    (empty? a-seq) 0
    :else (let [counter (fn [count a]
                          (inc count))]
            (reduce counter 0 a-seq))))

(defn my-reverse [a-seq]
  (let [lifter (fn [rev-set a]
                 (cons a rev-set))]
    (reduce lifter '() a-seq)))

(defn min-max-element [a-seq]
  (let [mima (fn [extremes a]
               (cond
                 (< a (get extremes 0)) (assoc extremes 0 a)
                 (> a (get extremes 1)) (assoc extremes 1 a)
                 :else extremes))]
    (reduce mima [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (seq [n])
    (let [lesser (fn [a] (if (< a n)
                           true
                           false))
          start (take-while lesser sorted-seq)
          end (take-last (- (count sorted-seq) (count start)) sorted-seq)]
    (concat start [n] end))))

(defn insertion-sort [a-seq]
  (cond
    (empty? a-seq) '()
    :else (reduce insert '() a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [odds a]
                 (if (contains? odds a)
                   (disj odds a)
                   (conj odds a)))]
    (cond
      (empty? a-seq) nil
      :else (reduce toggle #{} a-seq))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (reduce * 1 more))

(defn pred-and [& more]
  (cond
    (= 0 (count more)) (fn [more] true)
    (= 1 (count more)) (first more)
    (= 2 (count more)) (fn [x] (and ((first more) x) ((second more) x)))
    ;:else (fn [x] (reduce #(and %1 %2) more))))
    :else (fn [x] (every-pred more))))


(defn my-map [f a-seq]
  [:-])
