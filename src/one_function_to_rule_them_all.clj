(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce str "" (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (cons (first a-seq)
          (reduce (fn [initial item]
                      (conj (conj initial x) item))
                  []
                 (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (nil? e)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [reversed e]
                   (if (nil? e)
                     reversed
                     (cons e reversed)))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [min-maxer (fn [min-max e]
                      (let [fmm (first min-max)
                            smm (second min-max)]
                        (cond
                         (nil? e) min-max
                         (< e fmm) [e smm]
                         (> e smm) [fmm e]
                         :else min-max)))]
      (reduce min-maxer (vector (first a-seq) (first a-seq)) a-seq))))

(defn insert [sorted-seq n]
  (let [front (take-while (fn [x] (< x n)) sorted-seq)
        back (drop-while (fn [x] (<= x n)) sorted-seq)]
    (concat front [n] back)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggleElem [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggleElem #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (fn [x] (and (pred1 x) (pred2 x))) more)))

(defn my-map
  ([f & seqs]
   (if (> (count seqs) 1)
     (let [paramSeqs (partition (count seqs) (apply interleave seqs))]
       (reduce (fn [res xs] (conj res (apply f xs))) [] paramSeqs))
     (reduce (fn [res x] (conj res (f x))) [] (first seqs)))))
