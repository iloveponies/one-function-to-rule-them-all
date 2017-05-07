(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [w1 w2] (str w1 " " w2)) a-seq)))

(defn my-interpose [x a-seq]
  (let [reducer-fn (fn [current-accum-val next-item]
                        (if (empty? current-accum-val)
                        (conj current-accum-val next-item)
                        (conj (conj current-accum-val x) next-item)))]
  (reduce reducer-fn '[] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [current-accum-val _] (inc current-accum-val)) 0 a-seq))

(defn my-reverse [a-seq]
    (let [reducer-fn (fn [current-accum-val next-item]
                          (conj current-accum-val next-item))] ; conj adds to the front of a list
    (reduce reducer-fn '() a-seq)))

(defn min-max-element [a-seq]
    (let [reducer-fn (fn [current-accum-val next-item]
                          (let [[min-val max-val] current-accum-val]
                              (cond
                                (< next-item min-val max-val) (assoc current-accum-val 0 next-item) ; found new min
                                (< min-val next-item max-val) current-accum-val ; nothing to do here
                                :else (assoc current-accum-val 1 next-item)))) ; found new max
            first-item (first a-seq)
            second-item (first (rest a-seq))]
            (if (= 1 (count a-seq))
                (vector (first a-seq) (first a-seq))
                (if (< first-item second-item)
                    (reduce reducer-fn [first-item second-item] (rest (rest a-seq)))
                    (reduce reducer-fn [second-item first-item] (rest (rest a-seq)))))))


(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n '[])
    (if (< n (first sorted-seq))
        (concat [n] sorted-seq)
        (loop [top-bun (cons (first sorted-seq) '[])
           bottom-bun (rest sorted-seq)]
           (if (empty? bottom-bun)
               (concat top-bun [n]) ; no bottom bun, result is top-bun + n
               (if (< n (first bottom-bun))
                   (concat (concat top-bun [n]) bottom-bun) ; make the sandwich with n in the middle
                   (recur (concat top-bun [(first bottom-bun)]) (rest bottom-bun)))))))) ; move one from buttom bun to top bun

(defn insertion-sort [a-seq]
    (let [reducer-fn (fn [current-accum-val next-item]
                          (insert current-accum-val next-item))]
    (reduce reducer-fn '() a-seq)))

(defn parity [a-seq]
    (let [reducer-fn (fn [current-accum-val next-item]
                          (let [[item item-freq] next-item]
                              (if (odd? item-freq)
                                (conj current-accum-val item)
                                current-accum-val)))]
    (reduce reducer-fn #{} (frequencies a-seq))))

(defn minus
    ([x] (- 0 x))
    ([x y] (- x y)))

(defn count-params [& params]
    (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (* x y (reduce * more))))

(defn pred-and
    ([] (fn [v] true))
    ([x] (fn [v] (x v)))
    ([x & more] (fn [v] 
        (let [reducer-fn (fn [current-accum-val next-item] 
                            (if (true? current-accum-val) (and (next-item v) current-accum-val) false))]
        (reduce reducer-fn (x v) more)))))

(defn my-map [f a-seq]
  [:-])
