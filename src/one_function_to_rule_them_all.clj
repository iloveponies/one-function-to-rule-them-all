(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (= (count a-seq) 0)
    (str "")
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (drop 1 (interleave (repeat x) a-seq)))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (into [y] x)) [] a-seq))

(defn min-max-element [a-seq]
  (let [min-max
        (fn [[curmin curmax] x]
          [(min curmin x) (max curmax x)])
        fst (first a-seq)]
    (reduce min-max [fst fst] (rest a-seq))))

(defn insert [sorted-seq n]
    (loop [before [] after sorted-seq]
    (let [next (first after)]
      (if (or (empty? after) (< n next))
        (concat (conj before n) after)
        (recur (conj before next) (rest after))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [freqs (fn [freq x]
                (assoc freq x
                       (if (contains? freq x)
                         (inc (freq x))
                         1)))]
    (set (keys (filter #(odd? (val %)) (reduce freqs {} a-seq))))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([x] #(x %))
  ([x & more] #(reduce (fn [a b] (and a (b %))) (x %) more)))

(defn my-map
  ([f a] (map f a))
   ([f a b] (map f a b))
   ([f a b & more]
    (let [apu (fn apu [alphas]
      (let [ms (my-map seq alphas)]
        (when (every? identity ms)
        (cons (my-map first ms)
          (apu (my-map rest ms))))))]
            (my-map #(apply f %) (apu (conj more b a))))))
