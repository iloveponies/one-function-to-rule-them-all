(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (clojure.string/join " " a-seq))

(defn my-interpose [x a-seq]
  (let [kjh (concat (vector (vector (first a-seq))) (rest a-seq))
        intp (fn [m s] (conj (conj m x) s))]
    (if (empty? a-seq)
      a-seq
      (if (= (count a-seq) 1)
        (vector (first a-seq))
        (reduce intp kjh)))))

(defn my-count [a-seq]
  (letfn [(cnt [count _] (inc count))]
    (reduce cnt 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (loop [[a b] [(first a-seq) (first a-seq)]
              a-seq (rest a-seq)]
    (if (empty? a-seq)
      [a b]
      (recur [(min a (first a-seq)) (max b (first a-seq))] (rest a-seq)))))

(defn insert [sorted-seq n]
  (let [bg (fn [a i] (reverse (nthrest (reverse a) (- (- (count a) 1) i))))]
    (loop [i 0]
      (if (empty? sorted-seq)
        (vector n)
        (if (> (first sorted-seq) n)
          (concat (vector n) sorted-seq)
          (if (apply < (concat (concat (bg sorted-seq i) (vector n)) (nthrest sorted-seq (+ i 1))))
            (concat (concat (bg sorted-seq i) (vector n)) (nthrest sorted-seq (+ i 1)))
            (recur (inc i))))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)]
    (set (map first (filter #(odd? (second %)) freqs)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [e] true))
  ([p] (fn [e] (p e)))
  ([p1 p2] (fn [e] (and (p1 e) (p2 e))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq]
   (let [do-map (fn [acc e]
                  (conj acc (f e)))]
     (reduce do-map [] a-seq)))
  ([f a-seq b-seq]
   (loop [s1 a-seq
          s2 b-seq
          acc []]
     (println s1 s2 acc)
     (if (or (empty? s1) (empty? s2))
       acc
       (recur (rest s1)
              (rest s2)
              (conj acc (f (first s1) (first s2)))))))
  ([f a-seq b-seq & more]
   (loop [seqs (concat [a-seq b-seq] more)
          acc []]
     (if (contains? (set (map empty? seqs)) true)
       acc
       (recur (map rest seqs)
              (conj acc (apply f (map first seqs))))))))
