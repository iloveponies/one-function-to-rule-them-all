(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [c1 c2] (str c1 " " c2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (rest (reduce (fn [c1 c2] (conj (conj c1 x) c2)) [] a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [acc _] (inc acc)) 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [head (first a-seq)]
    (reduce (fn [[cur-min cur-max] e]
              (let [new-min (if (< e cur-min) e cur-min)
                    new-max (if (> e cur-max) e cur-max)]
                [new-min new-max]))
            [head head]
            (rest a-seq))))

(defn insert [sorted-seq n]
  (let [before (take-while (fn [x] (< x n)) sorted-seq)
        after (drop-while (fn [x] (< x n)) sorted-seq)]
    (concat before (cons n after))))

(defn insertion-sort [a-seq]
  (reduce (fn [acc e] (insert acc e)) [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [z _] (inc z)) 0 more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (* x (reduce (fn [a b] (* a b)) 1 more))))

(defn pred-and
  ([] (fn [z] true))
  ([x] (fn [z] (x z)))
  ([x & more] (reduce (fn [acc new-pred] (fn [y] (and (acc y) (new-pred y)))) x more)))

(defn my-map
  ([f a-seq]
    (if (empty? a-seq)
      '()
      (cons (f (first a-seq)) (my-map f (rest a-seq))))))

(defn my-map2
  ([] '())
  ([f & seqs]
   (let [firsts (map first seqs)
         rests (map rest seqs)
         _ (println firsts rests)]
    (if (empty? rests)
      '()
      (cons (f firsts)
            (my-map2 f (seq rests))
            )))))

(defn firsts
  ([] '())
  ([& seqs]
     (if (empty? (first seqs))
       '()
       (cons (map first seqs) (apply firsts (map rest seqs))))))

;(defn my-map [f & seqs]
;  (apply map f seqs (firsts seqs)))

;(defn map
;  ([f coll]
;   (lazy-seq
;    (when-let [s (seq coll)]
;      (if (chunked-seq? s)
;        (let [c (chunk-first s)
;              size (int (count c))
;              b (chunk-buffer size)]
;          (dotimes [i size]
;              (chunk-append b (f (.nth c i))))
;          (chunk-cons (chunk b) (map f (chunk-rest s))))
;        (cons (f (first s)) (map f (rest s)))))))
;  ([f c1 c2]
;   (lazy-seq
;    (let [s1 (seq c1)
;          s2 (seq c2)]
;      (when (and s1 s2)
;        (cons (f (first s1) (first s2))
;              (map f (rest s1) (rest s2)))))))
;  ([f c1 c2 c3]
;   (lazy-seq
;    (let [s1 (seq c1)
;          s2 (seq c2)
;          s3 (seq c3)]
;      (when (and  s1 s2 s3)
;        (cons (f (first s1) (first s2) (first s3))
;              (map f (rest s1) (rest s2) (rest s3)))))))
;  ([f c1 c2 c3 & colls]
;   (let [step (fn step [cs]
;                 (lazy-seq
;                  (let [ss (map seq cs)]
;                    (when (every? identity ss)
;                      (cons (map first ss) (step (map rest ss)))))))]
;     (map #(apply f %) (step (conj colls c3 c2 c1))))))













