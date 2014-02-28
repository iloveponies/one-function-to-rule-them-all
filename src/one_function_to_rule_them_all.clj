(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (let [f (fn [a b] (inc a))]
    (reduce f 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
    (let [f (fn [[mi ma] b]
              [(min mi b) (max ma b)])
          x (first a-seq)
          xs (rest a-seq)]
      (reduce f [x x] xs)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [p #(< % n)
          under (take-while p sorted-seq)
          over (drop-while p sorted-seq)
          this [n]]
      (concat under this over))))

(defn insertion-sort [a-seq]
    (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & zs] (reduce * (* x y) zs)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q] (fn [x] (and (p x) (q x))))
  ([p q & rs] (reduce pred-and (pred-and p q) rs)))

(defn my-map
  ([f s] (if (empty? s)
           []
           (cons (f (first s)) (my-map f (rest s)))))
  ([f s & ss] (let [seqs (cons s ss)]
                (if (some empty? seqs)
                  []
                  (cons (apply f (my-map first seqs))
                        (apply my-map (cons f (my-map rest seqs))))))))

;; (defn my-map
;;   ([f s] (reduce #(conj %1 (f %2)) [] s))
;;   ([f s & ss] (let [seqs (cons s ss)
;;                     first-args (my-map first seqs)
;;                     rest-args (my-map rest seqs)
;;                     g (fn [a b]
;;                         (conj (apply f first-args)
;;                               (apply my-map (cons f rest-args))))]
;;                 (reduce g [] seqs))))
