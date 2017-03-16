(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
      ""
      (reduce (fn [x y] (str x " " y)) (first a-seq) (rest a-seq))))

(defn my-interpose [gap a-seq]
  (if (empty? a-seq)
      ()
      (if (= 0 (count a-seq))
          a-seq
          (reduce (fn [x y] (concat x [gap] [y])) [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (reduce (fn [x y] (+ x 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [x y] (cons y x)) () a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [x y] [(min (first x) y) (max (second x) y)])
          [(first a-seq) (first a-seq)]
          a-seq))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq)
           (empty? b-seq))
      ()
      (if (empty? a-seq)
          (seq-merge b-seq a-seq)
          (if (empty? b-seq)
              (concat (repeat 1 (first a-seq))
                      (seq-merge (rest a-seq) b-seq))
              (if (<= (first a-seq) (first b-seq))
                  (concat (repeat 1 (first a-seq))
                          (seq-merge (rest a-seq) b-seq))
                  (seq-merge b-seq a-seq))))))

(defn insert [seq n]
  (seq-merge seq [n]))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))

(defn my-* [& params]
  (reduce * 1 params))

(defn pred-and [& preds]
  (fn [val] (if (empty? preds)
                true
                (reduce (fn [acc pred] (and acc (pred val))) true preds))))

(defn get-nth [seqs n]
  (reduce (fn [x y] (concat x [(get y n)])) () seqs))

(defn my-map [f & seqs]
  (reduce (fn [x y] (concat x [(apply f (get-nth seqs y))]))
          ()
          (range (count (first seqs)))))