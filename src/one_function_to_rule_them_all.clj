(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))



(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(str-cat ["asd" "wasd"])




(defn my-interpose [x a-seq]
   (let [interposer (fn [new-seq a-seq]
                      (if (empty? new-seq)
                        (conj new-seq a-seq)
                        (conj (conj new-seq x) a-seq)))]
     (reduce interposer [] a-seq)))


(defn my-count [a-seq]
  (let [counter (fn [count a-seq]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]

   (let [swapper (fn [new-seq a-seq]
                        (cons a-seq new-seq ))]
     (reduce swapper "" a-seq)))


(defn min-max-element [a-seq]
  (let [min-maxer (fn [min-max e]
                  (cond
                   (< e (first min-max)) [e (first (rest min-max))]
                   (> e (first (rest min-max))) [(first min-max) e]
                   :else  min-max))]
    (reduce min-maxer [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
   (loop [head [(first sorted-seq)]
         tail (rest sorted-seq)
         last-of-head (first sorted-seq)]
    (cond
     (empty? sorted-seq) [n]
     (<= n (first sorted-seq)) (concat [n] sorted-seq)
     (>= n (last sorted-seq)) (concat sorted-seq [n])
     (and (< last-of-head n) (> (first tail) n))(concat head [n] tail)
      :else (recur (conj head (first tail)) (rest tail) (first tail)))))

(defn insertion-sort [a-seq]
 (let [insertter (fn [sorted-seq elem]
                        (insert sorted-seq elem ))]
     (reduce insertter "" a-seq)))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn parity [a-seq]
  (let [toggle (fn [new-set elem]
                 (if (contains? new-set elem)
                   (disj new-set elem)
                   (conj new-set elem)))]
    (apply disj (set a-seq) (reduce toggle (set a-seq) a-seq))))



(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& params]
  (count params))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))



(defn pred-and
  ([] (fn [x]
        true))
  ([pred1] (fn [x]
         (if (pred1 x)
           true
           false)))
  ([pred1 pred2] (fn [x]
           (if (and (pred1 x) (pred2 x))
             true
             false)))
  ([pred1 pred2 & more]
          (reduce pred-and (pred-and pred1 pred2) more)))


(defn my-map-rec [f seq-1 seq-2]
 (if (or (empty? seq-1) (empty? seq-2))
   ()
   (cons (f (first seq-1) (first seq-2)) (my-map-rec f (rest seq-1) (rest seq-2)))))


(defn my-map
  ([f a-seq]
   (loop [seq a-seq
              new-seq '()]
      (if (empty? seq)
       new-seq
       (recur (rest seq) (concat   new-seq [(f (first seq))] )))))

  ([f a-seq b-seq]
   (loop [seq1 a-seq
          seq2 b-seq
          new-seq []]
   (if (or (empty? seq1) (empty? seq2))
   new-seq
   (recur (rest seq1)
          (rest seq2)
          (concat  new-seq [(f (first seq1) (first seq2))])))))

  ([f a-seq b-seq & more]
  (loop [seq1 a-seq
          seq2 b-seq
          new-seq nil
          remaining more]
     (if (empty? remaining)
       (my-map f seq1 seq2)
       (if (or (empty? seq1) (empty? seq2))
         (recur (first remaining) new-seq nil (rest remaining))
         (recur (rest seq1)
                (rest seq2)
                (concat  new-seq [(f (first seq1) (first seq2))])
                remaining))))))

(my-map + [1 2 3] [1 2 3] [1 2 3])



