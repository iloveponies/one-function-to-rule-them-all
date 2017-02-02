(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (let [pred (fn [x y] (str x " " y))]
    (if (empty? a-seq)
      ""
      (reduce pred a-seq))))

(defn my-interpose [x a-seq]
  (let [pred (fn [a b] 
              (if (empty? a)
                (conj a b)
                (conj a x b)))]
    (if (empty? a-seq)
      ()
      (reduce pred [] a-seq))))

(defn my-count [a-seq]
  (let [pred (fn [cnt elem] (inc cnt))]
    (reduce pred 0 a-seq)))

(defn my-reverse [a-seq]
  (let [pred (fn [a b] (conj a b))]
    (if (empty? a-seq)
      ()
      (reduce pred () a-seq))))

(defn min-max-element [a-seq]
  (let [pred (fn [vector elem]
              (cond
                (empty? vector)
                  [elem elem]
                (< elem (get vector 0))
                  (assoc vector 0 elem)
                (> elem (get vector 1))
                  (assoc vector 1 elem)
                :else
                  vector))]
    (reduce pred [] a-seq)))

(defn insert [sorted-seq n]
  (loop [result []
        elem (first sorted-seq)
        seq (rest sorted-seq)]
    (cond 
      (and (= elem nil) (empty? seq))
        (conj result n)
      (< elem n)
        (recur (conj result elem) (first seq) (rest seq))
      :else
        (concat (conj result n elem) seq))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle 
  "If element exists in a set, removes it. Otherwise adds it." 
  [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) 
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle (set ()) a-seq))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [x] true))
  ([x] x)
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

(defn my-map 
  ([f a-seq] (if (empty? a-seq) 
                a-seq 
                (cons (f (first a-seq)) (my-map f (rest a-seq)))))
  ([f a-seq b-seq] (cond
                      (empty? a-seq)
                        a-seq
                      (empty? b-seq)
                        b-seq
                      :else
                        (cons (f (first a-seq) (first b-seq)) (my-map f (rest a-seq) (rest b-seq)))))
  ([f a-seq b-seq & more] (loop [seq1 (my-map f a-seq b-seq)
                                seq2 (first more)]
                            (if (empty? seq2)
                              seq1
                             (recur (my-map f seq1 seq2) (rest more))))))