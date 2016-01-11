(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [str-op (fn [acc x] (if (empty? acc) (concat acc x) (concat acc " " x)))]
    (apply str (reduce str-op "" a-seq))))

(defn my-interpose [x a-seq]
  (let [init-seq (if (empty? a-seq) [] (conj [] (first a-seq)))
        interior (fn [init-seq y] (conj (conj init-seq x) y))]
    (reduce interior init-seq (rest a-seq))))

(defn my-count [a-seq]
  (let [count-each (fn [acc x] (inc acc))]
    (reduce count-each 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev (fn [init-seq x] (cons x init-seq))]
    (reduce rev () a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [acc (conj (conj [] (first a-seq)) (first a-seq))
          min-max (fn [acc x]
                    (cond
                      (< x (first acc)) (assoc acc 0 x)
                      (> x (first (rest acc))) (assoc acc 1 x)
                      :else acc))]
      (reduce min-max acc a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (conj sorted-seq n)
    (let [less-than-or-equal (fn [k] (<= k n))
          lower-half (take-while less-than-or-equal sorted-seq)
          upper-half (drop-while less-than-or-equal sorted-seq)]
      (concat lower-half (cons n upper-half)))))

(defn insertion-sort [a-seq]
  (let [acc []
        insert-in (fn [acc k] (insert acc k))]
    (reduce insert-in acc a-seq)))

(defn parity [a-seq]
  (let [inc-count (fn [a-map a-key]
                    (inc (a-map a-key 0)))
        update-map (fn [a-map a-key]
                     (assoc a-map a-key (inc-count a-map a-key)))
        totals-map (reduce update-map {} a-seq)
        odd-val? (fn [a-vec]
                   (odd? (second a-vec)))
        filtered-map (filter odd-val? totals-map)]
    (set (keys filtered-map))))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([& more] (reduce * more)))

(defn pred-and [& more]
  (fn [n]
    (loop [pred-seq more]
      (cond
        (empty? pred-seq) true
        (not ((first pred-seq) n)) false
        :else (recur (rest pred-seq))))))

(defn my-map [f & more]
  (defn process-singleton-seq [func a-seq]
    (loop [f func loop-seq a-seq acc ()]
      (if (empty? loop-seq) (reverse acc)
        (recur f (rest loop-seq) (cons (f (first loop-seq)) acc)))))
  (defn process-multiple-seqs [func seqs-seq]
    (loop [f func loop-seq seqs-seq acc ()]
      (if (some empty? loop-seq)
        (reverse acc)
        (recur
          f
          (for [s loop-seq] (rest s))
          (cons (apply f (for [s loop-seq] (first s))) acc)))))
  (if (empty? (first (rest more)))
    (process-singleton-seq f (first more))
    (process-multiple-seqs f more)))
