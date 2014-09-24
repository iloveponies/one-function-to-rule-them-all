(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (let [reduce-helper (fn [acc elem] (str acc " " elem))]
      (reduce reduce-helper (first a-seq) (rest a-seq)))))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq)
      ()
    :else
      (let [reduce-helper (fn [acc elem] (conj acc x elem))
            seq           (reverse a-seq)]
        (reduce reduce-helper (list (first seq)) (rest seq)))))

(defn my-count [a-seq]
  (let [inc-helper  (fn [acc elem] (inc acc))]
    (reduce inc-helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-helper  (fn [acc elem] (conj acc elem))]
    (reduce reverse-helper '() a-seq)))

(defn min-max-element [a-seq]
  (let [helper  (fn [[min max] elem]
                  (cond
                    (< elem min)
                      [elem max]
                    (> elem max)
                      [min elem]
                    :else
                      [min max]))]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [result ()
         seq    sorted-seq]
    (cond
      (empty? seq)
        (concat result (list n))
      (< n (first seq))
        (concat result (list n) seq)
      :else
        (recur
          (concat result (list (first seq)))
          (rest seq)))))

(defn insertion-sort [a-seq]
  (reduce insert (list) a-seq))

(defn parity [a-seq]
  (let [helper  (fn [acc elem]
                  (if (contains? acc elem)
                    (disj acc elem)
                    (conj acc elem)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* [& more]
  (if empty? more)
    1
    (reduce * 1 more))

(defn pred-and [& more]
  (fn [elem]
    (let [helper  (fn [acc pred]
                    (and acc (pred elem)))]
      (reduce helper true more))))

(defn helper-take-nth [n a-seq]
  (loop [seq    a-seq
         index  0]
    (if (= index n)
      (first seq)
      (recur (rest seq) (inc index)))))

(defn helper-take-nths [n a-seq]
  (let [helper  (fn [acc elem]
                  (conj acc (helper-take-nth n elem)))]
    (reverse (reduce helper () a-seq))))

(defn helper-transpose [a-seq col-length]
  (loop [seq        a-seq
         index      0
         result     ()]
    (if (= index col-length)
      (reverse result)
      (recur seq (inc index) (conj result (helper-take-nths index seq))))))

(defn my-map [f & more]
  (let [helper        (fn [acc elem]
                        (conj acc (apply f elem)))]
    (reverse (reduce helper () (helper-transpose more (count (first more)))))))
