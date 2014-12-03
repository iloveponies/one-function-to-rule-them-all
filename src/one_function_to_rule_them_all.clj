(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    (str "")
    (reduce (fn [string element] (str string " " element)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (reduce (fn [elements element] (conj (conj elements x) element)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [current-count element] (inc current-count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [reversed element] (cons element reversed)) [] a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [min-max element] (let [min-e (first min-max)
                                      max-e (second min-max)]
                                 (cond
                                   (< element min-e) [element max-e]
                                   (> element max-e) [min-e element]
                                   :else [min-e max-e]
                                ))) [(first a-seq) (first a-seq)] (rest a-seq)))

(defn insert [sorted-seq n]
  ;[sorted-seq n])
  (loop [a-seq sorted-seq
         a-sorted-seq []]
      (if (empty? sorted-seq)
        [n]
        (if (empty? a-seq)
          a-sorted-seq
          (cond
            (and (= (count a-seq) 1) (< n (first a-seq))) (conj a-sorted-seq n (first a-seq))
            (and (= (count a-seq) 1) (> n (first a-seq))) (conj a-sorted-seq (first a-seq) n)
            (< n (first a-seq)) (concat (conj a-sorted-seq n (first a-seq)) (rest a-seq))
            :else (recur (rest a-seq) (conj a-sorted-seq (first a-seq))))))))

(defn insertion-sort [a-seq]
  ;a-seq)
  (reduce (fn [sorted element] (insert sorted element)) [] a-seq))

(defn count-element [e a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [element-count element] (if (= element e) (inc element-count) element-count)) 0 a-seq)))

(defn parity [a-seq]
  (loop [parity-sequence #{}
         rest-seq a-seq]
    (if (empty? rest-seq)
      parity-sequence
      (if (not= (mod (count-element (first rest-seq) rest-seq) 2) 0)
        (recur (conj parity-sequence (first rest-seq)) (filter (fn [element] (not= (first rest-seq) element)) rest-seq))
        (recur parity-sequence (filter (fn [element] (not= (first rest-seq) element)) rest-seq))))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and-more [x a-seq]
  ;[x a-seq])
  (loop [a-seqe a-seq
         result true]
    (if (empty? a-seqe)
      result
      (recur (rest a-seqe) (and result ((first a-seqe) x))))))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p-1 p-2] (fn [x] (and (p-1 x) (p-2 x))))
  ([p-1 p-2 & more] (fn [x] (and (and (p-1 x) (p-2 x) (pred-and-more x more))))))

(defn get-at-index [i a-seq]
  (reduce (fn [elements element] (conj elements (get element i))) [] a-seq))

; [[1 2 3] [4 5 6] [7 8 9]] -> [[1 4 7] [2 5 8] [3 6 9]]
(defn convert-sequence [a-seq]
  (loop [result []
         index 0]
    (if (= (count a-seq) index)
      result
      (recur (conj result (get-at-index index a-seq)) (inc index)))))

(defn my-map-more [f a-seq]
  (loop [result []
         a-seqe a-seq]
    (if (empty? a-seqe)
      result
      (recur (conj result (apply f (first a-seqe))) (rest a-seqe))
  )))

(defn my-map
  ([f a-seq] (map f a-seq))
  ([f a-seq & more] (my-map-more f (convert-sequence (cons a-seq more)))))
