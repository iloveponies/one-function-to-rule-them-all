(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (let [add-space (fn [acc add] (str acc " " add))]
      (reduce add-space a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) a-seq
    (cons (first a-seq)
      (let [add-x (fn [acc add] (concat acc (cons x (seq [add]))))]
        (reduce add-x '() (rest a-seq))))))

(defn my-count [a-seq]
  (let [counter (fn [n k] (inc n))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [acc add] (cons add acc))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc add]
                  (let [[minitem maxitem] acc]
                    (if (< add minitem) [add maxitem]
                    (if (> add maxitem) [minitem add] [minitem maxitem]))))]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) (seq [n])
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (if (> n (first (reverse sorted-seq)))
        (reverse (cons n (reverse sorted-seq)))
        (loop [beginning (seq [(first sorted-seq)])
               end (rest sorted-seq)]
          (if (and (>= n (first (reverse beginning))) (<= n (first end))) (concat beginning (cons n end))
            (recur (concat beginning (seq [(first end)])) (rest end))))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [joku (fn [acc item] (if (contains? acc item) (disj acc item) (conj acc item)))]
  (reduce joku #{} a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
    [& more] (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (loop [product (* x y)
          a-seq more]
      (if (empty? a-seq) product
        (if (= 1 (count a-seq)) (* product (first a-seq))
          (if (= 2 (count a-seq))
            (* product (* (first a-seq) (first (rest a-seq))))
            (recur (* product (* (first a-seq) (first (rest a-seq)))) (rest (rest a-seq)))))))))

(defn pred-and
  ([& preds]
   (fn [item]
     (loop [ret true
            preds preds]
       (if (empty? preds) ret
         (recur (and ret ((first preds) item)) (rest preds)))))))

(defn max-count-seq [a-seq]
  (apply max (map count a-seq)))

(defn my-map [f & a-seq]
  (let [helper (fn [b-seq times] (loop [n times result b-seq] (if (= 0 n) result (recur (dec n) (rest result)))))]
  (reverse
    (loop [finalresult '()
           worklist (reverse (loop [result '() n (apply max (map count a-seq))
           rn 0]
      (if (= 0 n)
        result
        (recur
          (cons
            (loop [ret '()
                   b-seq a-seq]
                  (if (empty? b-seq)
                    ret
                    (recur
                        (cons (first (helper (first b-seq) rn)) ret)
                        (rest b-seq)))) result) (dec n) (inc rn)))))
                  k (count worklist)]
    (if (= 0 k) finalresult
      (recur
        (cons
          (apply f
                 (first worklist)) finalresult) (rest worklist) (dec k)))))))












