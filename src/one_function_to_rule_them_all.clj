(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  ;; (if (empty? a-seq)
  ;;   ""
  ;;   (clojure.string/join " " a-seq)))
  (if (empty? a-seq)
    ""
    (apply str
           (butlast (reduce concat
                            (map (fn [x] (concat x [" "]))
                                 a-seq))))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (butlast (reduce concat
                     (map (fn [e] (cons e (list x)))
                          a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [count elem]
                  (if (nil? elem)
                    count
                    (inc count)))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  ;; (if (empty? a-seq)
  ;;   '()
  ;;   (cons (last a-seq) (butlast a-seq))))
  (let [reverser (fn [a b]
                   (vec (flatten (concat (list b a)))))]
    (reduce reverser [] a-seq)))

(defn min-max-element [a-seq]
  [(reduce min a-seq)
   (reduce max a-seq)])

(defn insert [sorted-seq n]
  (cond (empty? sorted-seq) (list n)
        (<= n (first sorted-seq)) (concat (list n (first sorted-seq))
                                          (rest sorted-seq))
        (>  n (first sorted-seq)) (concat (list (first sorted-seq))
                                          (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (set
   (mapcat
    (fn [[k v]]
      (when (odd? v) [k]))
    (reduce (fn [result item]
              (update-in result [item] (fnil inc 0)))
            {}
            a-seq))))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
