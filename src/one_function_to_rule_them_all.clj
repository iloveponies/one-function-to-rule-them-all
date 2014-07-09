(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [prev-thing next-thing]
              (str prev-thing " " next-thing))
            a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    a-seq
    (reduce (fn [prev-stuff this-element]
              (conj prev-stuff x this-element))
            [(first a-seq)]
            (rest a-seq))))


(defn my-count [a-seq]
  (reduce (fn [count ele]
            (inc count))
          0
          a-seq))

(defn my-reverse [a-seq]
  (reduce conj
          '()
          a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[old-min old-max] new-val]
            [(min old-min new-val)
             (max old-max new-val)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (cond
   (empty? sorted-seq) [n]
   (> n (first sorted-seq)) (cons (first sorted-seq)
                                  (insert (rest sorted-seq)
                                          n))
   :else (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert
          []
          a-seq))

(defn parity [a-seq]
  (reduce (fn [parity-set new-element]
            (if (contains? parity-set new-element)
              (disj parity-set new-element)
              (conj parity-set new-element)))
          #{}
          a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& params]
  (reduce (fn [count ele]
            (inc count))
          0
          params))

(defn my-*
  ([] 1)
  ([& numbers] (reduce *
                       1
                       numbers)))

(defn pred-and
  ([] (fn [val] true))
  ([& predicates] (fn [val]
                    (reduce (fn [old-bool new-predicate]
                              (and old-bool
                                   (new-predicate val)))
                            true
                            predicates))))

(defn call-on-each [f sequences]
  (if (empty? (first sequences))
    '()
    (cons (f (first sequences))
          (call-on-each f (rest sequences)))))

(defn my-map [f & sequences]
  (if (empty? (first sequences))
    '()
    (cons (apply f (call-on-each first sequences))
          (apply my-map
                 f
                 (call-on-each rest sequences)))))
