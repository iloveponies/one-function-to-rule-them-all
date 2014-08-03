(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (cond
   (empty? a-seq) '()
   (= 1 (count a-seq)) a-seq
   :else (reduce (fn [y z]
                (let [the-list (if (list? y)
                                    (conj y x z)
                                    (list y x z))]
                  (reverse the-list)))
            a-seq)))

(defn my-count [a-seq]
  (let [counter (fn[cnt e](inc cnt))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn[new-list e](conj new-list e))]
    (reduce reverser '() a-seq)))

(defn nil-or-true [n pred tester]
  (or (nil? tester)(pred n tester)))

(defn min-max-element [a-seq]
  (let [miner (fn [min-max e] (if (nil-or-true e <= (first min-max)) [e (last min-max)] min-max))
         maxer (fn [min-max e] (if (nil-or-true e >= (last min-max)) [(first min-max) e] min-max))
         min-maxer (fn [min-max e](maxer (miner min-max e) e))]
   (reduce min-maxer [] a-seq)))

(defn insert [sorted-seq n]
  (loop [new-sorted-list '()
           sorted-list sorted-seq
           elem n]

    (let [fst (first sorted-list)
         snd (second sorted-list)]
      (cond
       (nil? fst)(list n)
       (nil-or-true n <= fst)(concat new-sorted-list (list n fst)(rest sorted-list))
       (nil-or-true n <= snd)(concat new-sorted-list (list fst n)(rest sorted-list))
       :else (recur (concat new-sorted-list (list fst)) (rest sorted-list) n)))))

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

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and [x]
  (fn [x] :-))

;(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
;(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
;(filter (pred-and number? integer? pos? even?)
;        [1 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f a-seq]
  [:-])
