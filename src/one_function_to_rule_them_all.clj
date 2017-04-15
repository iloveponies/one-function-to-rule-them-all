(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (cond (empty? a-seq) ""
        (empty? (rest a-seq)) (first a-seq)
        :else (reduce (fn [str1 str2] (str str1 " " str2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (not (list? a-seq))
    (my-interpose x (apply list a-seq))
    (rest (reverse (cond (empty? a-seq) '()
                         (empty? (rest a-seq)) (first a-seq)
                         :else (reduce (fn [sq elem] (cons elem (cons x sq)))
                                       '()
                                       a-seq))))))

(defn my-interpose [x a-seq]
  (let [add-x (fn [sq] (reduce (fn [sq elem] (cons elem (cons x sq)))
                               (list (first sq))
                               (rest sq)))
        fun (fn [sq] (reverse (add-x sq)))]
  (cond (empty? a-seq) '()
        (empty? (rest a-seq)) a-seq
        (not (list? a-seq)) (fun (apply list a-seq))
        :else (fun a-seq))))

(defn my-count [a-seq]
  (reduce (fn [x _] (inc x)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc x] (cons x acc)) '() a-seq))

(defn min-max-element [a-seq]
  (reduce (fn [[small big] x] [(min small x) (max big x)])
          [(first a-seq) (first a-seq)]
          (rest a-seq)))

(defn insert [sorted-seq n]
  (loop [init '()
        a-seq sorted-seq]
    (if (or (empty? a-seq)
        (< n (first a-seq)))
      (concat (reverse init) (cons n a-seq))
      (recur (cons (first a-seq) init) (rest a-seq)))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle-elem (fn [a-set elem]
                      (if (some (fn [set-elem] (= elem set-elem)) a-set)
                        (disj a-set elem)
                        (conj a-set elem)))]
    (reduce toggle-elem #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * (* x (first more)) (rest more))))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (reduce (fn [p q] (fn [x] (and (p x) (q x)))) more)))

; f takes only one parameter
(defn my-simple-map [f a-seq]
  (my-reverse (loop [ret '()
                     sq a-seq]
                 (if (empty? sq)
                   ret
                   (recur (cons (f (first sq)) ret) (rest sq))))))

(defn my-map
  ([f a-seq & more]
    (let [firsts (fn [sq] (my-simple-map first sq))
          rests  (fn [sq] (my-simple-map rest sq))
          some-empty? (fn [sq] (some boolean (my-simple-map empty? sq)))]
      (reverse (loop [ret '()
                      sq (cons a-seq more)]
                  (if (or (empty? sq)
                          (some-empty? sq))
                    ret
                    (recur (cons (apply f (firsts sq)) ret) (rests sq))))))))
