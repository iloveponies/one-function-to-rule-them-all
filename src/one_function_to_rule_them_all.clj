(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))


(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))


(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (let [f (fn [a b] (conj a x b))]
      (reduce f [(first a-seq)] (rest a-seq)))))


(defn my-count [a-seq]
  (let [counter (fn [acc x] (inc acc))]
    (reduce counter 0 a-seq)))


(defn my-reverse [a-seq]
  (if (empty? a-seq)
    []
    (let [f (fn [a b] (cons b a))]
      (reduce f () a-seq))))


(defn min-max-element [a-seq]
  (let [f (fn [[x y] b]
            (cond
              (< b x) [b y]
              (> b y) [x b]
              :else [x y]))]
    (reduce f [(first a-seq) (first a-seq)] (rest a-seq))))


(defn insert [sorted-seq n]
  (loop [acc []
         s sorted-seq
         el n]
    (cond
      (empty? s) (seq (conj acc el))
      (<= el (first s)) (concat (conj acc el) s)
      :else (recur (conj acc (first s)) (rest s) el))))


(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))


(defn parity [a-seq]
  (let [toggle (fn [st el]
                 (if (contains? st el)
                   (set (remove #{el} st))
                   (conj st el)))]
    (reduce toggle #{} a-seq)))


(defn minus
  ([x] (if (> x 0) (* -1 x) x))
  ([x y] (- x y)))


(defn count-params
  ([] 0)
  ([x & more] (+ 1 (count more))))


(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (fn [x]
                    (reduce (fn [ans ques] (and ans (ques x))) (and (p1 x) (p2 x)) more))))


;; (defn my-map
;;   ([f a-seq] (reduce (fn [prv el] (conj (vec prv) (f el))) '() a-seq))
;;   ([f a-seq & more] (reduce (fn [prv el] (conj (vec prv) (my-map f el))) [(my-map f a-seq)] more)))


;; (defn my-map
;;   ([f a-seq] (reduce (fn [acc el] (conj (vec acc) (f el))) '() a-seq))
;;   ([f a-seq b-seq] (loop [aa a-seq
;;                           bb b-seq
;;                           acc []]
;;                     (if (or (empty? aa) (empty? bb))
;;                       acc
;;                       (recur (rest aa)
;;                              (rest bb)
;;                              (conj acc (f (first aa) (first bb)))))))
;;   ([f a-seq b-seq & more] (reduce
;;                             (fn [prv el] (my-map f prv el))
;;                             (my-map f a-seq b-seq)
;;                             more)))
;; FAIL: (my-map vector [1 2 3] [1 2 3] [1 2 3]) => [[[1 1] 1] [[2 2] 2] [[3 3] 3]]


(defn my-map [f a-seq & more]
  (if (empty? more)
    (if (empty? a-seq)
      []
      (cons (f (first a-seq)) (my-map f (rest a-seq))))
    (let [all (cons a-seq more)
          firsts (my-map first all)
          rests (my-map rest all)]
       (if (some nil? firsts)
         []
         (cons (apply f firsts) (apply my-map (cons f rests)))))))

