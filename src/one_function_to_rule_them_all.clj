(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '[] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    ;(reduce (concat " ") a-seq))
    ;(reduce (fn [x y] (str(concat x " " y))) a-seq)))
    (reduce (fn [x y] (str x " " y)) a-seq)))

; TODO
(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce (fn [y z] (concat y (cons x z))) (first a-seq) (rest a-seq))))
    ;(reduce (fn [y z] (cons y (cons x z))) a-seq)))
    ;(reduce (fn [y z] (conj x y z)))))

(defn my-count [a-seq]
  (let [counter (fn [mcount elem]
                    (inc mcount))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [minimax (fn [[x y] z]
                  (if (< z x)
                    [z y]
                    (if (> z y)
                      [x z]
                      [x y])))]
    (reduce minimax [(first a-seq) (first a-seq)] a-seq)))

; I'm sure it was just something small, but since reduce wasn't required
; for this function, i went recursive just to get it working
(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
    (let [f (first sorted-seq)
          r (rest sorted-seq)]
      (if (< n f)
        (cons n sorted-seq)
        (cons f (insert r n))))))
;  (if (< n (first sorted-seq))
;    (cons n sorted-seq)
;    (loop [lst '[]
;           rst sorted-seq]
;      (let [f (first rst)]
;        (if (empty? rst)
;          (conj lst n)
;          (if (< n f)
;            (concat (conj (conj lst n) f) (rest rst)) ; reached end, 
;            (recur (conj lst f) (rest rst))))))))
;           beg (first sorted-seq)
;           end (rest sorted-seq)]
;      (if (empty? end)
;        (conj beg n)
;        (if (< n (first end))
;          (concat beg (cons n end))
;          (recur (concat beg '[(first end)]) (rest end)))))))

(defn insertion-sort [a-seq]
  (reduce insert '[] a-seq))

(defn parity [a-seq]
  ; didn't even try to see if "toggle" through keyword errors, just made
  ; it work regardless
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))]
    (reduce toggle #{} a-seq)))

(defn minus 
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([] 0)
  ([x] 1)
  ([x y] 2)
  ([x y & more]
    ;(+ 1 (count-params more))))
    (+ (count-params x y) (reduce count-params more))))
    ;(reduce count-params (count-params x) more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ;([x y & more] (* (my-* x y) (my-* more))))
  ([x y & more] (reduce my-* (my-* x y) more)))

(defn pred-and 
  ([] (fn [z] true))
  ([x] x)
  ;([x] (fn [z] true))
  ([x y] (fn [z] (and (x z) (y z))))
  ;([x y & more] (fn [z] (and ((pred-and x y) z) ((pred-and more) z)))))
  ([x y & more] (reduce pred-and (pred-and x y) more)))

; TODO
(defn my-map 
  ([f a-seq] (reduce (fn [x y] (conj x (f y))) '[] a-seq)))
  ;([f a-seq] [:-]))
  ;([f a-seq] (reduce cons (f (first a-seq)) (rest a-seq))) )
      ;((apply f a-seq))
  ;([f a-seq b-seq] (apply f a-seq b-seq))
  ;([f a-seq b-seq & more] (reduce my-map (my-map f a-seq b-seq) more)))
