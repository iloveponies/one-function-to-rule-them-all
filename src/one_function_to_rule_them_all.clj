(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [string s] (str string " " s)) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce (fn [interpose elem] (conj interpose x elem)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [count _] (inc count)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [[fst & rst]]
  (let [chooser (fn [[min1 max1] x]
                  [(min min1 x) (max max1 x)])]
    (reduce chooser [fst fst] rst)))

(defn insert [sorted-seq n]
  (loop [right '()
         left sorted-seq]
    (if (or (empty? left) (< n (first left)))
      (concat right (list n) left)
      (recur (concat right (list (first left)))
             (rest left)))))


(defn insert [sorted-seq n]
  (loop [counter 0
         seq sorted-seq]
    (if (or (empty? seq) (> (first seq) n))
      (concat (take counter sorted-seq) [n] (drop counter  sorted-seq))
      (recur (inc counter)
             (rest seq)))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [set elm]
                 (if (contains? set elm)
                   (disj set elm)
                   (conj set elm)))]

   (reduce toggle #{} a-seq)))


(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([] 0)
  ([x] 1)
  ([x & more] (reduce (fn [c _] (inc c)) 1 more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn pred-and 
  ([] (fn [_] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])


;; (defmacro report
;;   [to-try]
;;   `(let [result# ~to-try]
;;      (if result#
;;        (println (quote ~to-try) "was successful:" result#)
;;        (println (quote ~to-try) "was not successful:" result#))))

;; (defmacro doseq-macro
;;   [macroname & args]
;;   `(do
;;      ~@(map (fn [arg] (list macroname arg)) args)))



;; (defn my-map [f seq-1 seq-2]
;;   (cond
;;     (or (empty? seq-1) (empty? seq-2)) '()
;;     :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


;; (defn my-map [f seq-1 seq-2]
;;   (loop [seq-1 seq-1
;;          seq-2 seq-2
;;          mapped []]
;;     (if (or (empty? seq-1) (empty? seq-2))
;;       mapped
;;       (recur (rest seq-1) (rest seq-2)
;;              (conj mapped (f (first seq-1) (first seq-2)))))))

;; (defn my-map [f seq-1]
;;   )

;; (defn my-map
;;   ([f seq-1] (reduce (fn [mapped x] (conj mapped (f x))) [] seq-1))
;;   ([f seq-1 seq-2] (loop [seq-1 seq-1
;;          seq-2 seq-2
;;          mapped []]
;;     (if (or (empty? seq-1) (empty? seq-2))
;;       mapped
;;       (recur (rest seq-1) (rest seq-2)
;;              (conj mapped (f (first seq-1) (first seq-2))))))
;;    [f seq-1 seq-2 & more]
;;    (reduce (fn [mapped ]))))


;; (defn map
;;   "Returns a lazy sequence consisting of the result of applying f to
;;   the set of first items of each coll, followed by applying f to the
;;   set of second items in each coll, until any one of the colls is
;;   exhausted.  Any remaining items in other colls are ignored. Function
;;   f should accept number-of-colls arguments. Returns a transducer when
;;   no collection is provided."
;;   {:added "1.0"
;;    :static true}
;;   ([f]
;;    (fn [rf]
;;      (fn
;;        ([] (rf))
;;        ([result] (rf result))
;;        ([result input]
;;         (rf result (f input)))
;;        ([result input & inputs]
;;         (rf result (apply f input inputs))))))
;;   ([f coll]
;;    (lazy-seq
;;     (when-let [s (seq coll)]
;;       (if (chunked-seq? s)
;;         (let [c (chunk-first s)
;;               size (int (count c))
;;               b (chunk-buffer size)]
;;           (dotimes [i size]
;;             (chunk-append b (f (.nth c i))))
;;           (chunk-cons (chunk b) (map f (chunk-rest s))))
;;         (cons (f (first s)) (map f (rest s)))))))
;;   ([f c1 c2]
;;    (lazy-seq
;;     (let [s1 (seq c1) s2 (seq c2)]
;;       (when (and s1 s2)
;;         (cons (f (first s1) (first s2))
;;               (map f (rest s1) (rest s2)))))))
;;   ([f c1 c2 c3]
;;    (lazy-seq
;;     (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
;;       (when (and  s1 s2 s3)
;;         (cons (f (first s1) (first s2) (first s3))
;;               (map f (rest s1) (rest s2) (rest s3)))))))
;;   ([f c1 c2 c3 & colls]
;;    (let [step (fn step [cs]
;;                 (lazy-seq
;;                  (let [ss (map seq cs)]
;;                    (when (every? identity ss)
;;                      (cons (map first ss) (step (map rest ss)))))))]
;;      (map #(apply f %) (step (conj colls c3 c2 c1))))))





;; (defn my-map
;;   ([f coll]
;;    (when-let [s (seq coll)]
;;      (cons (f (first s)) (map f (rest s)))))
;;   ([f c1 c2]
;;    (let [s1 (seq c1) s2 (seq c2)]
;;      (when (and s1 s2)
;;        (cons (f (first s1) (first s2))
;;              (map f (rest s1) (rest s2))))))
;;   ([f c1 c2 c3]
;;    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
;;      (when (and  s1 s2 s3)
;;        (cons (f (first s1) (first s2) (first s3))
;;              (map f (rest s1) (rest s2) (rest s3))))))
;;   ([f c1 c2 c3 & colls]
;;    (let [step (fn step [cs]
;;                 (let [ss (map seq cs)]
;;                   (when (every? identity ss)
;;                     (cons (map first ss) (step (map rest ss))))))]
;;      (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defn my-step
  [cs]
  (when (apply and (map not-empty cs))
    (cons (map first cs)
          (my-step (map rest cs)))))
