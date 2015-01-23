(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [conj-x-conj (fn [a-seq elem]
                      (if (empty? a-seq)
                        (conj a-seq elem)
                        (conj (conj a-seq x) elem)))]
    (reduce conj-x-conj '[] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [acc e]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse-helper (fn [rev-seq e]
                         (conj rev-seq e))]
    (reduce reverse-helper '() a-seq)))

(defn min-max-element [a-seq]
  (let [nil-comp (fn [f a b]
                   (cond
                     (and (nil? a) (nil? b)) nil
                     (nil? a) b
                     (nil? b) a
                     :else (f a b)))
        nil-min (fn [a b] (nil-comp min a b))
        nil-max (fn [a b] (nil-comp max a b))
        min-max (fn [v e]
                  (let [[minv maxv] v]
                    [(nil-min minv e) (nil-max maxv e)]))]
   (reduce min-max [nil nil] a-seq)))

(defn insert [sorted-seq n]
  (loop [sort-seq sorted-seq
         build-seq '[]
         inserted false]
    (cond
      (true? inserted)
        (concat build-seq sort-seq)
      (empty? sort-seq)
        (conj build-seq n)
      (and (empty? build-seq) (<= n (first sort-seq)))
        (recur (rest sort-seq) (conj (conj build-seq n) (first sort-seq)) true)
      (empty? build-seq)
        (recur (rest sort-seq) (conj build-seq (first sort-seq)) false)
      (<= (last build-seq) n (first sort-seq))
        (recur (rest sort-seq) (conj (conj build-seq n) (first sort-seq)) true)
      :else
        (recur (rest sort-seq) (conj build-seq (first sort-seq)) false))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [parity-helper (fn [a-set e]
                        (if (contains? a-set e)
                          (disj a-set e)
                          (conj a-set e)))]
    (reduce parity-helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& more] (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] 
    (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [e] true))
  ([x] (fn [e] (x e)))
  ([x y] (fn [e] (and (x e) (y e))))
  ([x y & more]
   (fn [e]
     (loop [bool (and (x e) (y e))
            fns more]
       (if (empty? fns)
         bool
         (recur (and bool ((first fns) e)) (rest fns)))))))

(defn firsts-and-rests
  ([a-seq] [(first a-seq) (rest a-seq)])
  ([a-seq & more]
   (loop [firsts [(first a-seq)]
          rests [(rest a-seq)]
          break more]
     (if (empty? break)
       [firsts rests]
       (recur (conj firsts (first (first break)))
              (conj rests (rest (first break)))
              (rest break))))))

(defn my-map
  ([f a-seq]
   (loop [map-seq '[]
          my-seq a-seq]
     (if (empty? my-seq)
       map-seq
       (recur (conj map-seq (f (first my-seq))) (rest my-seq)))))
  ([f a-seq & more]
   (loop [map-seq '[]
          many-seqs (cons a-seq more)]
     (if (some empty? many-seqs)
       map-seq
       (let [[firsts rests] (apply firsts-and-rests many-seqs)]
         (recur (conj map-seq (apply f firsts)) rests))))))
       
