(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; (product '(1 2 4)
; =  (* (cons 1 (cons 2 (cons 4))))
; => (* 1 (product (cons 2 (cons 4 nil))))
; => (* 1 (* 2 ( product (cons 4 nil))))
; => (* 1 (* 2 (* 4 (product nil))))
; => (* 1 (* 2 (* 4 1)))
; => (* 1 (* 2 (* 4)))
; => (* 1 (* 2 4))
; => (* 1 8)
; => 8

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (when (seq coll) (if (singleton? coll) (first coll) (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
    nil
    (singleton? a-seq)
    (first a-seq)
    :else
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? n) 0
    (zero? k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) '()
    :else
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (for [i (range (count a-seq))]
      (concat (drop i a-seq) (take i a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [curr-elem (first a-seq)
        n-found (if (number? (get freqs curr-elem))
                  (get freqs curr-elem)
                  0)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (conj freqs {curr-elem (inc n-found)}) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (reduce concat (map #(repeat (second %) (first %)) a-map)))

(defn my-take [n coll]
  (cond
    (or (<= n 0) (empty? coll))
    []
    :else
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop-helper [c n coll]
  (cond
    (empty? coll) []
    (= c n) coll
    (< c n) (my-drop-helper (inc c) n (rest coll))
    :else []))

(defn my-drop [n coll]
  (my-drop-helper 0 n coll))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (vector (my-take h a-seq) (my-drop h a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (let [a (first a-seq) b (first b-seq)]
      (if (< a b)
        (cons a (seq-merge (rest a-seq) b-seq))
        (cons b (seq-merge (rest b-seq) a-seq))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn mono-req [x]
                     (if (>= 2 (count x))
                       true
                       (let [[a b c] x]
                         (if (< 0 (* (- a b) (- b c)))
                           (mono-req (rest x))
                           false))))
        start (last (take-while monotonic? (inits a-seq)))]
    (if (empty? a-seq)
      '()
      (cons start (split-into-monotonics (drop (count start) a-seq))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (= 1 (count a-set)) (vector a-set)
    :else (let [join (fn [x y] (map #(cons x %) y))]
            (->> a-set
                 (rotations)
                 (map #(join (first %) (permutations (rest %))))
                 (apply concat)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [actually-a-set (set a-set)
          a (first actually-a-set)
          s (powerset (disj actually-a-set a))]
      (clojure.set/union s (map #(conj % a) s)))))