(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                  (if (zero? n)
                    acc
                    (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (== (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         n a-seq]
    (cond
     (empty? n) nil
     (pred (first n)) i
     :else (recur (inc i) (rest n)))))

(defn avg [a-seq]
  (loop [i 0
         sum 0
         seq1 a-seq]
    (cond
     (and (empty? seq1) (== sum 0)) sum
     (empty? seq1) (/ sum i)
     :else (recur (inc i) (+ sum (first seq1)) (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [newset #{}
         n a-seq]
    (if (empty? n)
      newset
      (recur (toggle newset (first n)) (rest n)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (loop [i 2
                f1 1
                f2 0]
           (if (= i n)
             (+ f1 f2)
             (recur (inc i) (+ f1 f2) f1)))))

(defn cut-at-repetition [a-seq]
  (loop [new []
         seq1 a-seq]
    (if (or (empty? seq1) (contains? (set new) (first seq1)))
      new
      (recur (conj new (first seq1)) (rest seq1)))))
