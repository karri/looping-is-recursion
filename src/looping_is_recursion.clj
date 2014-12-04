(ns looping-is-recursion)
(use 'clojure.tools.trace)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                  acc
                  (recur (* acc base) base (dec exp))))]
    (helper 1 base exp))
  )

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
   :else (recur (rest a-seq)))
  )

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (some true? (map empty? [seq1 seq2])) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false)
  )

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         n 0]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) n
     :else (recur (rest seq1) (inc n))))
  )

(deftrace avg [a-seq]
  (loop [seq1 a-seq
         sum 0
         n 0]
    (cond
     (and (empty? seq1) (zero? n)) nil
     (empty? seq1) (/ sum n)
     :else (recur (rest seq1) (+ sum (first seq1)) (inc n))
     ))
  )

(defn parity [a-seq]
  (let [toggle (fn [a-set element]
                 (if (contains? a-set element)
                   (disj a-set element)
                   (conj a-set element)))]
    (loop [seq1 a-seq
           a-set #{}]
      (if (empty? seq1)
        a-set
        (recur (rest seq1) (toggle a-set (first seq1))))))
  )

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

