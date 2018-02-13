(ns mastermind.jeu
  (:gen-class))

;; ## code secret

(declare code-secret)

(defn code-secret [n]
  (loop [i n res []]
    (if (zero? i)
      res
      (recur (dec i) (conj res (rand-nth [:rouge :bleu :vert :jaune :noir :blanc]))))))



;; ## indications

(declare indications)

(defn contient? [v elt]
  (loop [v v]
    (if (seq v)
      (if (= (first v) elt)
        true
        (recur (rest v)))
      false)))

(defn indications [code ess]
  (if (= (count code) (count ess))
    (loop [i 0 res []]
      (if (= i (count code))
        res
        (if (= (nth code i) (nth ess i))
          (recur (inc i) (conj res :good))
          (if (contient? code (nth ess i))
            (recur (inc i) (conj res :color))
            (recur (inc i) (conj res :bad))))))
    nil))

;; ## fréquences

(declare frequences)

(defn frequences [v]
  (loop [v v res {}]
    (if (seq v)
      (if (contains? res (first v))
        (recur (rest v) (assoc res (first v) (inc (get res (first v)))))
        (recur (rest v) (assoc res (first v) 1)))
      res)))

;; ## fréquences disponibles

(declare freqs-dispo)


(defn freqs-dispo [code vindic]
  (if (= (count code) (count vindic))
    (loop [code code vindic vindic res {}]
      (if (seq code)
        (if (= nil (get res (first code)))
          (if (= (first vindic) :good)
            (recur (rest code) (rest vindic) (conj res [(first code) 0]))
            (recur (rest code) (rest vindic) (conj res [(first code) 1])))
          (if (= (first vindic) :good)
            (recur (rest code) (rest vindic) res)
            (recur (rest code) (rest vindic) (assoc res (first code) (inc (get res (first code)))))))
        res))
    nil))

;; ## filtrer par cadinalité

(declare filtre-indications)

(defn filtre-indications [code essai indic]
  (loop [code code essai essai indic indic freq (freqs-dispo code indic) res []]
    (if (seq code)
      (if (= (first indic) :good)
        (recur (rest code) (rest essai) (rest indic) freq (conj res :good))
        (if (= (first indic) :bad)
          (recur (rest code) (rest essai) (rest indic) freq (conj res :bad))
          (if (zero? (get freq (first essai)))
            (recur (rest code) (rest essai) (rest indic) freq (conj res :bad))
            (recur (rest code) (rest essai) (rest indic) (assoc freq (first essai) (dec (get freq (first essai)))) (conj res :color)))))
      res)))


(declare parse-int)

(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s ))
    (catch NumberFormatException e (Integer. 0))))




