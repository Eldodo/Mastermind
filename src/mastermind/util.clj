(ns mastermind.util
  (:gen-class)
  (:use [clojure.string]))


;; ## code secret

(declare code-secret)

(def colors #{:rouge :bleu :vert :jaune :noir :blanc})

(defn code-secret [n]
  (loop [i n res []]
    (if (zero? i)
      res
      (recur (dec i) (conj res (rand-nth (seq colors)))))))



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

;; ## parseInt

(declare parse-int)
(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s ))
    (catch NumberFormatException e (Integer. 0))))

;; ## color to string

(declare color-to-str)

(defn color-to-str [color]
  (case color
    :rouge "rouge"
    :bleu "bleu"
    :blanc "blanc"
    :noir "noir"
    :jaune "jaune"
    :vert "vert"
     "default"))
(defn str-to-color [str]
  (case str
    "rouge" :rouge
    "bleu" :bleu
    "blanc" :blanc
    "noir" :noir
    "jaune" :jaune
    "vert" :vert
    nil))

;; ## affiche code

(declare affiche-code)

(defn affiche-code [code]
  (print "[")
  (loop [code code]
    (if (seq code)
      (do
        (if (seq (rest code))
          (print (color-to-str (first code))" ")
          (print (color-to-str (first code))))
        (recur (rest code)))
      (println "]"))))

(declare verif-essai)

(defn verif-essai [essai]
  (loop [essai essai res []]
    (if (seq essai)
      (if (str-to-color (first essai))
        (recur (rest essai) (conj res (str-to-color (first essai))))
        nil)
      res)))

(declare demande-essai)

(defn demande-essai [taille]
  (println "Veuillez entrer une combinaison de"taille"couleurs rouge, vert, jaune, bleu, noir et blanc:")
  (loop [essai (split (read-line) #" ")]
    (if (= taille (count essai))
      (let [res (verif-essai essai)]
        (if (seq res)
          res
          (do (println "Essai non valide. Réessayez:")
            (recur (split (read-line) #" ")))))
      (do (println "Essai non valide. Réessayez:")
        (recur (split (read-line) #" "))))))

(declare correcte)
(defn correcte [indic]
  (loop [i indic]
    (if (seq i)
      (if (= (first i) :good)
        (recur (rest i))
        false)
      true)))

(declare affiche-indic)
(defn affiche-indic [indic]
  (print "[")
  (loop [i indic]
    (if (seq i)
      (do
        (case (first i)
          :good (print "good")
          :color (print "color")
          :bad (print "bad"))
        (if (seq (rest i))
          (print " ")
          ())
        (recur (rest i)))
      (println "]"))))
