(ns mastermind.game
  (:gen-class)
  (:use [mastermind.util]))

(declare mastermind)

(defn mastermind [code nbtry]
  (affiche-code code)
  (loop [code code nbtry nbtry]
    (if (zero? nbtry)
      (println "Vous n'avez pas trouvé dans le nombre de tentatives imparties, vous avez perdu!")
      (let [essai (demande-essai (count code))
            indic (filtre-indications code essai (indications code essai))]
        (if (correcte indic)
          (do
            (print "Bravo vous avez trouvé le code secret: ")
            (affiche-code code))
          (do
            (affiche-indic indic)
            (println "Arrêter la partie? y/n")
            (if (= (nth (read-line) 0) 'y')
              (do
                (println "Vous déclarez forfait, le code était:")
                (affiche-code code))
              (recur code (dec nbtry)))))))))


