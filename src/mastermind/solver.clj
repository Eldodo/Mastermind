(ns mastermind.game
  (:gen-class)
  (:use [mastermind.util]))

(declare solver)

(defn solver [code]
  (loop [code code
         vecset (vecset (count code))
         essai (solv-essai vecset)
         indic (filtre-indications code essai (indications code essai))
         freqs-dispo (freqs-dispo code indic) nbtry 1]
    (if (correcte indic)
      (do
        (println "Solution trouvée en" nbtry", dernier essai:")
        (affiche-code essai)
        (println "Le code était:")
        (affiche-code code))
      (let [vecsetTmp (majvecset vecset essai indic freqs-dispo)
            essaiTmp (solv-essai vecsetTmp)
            indicTmp (filtre-indications code essaiTmp (indications code essaiTmp))]
        (recur code vecsetTmp essaiTmp indicTmp (freqs-dispo code indicTmp) (inc nbtry))))))






