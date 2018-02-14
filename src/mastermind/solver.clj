(ns mastermind.solver
  (:gen-class)
  (:use [mastermind.util]))


;; ## solver trouve une solution en maximum 6 essais
;; ## effectue des tentatives aléatoire en filtrant à chaque essai les couleurs possible/impossible pour chaque indice du code

(declare solver)

(defn solver [code]
  (loop [code code
         vecset (vecset (count code)) ;; initialise le vecteur d'ensembles avec toutes les couleurs
         essai (solv-essai vecset) ;; première tentative
         indic (filtre-indications code essai (indications code essai)) ;; recupère l'indication correspondant à l'essai
         nbtry 1]
    (if (correcte indic) ;; vérifie si l'essai correspond au code
      (do
        (println "Solution trouvée en" nbtry"tentatives, dernier essai:")
        (affiche-code essai)
        (println "Le code était:")
        (affiche-code code))
      (let [vecsetTmp (majvecset vecset essai indic) ;; met à jour le vecteur d'ensembles
            essaiTmp (solv-essai vecsetTmp) ;; nouvel essai
            indicTmp (filtre-indications code essaiTmp (indications code essaiTmp))] ;; nouvelle indication
        (recur code vecsetTmp essaiTmp indicTmp (inc nbtry))))))






