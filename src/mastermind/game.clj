(ns mastermind.game
  (:gen-class)
  (:use [mastermind.util])
 (:use [clojure.string :only (split)]))

;; ## Lance le jeu mastermind

(declare mastermind)

(defn mastermind [code nbtryy]
  (println "Vous avez" nbtryy"tentatives pour trouver le code secret !")
  (loop [code code nbtry nbtryy]
    (if (zero? nbtry) ;; test s'il reste des tentatives
      (println "Vous n'avez pas trouvé dans le nombre de tentatives imparties, vous avez perdu!") ;; plus de tentative restante
      (let [essai (demande-essai (count code)) ;; demande une tentative à l'utilisateur
            indic (filtre-indications code essai (indications code essai))] ;; récupère l'indication correspondant à la tentative
        (if (correcte indic)
          (do ;; cas code secret découvert
            (let [trys (+ 1 (- nbtryy nbtry))]
            (println "Bravo vous avez trouvé le code secret en"trys"tentatives !")
            (println "Le code était:")
            (affiche-code code)))
          (do ;; cas code secret non découvert
            (affiche-indic indic) ;; affiche l'indication correspondant à la tentative
            (println "Arrêter la partie? y/n") ;; demande à l'utilisateur s'il veut arrêter la partie
            (if (= (nth (split (read-line) #"\n") 0) "y")
              (do ;; cas où l'utilisateur arrête la partie, affiche le code avant de mettre fin à la partie
                (println "Vous déclarez forfait, le code était:")
                (affiche-code code))
              (recur code (dec nbtry))))))))) ;; tentative suivante


