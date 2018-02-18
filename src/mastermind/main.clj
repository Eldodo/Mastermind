(ns mastermind.main
  (:use [mastermind.util])
  (:use [mastermind.game])
  (:use [mastermind.solver])
  (:use [clojure.string :only  (split)])
  (:gen-class))

;; ## demande à l'utilisateur d'entrer un entier strictement positif puis le retourne sous format Integer

(defn lire-entier []
  (let [valeur (read-line)]
  (loop [valeur valeur]
    (if (< (parse-int valeur) 1)
      (do
        (println "Veuillez saisir un entier positif:")
        (recur (read-line))
        )
      (parse-int valeur)))))

;; ## lance le jeu (mastermind), demande la taille du code à générer et deviner puis laisse 50 tentatives au joueur

;(defn -main
  ;[& args]
  ;;(println "Taper j pour jouer ou s pour utiliser le solver")
 ;(if (= (nth (split (read-line) #"\n") 0) "j")
   ; (do ;; lance le jeu
     ; (println "Saisir la taille du code secret à deviner:")
    ;  (def valeur (lire-entier))
    ;  (println (format "Le code est de taille %d." valeur))
    ;  (def code (code-secret valeur))
    ;  (mastermind code 50))
   ;; (do ;; lance le solveur
     ;; (println "Saisir le code à faire deviner (taille >= 1)")
     ;; (def code (lire-code))
    ;;  (println (format "Le code est de taille %d." (count code)))
   ;;   (solver code))))

 (defn -main
  [& args]
   (println "Saisir la taille du code secret à deviner:")
      (def valeur (lire-entier))
      (println (format "Le code est de taille %d." valeur))
      (def code (code-secret valeur))
      (mastermind code 50))
