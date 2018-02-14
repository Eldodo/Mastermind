(ns mastermind.main
  (:use [mastermind.util])
  (:use [mastermind.game])
  (:gen-class))

;; ## demande à l'utilisateur d'entrer un entier strictement positif puis le retourne sous format Integer

(defn lire-entier []
  (let [val (read-line)]
  (loop [val val]
    (if (< (parse-int val) 1)
      (do
        (println "Veuillez saisir un entier positif:")
        (recur (read-line))
        )
      (parse-int val)))))

;; ## lance le jeu (mastermind), demande la taille du code à générer et deviner puis laisse 50 tentatives au joueur

(defn -main
  [& args]
  (println "Saisir la taille du code secret a deviner:")
  (def val (lire-entier))
  (println (format "Le code est de taille %d." val))
  (def code (code-secret val))
  (mastermind code 50))
