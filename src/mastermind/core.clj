(ns mastermind.core
  (:use [mastermind.jeu :as jeu])
  (:gen-class))

(defn lire-entier []
  (let [val (read-line)]
  (loop [val val]
    (if (< (parse-int val) 1)
      (do
        (println "Veuillez saisir un entier positif:")
        (recur (read-line))
        )
      (parse-int val)))))

(defn -main
  [& args]
  (println "Saisir la taille du code secret a deviner:")
  (def val (lire-entier))
  (println(format "Le code est de taille %d." val))
)





