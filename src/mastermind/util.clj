(ns mastermind.util
  (:gen-class)
  (:use [clojure.string]))


;; ## les différentes couleurs possibles

(def colors #{:rouge :bleu :vert :jaune :noir :blanc})

;; ## code secret, retourne un code secret de taille n coposé des couleurs présentes dans colors

(declare code-secret)

(defn code-secret [n]
  (loop [i n res []]
    (if (zero? i)
      res
      (recur (dec i) (conj res (rand-nth (seq colors)))))))



;; ## contient?, retourne true si elt est dans v, false sinon

(defn contient? [v elt]
  (loop [v v]
    (if (seq v)
      (if (= (first v) elt)
        true
        (recur (rest v)))
      false)))


;; ## indications, retourne l'indication correspondant à l'essai (ess)

(declare indications)

(defn indications [code ess]
  (if (= (count code) (count ess)) ;; test si essai est de la meme taille que code, si false la fonction retourne nil
    (loop [i 0 res []]
      (if (= i (count code))
        res
        (if (= (nth code i) (nth ess i)) ;; test si la couleur est bien placée
          (recur (inc i) (conj res :good))
          (if (contient? code (nth ess i)) ;; test si la couleur est dans le code
            (recur (inc i) (conj res :color))
            (recur (inc i) (conj res :bad))))))
    nil))

;; ## fréquences, retourne une map qui a pour clé chaque élément distinct de v et lui associe pour valeur son nombre d'occurence dans v

(declare frequences)

(defn frequences [v]
  (loop [v v res {}]
    (if (seq v)
      (if (contains? res (first v))
        (recur (rest v) (assoc res (first v) (inc (get res (first v)))))
        (recur (rest v) (assoc res (first v) 1)))
      res)))

;; ## freqs-dispo, retourne une map qui associe à chaque couleur du code sa fréquence disponible par rapport à une indication
;; ## la frequence d'une couleur augmente de 1 si elle est associée à un bad ou un color, et est initialisée à 0 ou reste identique si associée à un good

(declare freqs-dispo)

(defn freqs-dispo [code vindic]
  (if (= (count code) (count vindic)) ;; test si code est de la meme taille que vindic, si false la fonction retourne nil
    (loop [code code vindic vindic res {}]
      (if (seq code)
        (if (= nil (get res (first code))) ;; test si la clé (first code) est absente de la map de retour
          (if (= (first vindic) :good)
            (recur (rest code) (rest vindic) (conj res [(first code) 0])) ;; clé absente dans la map de retour et couleur bien placée dans l'essai
            (recur (rest code) (rest vindic) (conj res [(first code) 1]))) ;; clé absente dans la map de retour et erreur dans essai, incrémente la valeur de 1
          (if (= (first vindic) :good)
            (recur (rest code) (rest vindic) res) ;; clé présente dans la map de retour et couleur bien placée
            (recur (rest code) (rest vindic) (assoc res (first code) (inc (get res (first code))))))) ;; clé présente dans la map de retour et erreur dans essai, incrémente la valeur de 1
        res))
    nil))


;; ## filtre-indications, filtre l'indication, correspondant à l'essai, par cardinalité, retourne un nouveau vecteur indication
;; ## Dans indic :good et :bad ne sont pas modifiés, si :color est présente alors on test si la fréquence (freq-dispo) de la couleur associée dans essai est nul
;; ## si oui alors on remplace :color par :bad
;; ## si non on laisse :color et on diminue la fréquence de la couleur de 1

(declare filtre-indications)

(defn filtre-indications [code essai indic]
  (loop [code code essai essai indic indic freq (freqs-dispo code indic) res []]
    (if (seq code)
      (if (= (first indic) :good)
        (recur (rest code) (rest essai) (rest indic) freq (conj res :good)) ;; cas :good
        (if (= (first indic) :bad)
          (recur (rest code) (rest essai) (rest indic) freq (conj res :bad)) ;; cas :bad
          (if (zero? (get freq (first essai)))
            (recur (rest code) (rest essai) (rest indic) freq (conj res :bad)) ;; cas :color avec fréquence nul
            (recur (rest code) (rest essai) (rest indic) (assoc freq (first essai) (dec (get freq (first essai)))) (conj res :color))))) ;; cas :color avec fréquence non nul
      res)))

;; ## parseInt, convertit en entier le string donné en entré et le retourne, retourne 0 si l'entré est erroné

(declare parse-int)
(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s ))
    (catch NumberFormatException e (Integer. 0)))) ;; entré erroné, NumberFormatException, retourne 0


;; ## color to string, prend un color en entré et retourne le string associé, retourne default si color erroné

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


;; ## str-to-color, prend un string en entré et retourne un color si le string est correct, nil sinon

(declare str-to-color)

(defn str-to-color [str]
  (case str
    "rouge" :rouge
    "bleu" :bleu
    "blanc" :blanc
    "noir" :noir
    "jaune" :jaune
    "vert" :vert
    nil))

;; ## affiche code, affiche le code donné en entré

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

;; ## verif-essai, prend un essai en entré et retourne nil si le format est incorrect, sinon retourne l'essai en convertissant les string en "color"

(declare verif-essai)

(defn verif-essai [essai]
  (loop [essai essai res []]
    (if (seq essai)
      (if (str-to-color (first essai)) ;; str-to-color retourne nil si (first essai) est erroné
        (recur (rest essai) (conj res (str-to-color (first essai))))
        nil)
      res)))


;; ## demande-essai, demande à l'utilisateur une tentative de combinaison de couleurs et vérifie si le format est valide
;; ## retourne un essai valide avec ses éléments convertient en color

(declare demande-essai)

(defn demande-essai [taille]
  (println "Veuillez entrer une combinaison de"taille"couleurs rouge, vert, jaune, bleu, noir et blanc:")
  (loop [essai (split (read-line) #" ")]
    (if (= taille (count essai)) ;; test si essai est de la meme taille que code
      (let [res (verif-essai essai)] ;; res = nil si essai erroné, sinon res = essai (avec les éléments convertient en color)
        (if (seq res) ;; if res != nul
          res ;; cas essai valide, retourne res
          (do (println "Essai non valide. Réessayez:") ;; cas essai erroné
            (recur (split (read-line) #" ")))))
      (do (println "Essai non valide. Réessayez:") ;; cas où l'essai n'est pas de la bonne taille
        (recur (split (read-line) #" "))))))

;; ## correcte, retourne true si l'indication ne contient que des :good (couleur bien placée), faux sinon

(declare correcte)

(defn correcte [indic]
  (loop [i indic]
    (if (seq i)
      (if (= (first i) :good) ;; test si (first i) vaut :good
        (recur (rest i))
        false) ;; element différent de :good dans indic, retourne false
      true)))


;; ## affiche-indic, permet d'afficher l'indication d'un essai

(declare affiche-indic)

(defn affiche-indic [indic]
  (print "[")
  (loop [i indic]
    (if (seq i)
      (do
        (case (first i) ;; affiche le string correspondant à (first i)
          :good (print "good")
          :color (print "color")
          :bad (print "bad"))
        (if (seq (rest i))
          (print " ")
          ())
        (recur (rest i)))
      (println "]"))))
