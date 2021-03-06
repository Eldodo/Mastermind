(ns mastermind.util
  (:gen-class)
  (:use [clojure.string :only (split)])
  (:use [midje.sweet]))


;; ## les différentes couleurs possibles

(def colors #{:rouge :bleu :vert :jaune :noir :blanc})


;; ## code secret, retourne un code secret de taille n coposé des couleurs présentes dans colors

(declare code-secret)

(defn code-secret [n]
  (loop [i n res []]
    (if (zero? i)
      res
      (recur (dec i) (conj res (rand-nth (seq colors)))))))

;; # test pour code-secret

(fact "Le `code-secret` est bien composé de couleurs."
      (every? colors
              (code-secret 4))
      => true)

(fact "Le `code-secret` est bien composé de couleurs."
      (every? colors
              (code-secret 8))
      => true)

(fact "Le `code-secret` a l'air aléatoire."
      (> (count (filter true? (map not=
                                   (repeatedly 20 #(code-secret 5))
                                   (repeatedly 20 #(code-secret 6))
                                   (repeatedly 20 #(code-secret 7)))))
         0)
      => true)




;; ## contient?, retourne true si elt est dans v, false sinon

(declare contient?)

(defn contient? [v elt]
  (loop [v v]
    (if (seq v)
      (if (= (first v) elt)
        true
        (recur (rest v)))
      false)))

;; # test pour contient?

(fact "Contient? retourne bien true si l'élément est présent"
      (= (contient? [1 2 3 4 5 6 7] 5) true)
      => true)

(fact "Contient? retourne bien false si l'élément est absent"
      (= (contient? [1 2 3 4 5 6 7] "bonjour") false)
      => true)




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

;; # test pour indications


(fact "`indications` sont les bonnes."
      (indications [:rouge :rouge :vert]
                   [:vert :rouge :bleu :jaune])
      => nil

      (indications [:rouge :rouge :vert :bleu]
                   [:bleu :rouge :vert :jaune])
      => [:color :good :good :bad]

      (indications [:rouge :rouge :vert :bleu]
                   [:rouge :rouge :vert :bleu])
      => [:good :good :good :good]

      (indications [:rouge :rouge :vert :vert]
                   [:vert :bleu :rouge :jaune])
      => [:color :bad :color :bad]

       (indications [:rouge :rouge :vert :vert]
                   [:vert])
      => nil

      (indications [:rouge :violet :vert  :vert  :jaune :orange 0      "bonjour"]
                   [:vert  :bleu   :rouge :jaune :rouge :blanc  :vert  "bonjour"])
      =>           [:color :bad    :color :color :color :bad    :color :good])




;; ## fréquences, retourne une map qui a pour clé chaque élément distinct de v et lui associe pour valeur son nombre d'occurence dans v

(declare frequences)

(defn frequences [v]
  (loop [v v res {}]
    (if (seq v)
      (if (contains? res (first v))
        (recur (rest v) (assoc res (first v) (inc (get res (first v)))))
        (recur (rest v) (assoc res (first v) 1)))
      res)))

;; # test pour frequences

(fact "les `frequences` suivantes sont correctes."
      (frequences [:rouge :rouge :vert :bleu :vert :rouge])
      => {:rouge 3 :vert 2 :bleu 1}

      (frequences [:rouge :vert :bleu])
      => {:rouge 1 :vert 1 :bleu 1}

      (frequences [1 2 3 2 1 4]) => {1 2, 2 2, 3 1, 4 1}

      (frequences [:rouge :vert :bleu "bonjour" 5 {}])
      => {:rouge 1 :vert 1 :bleu 1 "bonjour" 1 5 1 {} 1})




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

;; # test pour freqs-dispo

(fact "Les fréquences disponibles de `freqs-dispo` sont correctes."
      (freqs-dispo [:rouge :rouge :bleu :vert :rouge]
                   [:good :color :bad :good :color])
      => {:bleu 1, :rouge 2, :vert 0}

      (freqs-dispo [:rouge :rouge :bleu :vert :rouge]
                   [:good :color :bad :good])
      => nil

      (freqs-dispo [:rouge :rouge :bleu :vert]
                   [:good :color :bad :good :color])
      => nil

      (freqs-dispo [:vert :bleu :violet :rose :orange "bonjour"]
                   [:good :color :bad :good :color :bad])
      => {:bleu 1, :violet 1, :vert 0, :rose 0, :orange 1, "bonjour" 1})




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

;; # test pour filtre-indications

(fact "Le `filtre-indications` fonctionne bien."
      (filtre-indications [:rouge :rouge :vert :bleu]
                          [:vert :rouge :bleu :jaune]
                          [:color :good :color :bad])
      => [:color :good :color :bad]

      (filtre-indications [:rouge :vert :rouge :bleu]
                          [:rouge :rouge :bleu :rouge]
                          [:good :color :color :color])
      => [:good :color :color :bad]

      (filtre-indications [:rouge :vert :rouge :orange :vert :violet]
                          [:rouge :rouge :bleu :rouge :orange :violet]
                          [:good :color :bad :color :color :good])
      => [:good :color :bad :bad :color :good])




;; ## parseInt, convertit en entier le string donné en entrée et le retourne, retourne 0 si l'entré est erroné

(declare parse-int)
(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s ))
    (catch NumberFormatException e (Integer. 0)))) ;; entrée erroné, NumberFormatException, retourne 0

;; # test pour parse-int

(fact "parse-int fonctionne bien"
      (parse-int "grtd45erd7")
        => 45

      (parse-int "juhzed")
        => 0

      (parse-int "12")
        => 12)




;; ## color to string, prend un color en entrée et retourne le string associé, retourne default si color erroné

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

;; # test pour color-to-str

(fact "color-to-str retourne bien la bonne chaine"
      (color-to-str :rouge)
      => "rouge"

      (color-to-str :violet)
      => "default")




;; ## str-to-color, prend un string en entrée et retourne un color si le string est correct, nil sinon

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

;; # test pour str-to-color

(fact "str-to-color retourne bien la bonne couleur/nil"
      (str-to-color "bleu")
      => :bleu

      (str-to-color "vertbleurouge")
      => nil)




;; ## affiche code, affiche le code donné en entrée

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


;; ## verif-essai, prend un essai en entrée et retourne nil si le format est incorrect, sinon retourne l'essai en convertissant les string en "color"

(declare verif-essai)

(defn verif-essai [essai]
  (loop [essai essai res []]
    (if (seq essai)
      (if (str-to-color (first essai)) ;; str-to-color retourne nil si (first essai) est erroné
        (recur (rest essai) (conj res (str-to-color (first essai))))
        nil)
      res)))

;; # test pour verif-essai

(fact "verif-essai retourne bien nil ou l'essai convertit en color"
      (verif-essai ["rouge" "bleu" "vert"])
      => [:rouge :bleu :vert]

      (verif-essai ["red" "blue"])
      => nil)




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

;; # pas de test pour demande-essai qui demande une entrée utilisateur (read-line)


;; ## correcte, retourne true si l'indication ne contient que des :good (couleur bien placée), faux sinon

(declare correcte)

(defn correcte [indic]
  (loop [i indic]
    (if (seq i)
      (if (= (first i) :good) ;; test si (first i) vaut :good
        (recur (rest i))
        false) ;; element différent de :good dans indic, retourne false
      true)))

;; # test pour correcte

(fact "correcte fonctionne correctement"
      (correcte  [:good :good :good])
      => true

      (correcte [:color :bad :good])
      => false)





;; ## affiche-indic, permet d'afficher l'indication d'un essai

(declare affiche-indic)

(defn affiche-indic [indic]
  (loop [i indic good 0 color 0]
    (if (seq i)
        (case (first i)
          :good (recur (rest i) (inc good) color)
          :color (recur (rest i) good (inc color))
          :bad (recur (rest i) good color))
        (println good "couleur(s) bien placée(s) et" color " mal placée(s)"))))



;; ################################################
;; #### suite : fonctions pour solveur uniquement #
;; ################################################


;; ## vecset, retourne un vecteur de taille n

(declare vecset)

(defn vecset [taille]
  (loop [n (- (count colors) 1) res []]
    (if (= -1 n)
      res
      (recur (dec n) (conj res [0 (nth (seq colors) n) 0 (vec (take taille (repeat 0)))])))))
;;conj [int=0 "une couleur" int=0 [vecteur de la taille du code à trouver initialisé à 0]
;;premier int=0: couleur non encore testé, 1:couleur testée et présente dans le code, 2 couleur (testé ou pas) non présente
;;"une couleur" la couleur associée
;;second int: nombre d'occurence de la couleur dans le code
;;[vecteur] représente les positions potentielles de la couleur [0=non testé, 1=à determiner, 2=bonne position
;;3=mauvaise position]


(fact "vecset génére bien ce qu'on veut pour un code de taille n"
      (vecset 4)
      => [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]]
      (vecset 2)
      => [[0 :blanc 0 [0 0]] [0 :vert 0 [0 0]] [0 :noir 0 [0 0]]
          [0 :bleu 0 [0 0]] [0 :rouge 0 [0 0]] [0 :jaune 0 [0 0]]])


;; ## assocVec modifie l'entier indiquant le nombre d'occurence d'une couleur

(declare assocVec)

(defn assocVec [v indice valeur]
  (loop [v v i indice res []]
    (if (seq v)
      (if (zero? i)
        (recur (rest v) (dec i) (conj res (assoc (first v) 2 valeur)))
        (recur (rest v) (dec i) (conj res (first v))))
      res)))

(fact "assocVec modifie bien le nombre d'occurence de la i-ième couleur de v"
      (assocVec [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 2 3)

      => [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 3 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])

;; ## getNbOcc retourne le nombre d'occurence d'une couleur dans le code

(declare getNbOcc)

(defn getNbOcc [v indice]
  (loop [v v i indice]
    (if (zero? i)
      (nth (first v) 2)
      (recur (rest v) (dec i)))))

(fact "getNbOcc retourne bien le nombre d'occurence de la i-ième couleur de v"
      (getNbOcc  [[2 :blanc 0 [0 0 0 0]] [2 :vert 0 [0 0 0 0]] [1 :noir 3 [0 0 0 0]]
          [2 :bleu 0 [0 0 0 0]] [2 :rouge 0 [0 0 0 0]] [1 :jaune 1 [0 0 0 0]]]  2)
      => 3)


;; ## getCode retourne le code correpond à la map donnée en paramètre
;; map de la forme {indice0 :color, indice1 :color}

(declare getCode)

(defn getCode [s taille]
  (loop [s s i 0 res []]
    (if (= taille i)
      res
      (recur s (inc i) (conj res (get s i))))))

(fact "getCode retourne bien le code correspondant à s"
      (getCode {0 :blanc, 1 :noir, 3 :rouge, 2 :vert} 4)
      => [:blanc :noir :vert :rouge])

;; ## getPositionValide retourne le premier indice où on peut mettre la couleur

(declare getPositionValide)

(defn getPositionValide [vecset indice setOccupe taille] ;;setOccupe : ensemble des indices déjà occupés
  (loop [v vecset i indice]
    (if (zero? i)
      (loop [vect (nth (first v) 3) ind 0]
        (if (= ind taille)
          nil
          (if (contains? setOccupe ind)
            (recur vect (inc ind))
            (if (> 3 (nth vect ind))
              ind
              (recur vect (inc ind))))))
      (recur (rest v) (dec i)))))

(fact "retourne bien le premier indice valide"
      (getPositionValide (vecset 4) 1 #{} 4)
      => 0
      (getPositionValide (vecset 4) 1 #{0 1} 4)
      => 2
      (getPositionValide [[2 :blanc 0 [0 0 0 0]] [1 :vert 2 [3 3 2 2]] [1 :noir 1 [0 0 0 0]]
          [2 :bleu 0 [0 0 0 0]] [2 :rouge 0 [0 0 0 0]] [1 :jaune 2 [0 0 0 0]]] 1 #{} 4)
      => 2
      (getPositionValide [[2 :blanc 0 [0 0 0 0]] [1 :vert 2 [3 3 2 2]] [1 :noir 1 [0 0 0 0]]
          [2 :bleu 0 [0 0 0 0]] [2 :rouge 0 [0 0 0 0]] [1 :jaune 2 [0 0 0 0]]] 1 #{2} 4)
      => 3)

;; ## getIndication retourne le nombre de couleurs bien placées et mal placées

(declare get-indic)

(defn get-indic [indic]
  (loop [i indic good 0 color 0]
    (if (seq i)
        (case (first i)
          :good (recur (rest i) (inc good) color)
          :color (recur (rest i) good (inc color))
          :bad (recur (rest i) good color))
        [good color])))

(fact "get-indic retourne bien le nombre de couleurs bien placées et mal placées"
      (get-indic [:bad :good :color :color])
      => [1 2])


;; ## getIndexOfColorInVecset : retourne l'indice de la couleur color dans vecset

(declare getIndexOfColorInVecset)

(defn getIndexOfColorInVecset [color vecset]
  (loop [i 0 v vecset]
    (if (= color (second (first v)))
      i
      (recur (inc i) (rest v)))))

(fact "retourne bien l'indice de la couleur dans le vecteur donnée"
      (getIndexOfColorInVecset :bleu [[0 :blanc 0 [0 0]] [0 :vert 0 [0 0]] [0 :noir 0 [0 0]]
          [0 :bleu 0 [0 0]] [0 :rouge 0 [0 0]] [0 :jaune 0 [0 0]]])
      => 3
      (getIndexOfColorInVecset :bleu [[0 :blanc 0 [0 0]] [0 :vert 0 [0 0]] [0 :noir 0 [0 0]]
          [0 :jaune 0 [0 0]] [0 :rouge 0 [0 0]] [0 :bleu 0 [0 0]]])
      => 5)


;; ## conjPoscol, associe à l'indice ind du code les couleurs possibles, retourne le vecteur contenant ces couleurs

(declare conjPoscol)

(defn conjPoscol [vecset ind]
  (loop [vecs vecset res []]
    (if (seq vecs)
      (if (> 2 (first (first vecs)))
        (if (> 3 (nth (nth (first vecs) 3) ind))
          (recur (rest vecs) (conj res (second (first vecs))))
          (recur (rest vecs) res))
        (recur (rest vecs) res))
      res)))

(fact "conjPoscol associe bien des couleurs valide pour l'indice correspondant"
      (conjPoscol  [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 1)
      => [:blanc :vert :noir :bleu :rouge :jaune]
      (conjPoscol [[0 :blanc 0 [3 0]] [0 :vert 0 [1 0]] [0 :noir 0 [1 0]]
          [0 :bleu 0 [3 0]] [0 :rouge 0 [3 0]] [0 :jaune 0 [1 0]]] 0)
      => [:vert :noir :jaune])


;; ## mapPosCol, retourne une map qui à chaque indice du code associe les couleurs possibles

(declare mapPosCol)

(defn mapPosCol [vecset taille]
  (loop [vecset vecset i 0 res {}]
    (if (= taille i)
      res
      (recur vecset (inc i) (conj res [i (conjPoscol vecset i)])))))


(fact "la map retournée est correct"
      (mapPosCol [[0 :blanc 0 [3 0]] [0 :vert 0 [1 0]] [0 :noir 0 [1 0]]
          [0 :bleu 0 [3 0]] [0 :rouge 0 [3 0]] [0 :jaune 0 [1 0]]] 2)
      =>{0 [:vert :noir :jaune], 1 [:blanc :vert :noir :bleu :rouge :jaune]})



;; getEssai, retourne un essai "aléatoire" avec mapPosCol, l'essai ne contient que les couleurs du code

(declare getEssai)

(defn getEssai [vecset mapPos taille]
  (loop [vecs vecset mP mapPos i 0 res []]
    (if (= i taille)
      res
      (do
        (let [col (rand-nth (get mP i))
              index (getIndexOfColorInVecset col vecs)
              nbOcc (getNbOcc vecs index)]
          (if (zero? nbOcc) ;; essai invalide on recommence
            (recur vecset mapPos 0 [])
            (recur (assocVec vecs index (dec nbOcc)) mP (inc i) (conj res col))))))))


;; ## getNbConnu, retourne le nombre de couleur connu du code

(declare getNbConnu)

(defn getNbConnu [vecset]
  (loop [v vecset res 0]
    (if (seq v)
      (if (= 1 (first (first v)))
        (recur (rest v) (+ res (nth (first v) 2))) ;; ajoute au résultat le nombre d'occurence d'une couleur présente dans le code
        (recur (rest v) res))
      res)))

(fact "getNbConnu retourne bien le nombre de couleur connu du code"
      (getNbConnu (vecset 4))
      => 0
      (getNbConnu [[1 :blanc 2 [1 1 1 1]] [1 :vert 1 [1 1 1 1]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])
      => 3)

;; ## solv-essai, prend un vecset en parametre et retourne un essai

(declare solv-essai)

(defn solv-essai [vecset taille]
  (if (= taille (getNbConnu vecset))
    (getEssai vecset (mapPosCol vecset taille) taille) ;; on connait toute les couleurs du code -> essai "aléatoire"
    (loop [vecs vecset, i 0, setRes {}, ensOccupe #{},nieme 0]
      (if (= i taille)
        (getCode setRes taille)
        (case (first (first (nthrest vecs nieme)))
          0 (do ;; cas couleur non testée -> on remplit le reste de l'essai avec cette couleur
              (let [pos (getPositionValide vecset nieme ensOccupe taille)]
              (recur vecs (inc i) (conj setRes [pos (second (first (nthrest vecs nieme)))]) (conj ensOccupe pos) nieme)))
          1 (do ;; cas couleur déjà testée et présente, on la place dans le code à des positions valides (* son nombre d'occurence)
              (let [nbOcc (getNbOcc vecs nieme)]
                (if (zero? nbOcc)
                  (recur vecs i setRes ensOccupe (mod (inc nieme) (count colors)));; la couleur a été placée "son nombre d'occurence" fois, on passe à la couleur suivante
                  (let [pos (getPositionValide vecset nieme ensOccupe taille)]
                    (if (= nbOcc 1)
                      (recur (assocVec vecs nieme (dec nbOcc)) (inc i) (conj setRes [pos (second (first (nthrest vecs nieme)))]) (conj ensOccupe pos) (mod (inc nieme) (count colors)))
                      (recur (assocVec vecs nieme (dec nbOcc)) (inc i) (conj setRes [pos (second (first (nthrest vecs nieme)))]) (conj ensOccupe pos) nieme))))))
                      ;; (assocVec vecs nieme (dec nbOcc)) diminue de 1 le nombre d'occurence de la couleur dans vecset
          2 (recur vecs i setRes ensOccupe (mod (inc nieme) (count colors)))))))) ;; cas couleur non présente dans le code, on passe à la couleur suivante


;; ## setFalseCol, modifie vecset pour indiquer que la couleur à l'indice donné n'est pas dans le code

(declare setFalseCol)

(defn setFalseCol [vecset indice taille]
  (loop [v vecset i 0 res []]
    (if (= taille i)
      res
      (if (= indice i)
        (recur (rest v) (inc i) (conj res (assoc (first v) 0 2)))
        (recur (rest v) (inc i) (conj res (first v)))))))


(fact "setFalseCol fonctionne correctement"
      (setFalseCol [[1 :blanc 2 [1 1 1 1]] [1 :vert 1 [1 1 1 1]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 2 6)

      => [[1 :blanc 2 [1 1 1 1]] [1 :vert 1 [1 1 1 1]] [2 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])

;; ## setFalsePosIndCol, modifie vecset pour indiquer que la couleur n'est pas à l'indice indiqué dans le code

(declare setFalsePosIndCol)

(defn setFalsePosIndCol [vecset indVec indFalseCol taille];; indVec : indice dans vecset, indFalseCol indice incorrect dans le code pour la couleur
  (loop [v vecset i 0 res []]
    (if (= taille i)
      res
      (if (= indVec i)
        (recur (rest v) (inc i) (conj res [(first (first v)) (second (first v)) (second (rest (first v))) (assoc (nth (first v) 3) indFalseCol 3)]))
        (recur (rest v) (inc i) (conj res (first v)))))))

(fact "setFalsePosIndCol fonctionne correctement"
      (setFalsePosIndCol [[1 :blanc 2 [1 1 1 1]] [1 :vert 1 [1 1 1 1]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 2 3 6)

      => [[1 :blanc 2 [1 1 1 1]] [1 :vert 1 [1 1 1 1]] [0 :noir 0 [0 0 0 3]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])


;; ## setFalsePos, modifie vecset pour indiquer que les positions des couleurs de l'essai sont fausses

(declare setFalsePos)

(defn setFalsePos [vecset essai]
  (loop [v vecset ess essai indice 0]
    (if (seq ess)
        (recur (setFalsePosIndCol v (getIndexOfColorInVecset (first ess) v) indice (count colors)) (rest ess) (inc indice))
        v)))

(fact "setFalsePos fonctionne correctement"
      (setFalsePos [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] [:bleu :blanc :vert :rouge])

      => [[0 :blanc 0 [0 3 0 0]] [0 :vert 0 [0 0 3 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [3 0 0 0]] [0 :rouge 0 [0 0 0 3]] [0 :jaune 0 [0 0 0 0]]])


;; ## getSetColConnu, retourne l'ensemble des couleurs connu du code

(declare getSetColConnu)

(defn getSetColConnu [vecset]
  (loop [v vecset res #{}]
    (if (seq v)
      (if (= 1 (first (first v)))
        (recur (rest v) (conj res (nth (first v) 1)))
        (recur (rest v) res))
      res)))

(fact "getSetColConnu fonctionne correctement"
      (getSetColConnu [[1 :blanc 0 [0 0 0 0]] [2 :vert 0 [0 0 0 0]] [2 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [1 :rouge 0 [0 0 0 0]] [1 :jaune 0 [0 0 0 0]]])
      => #{:blanc :rouge :jaune})



;; ## setColTestee ; modifie vecset pour indiquer que la couleur correspondante a été testé et est présente nb fois dans le code

(declare setColTestee)

(defn setColTestee [vecset indVec nb taille];; indVec : indice dans vecset
   (loop [v vecset i 0 res []]
     (if (= taille i)
       res
       (if (= indVec i)
         (do
           (let [vtmp (assoc (first v) 2 nb)] ;; assoc indique ici : présente nb fois
           (recur (rest v) (inc i) (conj res (assoc vtmp 0 1))))) ;; assoc indique ici : couleur testée et présente dans le code
         (recur (rest v) (inc i) (conj res (first v)))))))

(fact "setColTestee fonctionne correctement"
      (setColTestee [[0 :blanc 0 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 0 2 6)

      => [[1 :blanc 2 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])




;; ## setWhichColTestee ; modifie vecset pour indiquer qu'une nouvelle couleur a été testé et est présente nb fois dans le code

(declare setWhichColTestee)

(defn setWhichColTestee [vecset nb setColConnu essai]
  (loop [ess essai]
    (if (contains? setColConnu (first ess)) ;; la nouvelle couleur n'est pas déjà connu,
      (recur (rest ess))
      (setColTestee vecset (getIndexOfColorInVecset (first ess) vecset) nb (count colors))))) ;; on a trouvé la couleur non connue et testée

(fact "setWhichColTestee fonctionne correctement"
      (setWhichColTestee [[1 :blanc 1 [0 0 0 0]] [0 :vert 0 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]] 1 #{:blanc} [:blanc :vert :vert :vert])

      =>[[1 :blanc 1 [0 0 0 0]] [1 :vert 1 [0 0 0 0]] [0 :noir 0 [0 0 0 0]]
          [0 :bleu 0 [0 0 0 0]] [0 :rouge 0 [0 0 0 0]] [0 :jaune 0 [0 0 0 0]]])




;; ## majvecset, met à jour un vecset à partir d'un essai et de deux indications (nbGood nbColor)

(declare majvecset)

(defn majvecset [vecset essai nbGood nbColor taille nbConnu setColConnu] ;; nbConnu : nombre de couleurs déjà détérminé
  (if (zero? (+ nbGood nbColor))
    (setFalseCol vecset (getIndexOfColorInVecset (first essai) vecset) (count colors)) ;; cas aucune couleur de l'essai n'est dans le code
    (if (= nbConnu (+ nbGood nbColor))
      (loop [v vecset ess essai]
        (if (seq ess)
          (if (contains? setColConnu (first ess))
            (recur v (rest ess)) ;; couleur présente dans le code
            (recur (setFalseCol v (getIndexOfColorInVecset (first ess) v) (count colors)) (rest ess))) ;; couleur non présente dans le code
          (if (zero? nbGood)
            (setFalsePos v essai) ;; aucune couleur bien placée
            v)))
      (if (zero? nbConnu)
        (setColTestee vecset (getIndexOfColorInVecset (first essai) vecset) (+ nbGood nbColor) (count colors)) ;; première couleur trouvée
        (setWhichColTestee vecset (- (+ nbGood nbColor) nbConnu) setColConnu essai))))) ;; nouvelle couleur trouvée



;; ## lire-code, lit un code donné par l'utilisateur en vérifiant son format et retourne le code convertit en color

(declare lire-code)

(defn lire-code []
  (println "Veuillez entrer une combinaison de couleurs rouge, vert, jaune, bleu, noir et blanc:")
  (loop [code (split (read-line) #" ")]
     (let [res (verif-essai code)] ;; on peut utiliser cette fonction pour verifier la validité du code donné et récupérer le code convertit en color
       (if (seq res) ;; if res != nul
          res ;; cas code valide, retourne res
         (do
           (println "Code non valide. Réessayez:")
           (recur (split (read-line) #" ")))))));; code non valide demande une nouvelle saisie

;; fonctions non utilisées qui sont censé générer l'ensemble des permutations d'un vetceur
;; WIP

(defn inject [deb elt fin]
  (lazy-cat deb (conj fin elt)))

(defn permut-1
  ([elt s]
   (lazy-seq (cons (inject '() elt s) (permut-1 [(first s)] elt (rest s)))))
  ([deb elt fin]
   (if (seq fin)
     (lazy-seq (cons (inject deb elt fin) (permut-1 (lazy-cat deb [(first fin)]) elt (rest fin))))
     (lazy-seq (cons (inject deb elt '()) ())))))

(defn permut-2 [elt seqseq]
  (if (seq seqseq)
    (lazy-cat (permut-1 elt (first seqseq)) (permut-2 elt (rest seqseq)))))

(count (permut-2 1 '((2 3 4) (2 4 3) (3 2 4) (3 4 2) (4 2 3) (4 3 2))))

(defn permut
  ([v]
   (if (seq v)
    (lazy-cat (permut-2 (nth v 0) (permut (nthrest v 1))) (permut 1 v))
     ()))
  ([i v]
   (if (< i (count v))
     (lazy-cat (permut-2 (nth v i) (permut (concat (subvec (vec v) 0 i) (nthrest v (inc i))))) (permut (inc i) v))
     ())))

(take 10 (permut [1 2 3 4]))








