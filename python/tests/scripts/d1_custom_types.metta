;; In this example a custom inhabitation operator :: is introduced.
;; Here :: only differs from : to underline that this is not a
;; built-in typing operator, but rather defined by custom expressions
;; in pure MeTTa.

(= (:? $c)
   (match &self (:: $c $t) $t))
(= (:? ($c $a))
   (match &self (:: $c (-> (:? $a) $t)) $t))
(= (:? ($c $a $b))
   (match &self (:: $c (-> (:? $a) (:? $b) $t)) $t))

(:: = (-> $t $t Type))

(:: Entity Type)
(:: Human (-> Entity Type))
(:: Socrates Entity)
(:: Plato Entity)
(:: Mortal (-> Entity Type))
(:: HumansAreMortal (-> (Human $t) (Mortal $t)))
(:: SocratesIsHuman (Human Socrates))
(:: SocratesIsMortal (Mortal Socrates))

(:: Sam Entity)
(:: Frog (-> Entity Prop))
(:: Green (-> Entity Prop))
(:: Croaks (-> Entity Prop))
(:: GreenAndCroaksIsFrog (-> (Green $t) (Croaks $t) (Frog $t)))
(:: SamIsGreen (Green Sam))
(:: SamCroaks (Croaks Sam))

!(println! (:? (HumansAreMortal SocratesIsHuman)))
;!(println! (:? (HumanAreMortal (Human Socrates))))
;!(println! (:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman))))
;!(println! (:? (= SamIsGreen (HumansAreMortal SocratesIsHuman))))
;!(println! (:? (= (Human Socrates) (Human Plato))))

;(:: x (Frog Sam))
;!(println! (:? (= x (GreenAndCroaksIsFrog SamIsGreen SamCroaks))))

(:: List (-> Type Type))
(:: Nil (List $a))
(:: Cons (-> $a (List $a) (List $a)))
;!(println! (:? (Cons Socrates (Cons Plato Nil))))
;!(println! (:? (Cons Socrates (Cons SocratesIsHuman Nil))))
