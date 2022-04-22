!(pragma! type-check auto)

(: T Type)
(: = (-> $t $t Type))
(= (= $x $x) T)

(: Entity Type)
(: Human (-> Entity Type))
(: Socrates Entity)
(: Plato Entity)
(: Mortal (-> Entity Type))
(= (Human Socrates) T)
(= (Human Plato) T)
(= (Mortal Socrates) T)

!(HumanAreMortal (Human Socrates))
!(= (Human Socrates) (Human Plato))

(: HumansAreMortal (-> (Human $t) (Mortal $t)))
(: SocratesIsHuman (Human Socrates))
(: SocratesIsMortal (Mortal Socrates))
!(println! (HumansAreMortal SocratesIsHuman))
;!(println! (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))

(= (Mortal $x) (Human $x))
;!(println! (Mortal Plato))

(: if (-> Type $t $t))
(= (if T $then) $then)
;!(println! (if (Mortal $x) $x))

!(pragma! type-check none)
