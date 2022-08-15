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

!(HumansAreMortal (Human Socrates))
!(= (Human Socrates) (Human Plato))

(: HumansAreMortal (-> (Human $t) (Mortal $t)))
;!(HumansAreMortal (Human Socrates))

(: SocratesIsHuman (Human Socrates))
(: SocratesIsMortal (Mortal Socrates))
;!(HumansAreMortal SocratesIsHuman)
;!(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))

(= (Mortal $x) (Human $x))
;!(Mortal Plato)

(: if (-> Type $t $t))
(= (if T $then) $then)
;!(if (Mortal $x) $x)
