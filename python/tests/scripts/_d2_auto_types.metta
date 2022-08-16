!(pragma! type-check auto)

(: = (-> $t $t Type))

(: Entity Type)
(: Human (-> Entity Type))
(: Socrates Entity)
(: Plato Entity)
(: Mortal (-> Entity Type))
(: HumansAreMortal (-> (Human $t) (Mortal $t)))
(: SocratesIsHuman (Human Socrates))
(: SocratesIsMortal (Mortal Socrates))

!(HumansAreMortal SocratesIsHuman)
!(HumansAreMortal (Human Socrates))
!(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))
(: PlatoIsHuman (Human Plato))
!(= SocratesIsMortal (HumansAreMortal PlatoIsHuman))
!(= (Human Socrates) (Human Plato))
