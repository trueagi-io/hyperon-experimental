!(pragma! type-check auto)

; checking grounded types
!(assertEqualToResult
   (+ 5 "S")
  ((Error "S" BadType)))

!(assertEqualToResult
   (== 5 "S")
  ((Error "S" BadType)))

; checking custom types
(: Entity Type)
(: Socrates Entity)
(: Human (-> Entity Type))
(: Mortal (-> Entity Type))
(: HumansAreMortal (-> (Human $t) (Mortal $t)))
(: SocratesIsHuman (Human Socrates))
(: SamIsMortal (Mortal Sam))
; well typed
(HumansAreMortal SocratesIsHuman)
(== Mortal Human)
; badly typed
!(assertEqualToResult
   (== SocratesIsHuman SamIsMortal)
  ((Error SamIsMortal BadType)))
!(assertEqualToResult
   (HumansAreMortal (Human Socrates))
  ((Error (Human Socrates) BadType)))


; we'll got the error, although `collapse` will additionally
; wrap it into expression
!(assertEqualToResult
   (collapse (+ 5 "S"))
   (((Error "S" BadType))))

; `match` can accept badly typed expressions without type error,
; which is correct
(match &self (== SocratesIsHuman SamIsMortal) Really?)

; internal call doesn't pass type check despite
; it is wrapped by function with type parameter
(let $x (+ 5 "S") $x)
(: f (-> $t Number))
!(assertEqualToResult
   (f (+ 5 "S"))
   ((Error "S" BadType)))

