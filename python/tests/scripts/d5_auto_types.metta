!(pragma! type-check auto)

; checking grounded types
!(assertEqualToResult
   (+ 5 "S")
  ((Error (+ 5 "S") BadType)))

!(assertEqualToResult
   (== 5 "S")
  ((Error (== 5 "S") BadType)))

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
  ((Error (== SocratesIsHuman SamIsMortal) BadType)))
!(assertEqualToResult
   (HumansAreMortal (Human Socrates))
  ((Error (HumansAreMortal (Human Socrates)) BadType)))


; FIXME? Evaluation of `(+ 5 "S")` inside `collapse` leads to
; `(Error ...)`, but it is evaluated further, and the interpreter
; returns an empty result since the expression is badly typed,
; eventually, the interpreter keeps `collapse` unreduced
!(assertEqualToResult
   (collapse (+ 5 "S"))
  ((collapse (+ 5 "S"))))
; the fix is to have the argument of `Error` of `Atom` type
(: Error (-> Atom Atom Atom))
; we'll got the error, although `collapse` will additionally
; wrap it into expression
!(assertEqualToResult
   (collapse (+ 5 "S"))
  (((Error (+ 5 "S") BadType))))

; `match` can accept badly typed expressions without type error,
; which is correct
(match &self (== SocratesIsHuman SamIsMortal) Really?)

; FIXME? These type-check
(let $x (+ 5 "S") $x)
(: f (-> $t Number))
(f (+ 5 "S"))


