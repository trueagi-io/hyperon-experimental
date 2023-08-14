;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Auto type-checking can be enabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(pragma! type-check auto)

; checking grounded types
!(assertEqualToResult
   (+ 5 "S")
  ((Error "S" BadType)))
!(assertEqualToResult
   (== 5 "S")
  ((Error "S" BadType)))

; declaring some custom types
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

; badly typed examples produce `Error` grounded values describing the issue:
!(assertEqualToResult
   (== SocratesIsHuman SamIsMortal)
  ((Error SamIsMortal BadType)))
!(assertEqualToResult
   (HumansAreMortal (Human Socrates))
  ((Error (Human Socrates) BadType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `collapse` evaluates its argument, so it shows a type error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqualToResult
   (collapse (+ 5 "S"))
   (((Error "S" BadType))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; `match` does not perform type checking on its pattern,
; so it accepts a badly typed expression, which is expected.
; It can be useful to deal with "wrong" MeTTa programs on
; a meta-level in MeTTa itself, so this behavior of `match`
; allows us to write code that analyzes badly typed expressions
; within MeTTa (e.g. for self-reflective genetic programming).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(match &self (== SocratesIsHuman SamIsMortal) Really?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; internal call doesn't pass type check even if wrapped
; by a function with type parameter:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; let binds without type checking, but errors when used
(let $x (+ 5 "S") $x)
(: f (-> $t Number))
!(assertEqualToResult
   (f (+ 5 "S"))
   ((Error "S" BadType)))
