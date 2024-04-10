; This script demonstrates how one can document MeTTa code and get
; help using the documenatation.

; Let's document a function which has two arguments and returns value.
; One can use `@doc` expression to do it. First argument of the expression is an
; atom being documented. Other arguments describe the atom, describe function
; parameters and return value.
(@doc some-func
     (@desc "Test function")
     (@params (
              (@param "First argument")
              (@param "Second argument")
              ))
     (@return "Return value")
     )
; Function type is required to document the function
(: Arg1Type Type)
(: Arg2Type Type)
(: ReturnType Type)
(: some-func (-> Arg1Type Arg2Type ReturnType))

; `get-doc` function returns a `@doc-formal` expression which contains the full
; documentation of the atom including user defined description and types.
!(assertEqual
   (get-doc some-func)
   (@doc-formal (@item some-func) (@kind function)
               (@type (-> Arg1Type Arg2Type ReturnType))
               (@desc "Test function")
               (@params (
                        (@param (@type Arg1Type) (@desc "First argument"))
                        (@param (@type Arg2Type) (@desc "Second argument"))))
               (@return (@type ReturnType) (@desc "Return value"))))

; Same approach can be used to document single atom of any @kind.
(@doc SomeSymbol (@desc "Test symbol atom having specific type"))
(: SomeSymbol SomeType)

!(assertEqual
   (get-doc SomeSymbol)
   (@doc-formal (@item SomeSymbol) (@kind atom) (@type SomeType)
               (@desc "Test symbol atom having specific type")))

; Grounded atoms are also can be documented using `@doc` expressions. Type of
; the grounded atom is a part of its implementation.
(@doc some-gnd-atom
     (@desc "Test function")
     (@params (
              (@param "First argument")
              (@param "Second argument")
              ))
     (@return "Return value")
     )

; As some-gnd-function is not imported really in this example type is not
; available and @doc-formal contains %Undefined% instead.
!(assertEqual
   (get-doc some-gnd-atom)
   (@doc-formal (@item some-gnd-atom) (@kind function)
               (@type %Undefined%)
               (@desc "Test function")
               (@params (
                        (@param (@type %Undefined%) (@desc "First argument"))
                        (@param (@type %Undefined%) (@desc "Second argument"))))
               (@return (@type %Undefined%) (@desc "Return value"))))

; If atom is not documented then `get-doc` returns "No documentation"
; description.
!(assertEqual
   (get-doc NoSuchAtom)
   (@doc-formal (@item NoSuchAtom) (@kind atom) (@type %Undefined%) (@desc "No documentation")))

; Same result is returned if for instance documentation for the function
; application is queried.
!(assertEqual
   (get-doc (some-func arg1 arg2))
   (@doc-formal (@item (some-func arg1 arg2)) (@kind atom) (@type ReturnType) (@desc "No documentation")))

; `help!` function gets the documentation and prints it in a human readable
; format.
!(help! some-func)
; Output:
;
; Function some-func: (-> Arg1Type Arg2Type ReturnType) Test function
; Parameters:
;   Arg1Type First argument
;   Arg2Type Second argument
; Return: (@type ReturnType) Return value
;

!(help! SomeSymbol)
; Output:
;
; Atom SomeSymbol: SomeType Test symbol atom having specific type
;

!(help! some-gnd-atom)
; Output:
;
; Function some-gnd-atom: %Undefined% Test function
; Parameters:
;   %Undefined% First argument
;   %Undefined% Second argument
; Return: (@type %Undefined%) Return value
;


!(help! NoSuchAtom)
; Output:
;
; Atom NoSuchAtom: %Undefined% No documentation
;

!(help! (some-func arg1 arg2))
; Output:
;
; Atom (some-func arg1 arg2): ReturnType No documentation
;
