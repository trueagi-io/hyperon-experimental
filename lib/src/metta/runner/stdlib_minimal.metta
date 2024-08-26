(@doc =
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern") ))
  (@return "Not reduced itself unless custom equalities over equalities are added") )
(: = (-> $t $t Atom))

(@doc ErrorType (@desc "Type of the atom which contains error"))
(: ErrorType Type)

(@doc Error
  (@desc "Error constructor")
  (@params (
    (@param "Atom which contains error")
    (@param "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (@return "Instance of the error atom"))
(: Error (-> Atom Atom ErrorType))

(@doc return
  (@desc "Returns value from the (function ...) expression")
  (@params (
    (@param "Value to be returned")))
  (@return "Passed argument"))
(: return (-> $t $t))

(@doc function
  (@desc "Evaluates the argument until it becomes (return <result>). Then (function (return <result>)) is reduced to the <result>.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of atom's evaluation"))
(: function (-> Atom Atom))

(@doc eval
  (@desc "Evaluates input atom, makes one step of the evaluation")
  (@params (
    (@param "Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function")))
  (@return "Result of evaluation"))
(: eval (-> Atom Atom))

(@doc chain
  (@desc "Evaluates first argument, binds it to the variable (second argument) and then evaluates third argument which contains (or not) mentioned variable")
  (@params (
    (@param "Atom to be evaluated")
    (@param "Variable")
    (@param "Atom which will be evaluated at the end")))
  (@return "Result of evaluating third input argument"))
(: chain (-> Atom Variable Atom Atom))

(@doc unify
  (@desc "Matches two first arguments and returns third argument if they are matched and forth argument otherwise")
  (@params (
    (@param "First atom to unify with")
    (@param "Second atom to unify with")
    (@param "Result if two atoms unified successfully")
    (@param "Result otherwise")))
  (@return "Third argument when first two atoms are matched of forth one otherwise"))
(: unify (-> Atom Atom Atom Atom Atom))

(@doc cons-atom
  (@desc "Constructs an expression using two arguments")
  (@params (
    (@param "Head of an expression")
    (@param "Tail of an expression")))
  (@return "New expression consists of two input arguments"))
(: cons-atom (-> Atom Expression Expression))

(@doc decons-atom
  (@desc "Works as a reverse to cons-atom function. It gets Expression as an input and returns it splitted to head and tail, e.g. (decons-atom (Cons X Nil)) -> (Cons (X Nil))")
  (@params (
    (@param "Expression")))
  (@return "Deconsed expression"))
(: decons-atom (-> Expression Expression))

(@doc collapse-bind
  (@desc "Evaluates the Atom (first argument) and returns an expression which contains all alternative evaluations in a form (Atom Bindings). Bindings are represented in a form of a grounded atom.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "All alternative evaluations"))
(: collapse-bind (-> Atom Expression))

(@doc superpose-bind
  (@desc "Complement to the collapse-bind. It takes result of collapse-bind (first argument) and returns only result atoms without bindings")
  (@params (
    (@param "Expression in form (Atom Binding)")))
  (@return "Non-deterministic list of Atoms"))
(: superpose-bind (-> Expression Atom))

(@doc metta
  (@desc "Run MeTTa interpreter on atom.")
  (@params (
    (@param "Atom to be interpreted")
    (@param "Type of input atom")
    (@param "Atomspace where intepretation should take place")))
  (@return "Result of interpretation"))
(: metta (-> Atom Type Grounded Atom))

(@doc id
  (@desc "Returns its argument")
  (@params (
    (@param "Input argument")))
  (@return "Input argument"))
(: id (-> Atom Atom))
(= (id $x) $x)

(@doc atom-subst
  (@desc "Substitutes variable passed as a second argument in the third argument by the first argument")
  (@params (
    (@param "Value to use for replacement")
    (@param "Variable to replace")
    (@param "Template to replace variable by the value")))
  (@return "Template with substituted variable"))
(: atom-subst (-> Atom Variable Atom Atom))
(= (atom-subst $atom $var $templ)
  (function (chain (eval (id $atom)) $var (return $templ))) )

(@doc if-decons-expr
  (@desc "Checks if first argument is non empty expression. If so gets tail and head from the first argument and returns forth argument using head and tail values. Returns fifth argument otherwise.")
  (@params (
    (@param "Expression to be deconstructed")
    (@param "Head variable")
    (@param "Tail variable")
    (@param "Template to return if first argument is a non-empty expression")
    (@param "Default value to return otherwise")))
  (@return "Either template with head and tail replaced by values or default value"))
(: if-decons-expr (-> Expression Variable Variable Atom Atom Atom))
(= (if-decons-expr $atom $head $tail $then $else)
  (function (eval (if-equal $atom ()
    (return $else)
    (chain (decons-atom $atom) $list
      (unify $list ($head $tail) (return $then) (return $else)) )))))

(@doc if-error
  (@desc "Checks if first argument is an error atom. Returns second argument if so or third argument otherwise.")
  (@params (
    (@param "Atom to be checked for the error")
    (@param "Value to return if first argument is an error")
    (@param "Value to return otherwise")))
  (@return "Second or third argument"))
(: if-error (-> Atom Atom Atom Atom))
(= (if-error $atom $then $else)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $meta Expression
      (eval (if-equal $atom ()
        (return $else)
        (chain (decons-atom $atom) $list
          (unify $list ($head $tail)
            (eval (if-equal $head Error (return $then) (return $else)))
            (return $else) ))))
      (return $else) )))))

(@doc return-on-error
  (@desc "Returns first argument if it is Empty or an error. Returns second argument otherwise.")
  (@params (
    (@param "Previous evaluation result")
    (@param "Atom for further evaluation")))
  (@return "Return previous result if it is an error or Empty or continue evaluation"))
(: return-on-error (-> Atom Atom Atom))
(= (return-on-error $atom $then)
  (function (eval (if-equal $atom Empty (return (return Empty))
    (eval (if-error $atom (return (return $atom))
      (return $then) ))))))

; Difference between `switch` and `case` is a way how they interpret `Empty`
; result. `CaseOp` interprets first argument inside itself and then manually
; checks whether result is empty. `switch` is interpreted in a context of
; main interpreter. Minimal interpreter correctly passes `Empty` as an
; argument to the `switch` but when `switch` is called from MeTTa interpreter
; (for example user evaluates `!(switch (unify A B ok Empty) ...)` then
; emptiness of the first argument is checked by interpreter and it will
; break execution when `Empty` is returned.
(@doc switch
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom to be matched with patterns")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result which corresponds to the pattern which is matched with the passed atom first"))
(: switch (-> %Undefined% Expression Atom))
(= (switch $atom $cases)
  (function (chain (decons-atom $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (chain (eval (if-equal $res NotReducible Empty $res)) $x (return $x)) ))))

(@doc switch-internal
  (@desc "This function is being called inside switch function to test one of the cases and it calls switch once again if current condition is not met")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Deconsed tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating of Atom bound to met condition"))
(= (switch-internal $atom (($pattern $template) $tail))
  (function (unify $atom $pattern
    (return $template)
    (chain (eval (switch $atom $tail)) $ret (return $ret)) )))

; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
(@doc is-function
  (@desc "Function checks if input type is a function type")
  (@params (
    (@param "Type atom")))
  (@return "True if type is a function type, False - otherwise"))
(: is-function (-> Type Bool))
(= (is-function $type)
  (function (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta) (
      (($type Expression)
        (eval (if-decons-expr $type $head $_tail
          (unify $head -> (return True) (return False))
          (return (Error (is-function $type) "is-function non-empty expression as an argument")) )))
      (($type $meta) (return False))
    ))))))

(@doc type-cast
  (@desc "Casts atom passed as a first argument to the type passed as a second argument using space as a context")
  (@params (
    (@param "Atom to be casted")
    (@param "Type to cast atom to")
    (@param "Context atomspace")))
  (@return "Atom if casting is successful, (Error ... BadType) otherwise"))
(= (type-cast $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type $meta
      (return $atom)
      (chain (eval (collapse-bind (eval (get-type $atom $space)))) $collapsed
        (chain (eval (map-atom $collapsed $pair (eval (first-from-pair $pair)))) $actual-types
            (chain (eval (foldl-atom $actual-types False $a $b (eval (match-type-or $a $b $type)))) $is-some-comp
              (eval (if $is-some-comp
                (return $atom)
                (return (Error $atom BadType)) ))))))))))

(@doc match-types
  (@desc "Checks if two types can be unified and returns third argument if so, fourth - otherwise")
  (@params (
    (@param "First type")
    (@param "Second type")
    (@param "Atom to be returned if types can be unified")
    (@param "Atom to be returned if types cannot be unified")))
  (@return "Third or fourth argument"))
(= (match-types $type1 $type2 $then $else)
  (function (eval (if-equal $type1 %Undefined%
    (return $then)
    (eval (if-equal $type2 %Undefined%
      (return $then)
      (eval (if-equal $type1 Atom
        (return $then)
        (eval (if-equal $type2 Atom
          (return $then)
          (unify $type1 $type2 (return $then) (return $else)) ))))))))))

(@doc first-from-pair
  (@desc "Gets a pair as a first argument and returns first atom from pair")
  (@params (
    (@param "Pair")))
  (@return "First atom from a pair"))
(= (first-from-pair $pair)
  (function
    (unify $pair ($first $second)
      (return $first)
      (return (Error (first-from-pair $pair) "incorrect pair format")))))

(@doc match-type-or
  (@desc "Checks if two types (second and third arguments) can be unified and returns result of OR operation between first argument and type checking result")
  (@params (
    (@param "Boolean value")
    (@param "First type")
    (@param "Second type")))
  (@return "True or False"))
(= (match-type-or $folded $next $type)
  (function
    (chain (eval (match-types $next $type True False)) $matched
      (chain (eval (or $folded $matched)) $or (return $or)) )))

(@doc filter-atom
  (@desc "Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter. E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)")
  (@params (
    (@param "List of atoms")
    (@param "Variable")
    (@param "Filter predicate")))
  (@return "Filtered list"))
(: filter-atom (-> Expression Variable Atom Expression))
(= (filter-atom $list $var $filter)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (filter-atom $tail $var $filter)) $tail-filtered
      (chain (eval (atom-subst $head $var $filter)) $filter-expr
        (chain $filter-expr $is-filtered
          (eval (if $is-filtered
            (chain (cons-atom $head $tail-filtered) $res (return $res))
            (return $tail-filtered) )))))
    (return ()) ))))

(@doc map-atom
  (@desc "Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)")
  (@params (
    (@param "List of atoms")
    (@param "Variable name")
    (@param "Template using variable")))
  (@return "Result of evaluating template for each atom in a list"))
(: map-atom (-> Expression Variable Atom Expression))
(= (map-atom $list $var $map)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (map-atom $tail $var $map)) $tail-mapped
      (chain (eval (atom-subst $head $var $map)) $map-expr
        (chain $map-expr $head-mapped
          (chain (cons-atom $head-mapped $tail-mapped) $res (return $res)) )))
    (return ()) ))))

(@doc foldl-atom
  (@desc "Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside")
  (@params (
    (@param "List of values")
    (@param "Init value")
    (@param "Variable")
    (@param "Variable")
    (@param "Operation")))
  (@return "Result of applying operation to the list of values"))
(: foldl-atom (-> Expression Atom Variable Variable Atom Atom))
(= (foldl-atom $list $init $a $b $op)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (atom-subst $init $a $op)) $op-init
      (chain (eval (atom-subst $head $b $op-init)) $op-head
        (chain $op-head $head-folded
          (chain (eval (foldl-atom $tail $head-folded $a $b $op)) $res (return $res)) )))
    (return $init) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Standard library written in MeTTa ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@doc if
  (@desc "Replace itself by one of the arguments depending on condition.")
  (@params (
    (@param "Boolean condition")
    (@param "Result when condition is True")
    (@param "Result when condition is False")))
  (@return "Second or third argument") )
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

; TODO: help! not working for operations which are defined in both Python and
; Rust standard library: or, and, not
(@doc or
  (@desc "Logical disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "True if any of input arguments is True, False - otherwise"))
(: or (-> Bool Bool Bool))
(= (or False False) False)
(= (or False True) True)
(= (or True False) True)
(= (or True True) True)

(@doc and
  (@desc "Logical conjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if both arguments are True, False - otherwise"))
(: and (-> Bool Bool Bool))
(= (and False False) False)
(= (and False True) False)
(= (and True False) False)
(= (and True True) True)

(@doc not
  (@desc "Logical negation")
  (@params (
    (@param "Argument")))
  (@return "Negates boolean input argument (False -> True, True -> False)"))
(: not (-> Bool Bool))
(= (not True) False)
(= (not False) True)

(@doc let
  (@desc "Unify two first argument and apply result of the unification on third argument. Second argument is evaluated before unification.")
  (@params (
    (@param "First atom to be unified")
    (@param "Second atom to be unified")
    (@param "Expression which will be evaluated if two first arguments can be unified")))
  (@return "Third argument or Empty"))
(: let (-> Atom %Undefined% Atom %Undefined%))
(= (let $pattern $atom $template)
  (unify $atom $pattern $template Empty))

(@doc let*
  (@desc "Same as let but inputs list of pairs of atoms to be unified. For example (let* (($v1 (+ 1 2)) ($v2 (* 5 6))) (+ $v1 $v2))")
  (@params (
    (@param "List of pairs, atoms in each pair to be unified")
    (@param "Expression which will be evaluated if each pair can be unified")))
  (@return "Second argument or Empty"))
(: let* (-> Expression Atom %Undefined%))
(= (let* $pairs $template)
  (eval (if-decons-expr $pairs ($pattern $atom) $tail
    (let $pattern $atom (let* $tail $template))
    $template )))

(@doc add-reduct
  (@desc "Reduces atom (second argument) and adds it into the atomspace (first argument)")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
(: add-reduct (-> Grounded %Undefined% (->)))
(= (add-reduct $dst $atom) (add-atom $dst $atom))

(@doc car-atom
  (@desc "Extracts the first atom of an expression as a tuple")
  (@params (
    (@param "Expression")))
  (@return "First atom of an expression"))
(: car-atom (-> Expression Atom))
(= (car-atom $atom)
  (eval (if-decons-expr $atom $head $_
    $head
    (Error (car-atom $atom) "car-atom expects a non-empty expression as an argument") )))

(@doc cdr-atom
  (@desc "Extracts the tail of an expression (all except first atom)")
  (@params (
    (@param "Expression")))
  (@return "Tail of an expression"))
(: cdr-atom (-> Expression Expression))
(= (cdr-atom $atom)
  (eval (if-decons-expr $atom $_ $tail
    $tail
    (Error (cdr-atom $atom) "cdr-atom expects a non-empty expression as an argument") )))

(@doc quote
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))
(: quote (-> Atom Atom))
(= (quote $atom) NotReducible)

(@doc unquote
  (@desc "Unquotes quoted atom, e.g. (unquote (quote $x)) returns $x")
  (@params (
    (@param "Quoted atom")))
  (@return "Unquoted atom"))
(: unquote (-> %Undefined% %Undefined%))
(= (unquote (quote $atom)) $atom)

; TODO: there is no way to define operation which consumes any number of
; arguments  and returns unit
(@doc nop
  (@desc "Outputs unit atom")
  (@params ())
  (@return "Unit atom"))
(= (nop) ())
(= (nop $x) ())

; TODO: MINIMAL added for compatibility, remove after migration
(@doc empty
  (@desc "Cuts evaluation of the non-deterministic branch and removes it from the result")
  (@params ())
  (@return "Nothing"))
(= (empty) Empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Documentation formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@doc @doc
  (@desc "Used for documentation purposes. Function documentation starts with @doc")
  (@params (
    (@param "Function name")
    (@param "Function description. Starts with @desc")
    (@param "(Optional) parameters description starting with @params which should contain one or more @param symbols")
    (@param "(Optional) description of what function will return. Starts with @return")))
  (@return "Function documentation using @doc-formal"))
(: @doc (-> Atom DocDescription DocInformal))
(: @doc (-> Atom DocDescription DocParameters DocReturnInformal DocInformal))

(@doc @desc
  (@desc "Used for documentation purposes. Description of function starts with @desc as a part of @doc")
  (@params (
    (@param "String containing function description")))
  (@return "Function description"))
(: @desc (-> String DocDescription))

(@doc @param
  (@desc "Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc")
  (@params (
    (@param "String containing parameter description")))
  (@return "Parameter description"))
(: @param (-> String DocParameterInformal))
(: @param (-> DocType DocDescription DocParameter))

(@doc @return
  (@desc "Used for documentation purposes. Description of function return value starts with @return as a part of @doc")
  (@params (
    (@param "String containing return value description")))
  (@return "Return value description"))
(: @return (-> String DocReturnInformal))
(: @return (-> DocType DocDescription DocReturn))

(@doc @doc-formal
  (@desc "Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)")
  (@params (
    (@param "Function/Atom name for which documentation is to be displayed. Format (@item name)")
    (@param "Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed")
    (@param "Contains type notation of function/atom")
    (@param "Function/atom description")
    (@param "(Functions only). Description of function parameters")
    (@param "(Functions only). Description of function's return value")))
  (@return "Expression containing full documentation on function"))
(: @doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: @doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))

(@doc @item
  (@desc "Used for documentation purposes. Converts atom/function's name to DocItem")
  (@params (
    (@param "Atom/Function name to be documented")))
  (@return "(@item Atom) entity"))
(: @item (-> Atom DocItem))

; TODO: help! gives two outputs
;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
(@doc (@kind function)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case"))
(: (@kind function) DocKindFunction)

(@doc (@kind atom)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case"))
(: (@kind atom) DocKindAtom)

(@doc @type
  (@desc "Used for documentation purposes. Converts atom/function's type to DocType")
  (@params (
    (@param "Atom/Function type to be documented")))
  (@return "(@type Type) entity"))
(: @type (-> Type DocType))

(@doc @params
  (@desc "Used for function documentation purposes. Contains several @param entities with description of each @param")
  (@params (
    (@param "Several (@param ...) entities")))
  (@return "DocParameters containing description of all parameters of function in form of (@params ((@param ...) (@param ...) ...))"))
(: @params (-> Expression DocParameters))

(@doc get-doc
  (@desc "Returns documentation for the given Atom/Function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc (-> Atom Atom))
(= (get-doc $atom)
  (let $meta-type (get-metatype $atom)
    (case $meta-type (
      (Expression (get-doc-atom $atom))
      ($_ (get-doc-single-atom $atom)) ))))

(@doc get-doc-single-atom
  (@desc "Function used by get-doc to get documentation on either function or atom. It checks if input name is the name of function or atom and calls correspondent function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc-single-atom (-> Atom Atom))
(= (get-doc-single-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (if (is-function $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom) ))))

(@doc get-doc-function
  (@desc "Function used by get-doc-single-atom to get documentation on a function. It returns documentation on a function if it exists or default documentation with no description otherwise")
  (@params (
    (@param "Function name for which documentation is needed")
    (@param "Type notation for this function")))
  (@return "Documentation for the given function"))
(: get-doc-function (-> Atom Type Atom))
(= (get-doc-function $name $type)
  (let $top-space (mod-space! top)
  (unify $top-space (@doc $name $desc (@params $params) $ret)
    (let $type' (if (== $type %Undefined%) (undefined-doc-function-type $params) (cdr-atom $type))
    (let ($params' $ret') (get-doc-params $params $ret $type')
      (@doc-formal (@item $name) (@kind function) (@type $type) $desc (@params $params') $ret')))
    (@doc-formal (@item $name) (@kind function) (@type $type) (@desc "No documentation")) )))

(@doc undefined-doc-function-type
  (@desc "Function used by get-doc-single-atom in case of absence of function's type notation")
  (@params (
    (@param "List of parameters for the function we want to get documentation for")))
  (@return "List of %Undefined% number of which depends on input list size. So for two parameters function will return (%Undefined% %Undefined% %Undefined%)"))
(: undefined-doc-function-type (-> Expression Type))
(= (undefined-doc-function-type $params)
  (if (== () $params) (%Undefined%)
    (let $params-tail (cdr-atom $params)
    (let $tail (undefined-doc-function-type $params-tail)
      (cons-atom %Undefined% $tail) ))))

(@doc get-doc-params
  (@desc "Function used by get-doc-function to get function's parameters documentation (including return value)")
  (@params (
    (@param "List of parameters in form of ((@param Description) (@param Description)...)")
    (@param "Return value's description in form of (@return Description)")
    (@param "Type notation without -> starting symbol e.g. (Atom Atom Atom)")))
  (@return "United list of params and return value each augmented with its type. E.g. (((@param (@type Atom) (@desc Description)) (@param (@type Atom) (@desc Description2))) (@return (@type Atom) (@desc Description)))"))
(: get-doc-params (-> Expression Atom Expression (Expression Atom)))
(= (get-doc-params $params $ret $types)
  (let $head-type (car-atom $types)
  (let $tail-types (cdr-atom $types)
    (if (== () $params)
      (let (@return $ret-desc) $ret
        (() (@return (@type $head-type) (@desc $ret-desc))) )
      (let (@param $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (@param (@type $head-type) (@desc $param-desc)) $params')
          ($result-params $result-ret) ))))))))

(@doc get-doc-atom
  (@desc "Function used by get-doc (in case of input type Expression) and get-doc-single-atom (in case input value is not a function) to get documentation on input value")
  (@params (
    (@param "Atom's name to get documentation for")))
  (@return "Documentation on input Atom"))
(: get-doc-atom (-> Atom Atom))
(= (get-doc-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (unify $top-space (@doc $atom $desc)
      (@doc-formal (@item $atom) (@kind atom) (@type $type) $desc)
      (unify $top-space (@doc $atom $desc' (@params $params) $ret)
        (get-doc-function $atom %Undefined%)
        (@doc-formal (@item $atom) (@kind atom) (@type $type) (@desc "No documentation")) )))))

(@doc help!
  (@desc "Function prints documentation for the input atom.")
  (@params (
    (@param "Input to get documentation for")))
  (@return "Unit atom"))
(: help! (-> Atom (->)))
(= (help! $atom)
  (case (get-doc $atom) (
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr)
                 (@params $params)
                 (@return (@type $ret-type) (@desc $ret-desc)))
      (let () (println! (format-args "Function {}: {} {}" ($item $type $descr)))
      (let () (println! (format-args "Parameters:" ()))
      (let () (for-each-in-atom $params help-param!)
      (let () (println! (format-args "Return: (type {}) {}" ($ret-type $ret-desc)))
      () )))))
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr))
      (let () (println! (format-args "Function {} (type {}) {}" ($item $type $descr)))
      () ))
    ((@doc-formal (@item $item) (@kind atom) (@type $type) (@desc $descr))
      (let () (println! (format-args "Atom {}: {} {}" ($item $type $descr)))
      () ))
    ($other (Error $other "Cannot match @doc-formal structure") ))))

(@doc help-param!
  (@desc "Function used by function help! to output parameters using println!")
  (@params (
    (@param "Parameters list")))
  (@return "Unit atom"))
(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (@param (@type $type) (@desc $desc)) $param
    (println! (format-args "  {} {}" ((type $type) $desc))) ))

(@doc for-each-in-atom
  (@desc "Applies function passed as a second argument to each atom inside first argument")
  (@params (
    (@param "Expression to each atom in which function will be applied")
    (@param "Function to apply")))
  (@return "Unit atom"))
(: for-each-in-atom (-> Expression Atom (->)))
(= (for-each-in-atom $expr $func)
  (if (noreduce-eq $expr ())
    ()
    (let $head (car-atom $expr)
      (let $tail (cdr-atom $expr)
      (let $_ ($func $head)
      (for-each-in-atom $tail $func) )))))

(@doc noreduce-eq
  (@desc "Checks equality of two atoms without reducing them")
  (@params (
    (@param "First atom")
    (@param "Second atom")))
  (@return "True if not reduced atoms are equal, False - otherwise"))
(: noreduce-eq (-> Atom Atom Bool))
(= (noreduce-eq $a $b) (== (quote $a) (quote $b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grounded function's documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@doc add-atom
  (@desc "Adds atom into the atomspace without reducing it")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))

(@doc get-type
  (@desc "Returns type notation of input atom")
  (@params (
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom"))

(@doc get-type-space
  (@desc "Returns type notation of input Atom (second argument) relative to a specified atomspace (first argument)")
  (@params (
    (@param "Atomspace where type notation for input atom will be searched")
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom in provided atomspace"))

(@doc get-metatype
  (@desc "Returns metatype of the input atom")
  (@params (
    (@param "Atom to get metatype for")))
  (@return "Metatype of input atom"))

(@doc if-equal
  (@desc "Checks if first two arguments are equal and evaluates third argument if equal, fourth argument - otherwise")
  (@params (
    (@param "First argument")
    (@param "Second argument")
    (@param "Atom to be evaluated if arguments are equal")
    (@param "Atom to be evaluated if arguments are not equal")))
  (@return "Evaluated third or fourth argument"))

(@doc new-space
  (@desc "Creates new Atomspace which could be used further in the program as a separate from &self Atomspace")
  (@params ())
  (@return "Reference to a new space"))

(@doc remove-atom
  (@desc "Removes atom from the input Atomspace")
  (@params (
    (@param "Reference to the space from which the Atom needs to be removed")
    (@param "Atom to be removed")))
  (@return "Unit atom"))

(@doc get-atoms
  (@desc "Shows all atoms in the input Atomspace")
  (@params (
    (@param "Reference to the space")))
  (@return "List of all atoms in the input space"))

(@doc new-state
  (@desc "Creates a new state atom wrapping its argument")
  (@params (
    (@param "Atom to be wrapped")))
  (@return "Returns (State $value) where $value is an argument to a new-state"))

(@doc change-state!
  (@desc "Changes input state's wrapped atom to another value (second argument). E.g. (change-state! (State 5) 6) -> (State 6)")
  (@params (
    (@param "State created by new-state function")
    (@param "Atom which will replace wrapped atom in the input state")))
  (@return "State with replaced wrapped atom"))

(@doc get-state
  (@desc "Gets a state as an argument and returns its wrapped atom. E.g. (get-state (State 5)) -> 5")
  (@params (
    (@param "State")))
  (@return "Atom wrapped by state"))

(@doc match
  (@desc "Searches for all declared atoms corresponding to the given pattern (second argument) inside space (first argument) and returns the output template (third argument)")
  (@params (
    (@param "Atomspace to search pattern")
    (@param "Pattern atom to be searched")
    (@param "Output template typically containing variables from the input pattern")))
  (@return "If match was successfull it outputs template (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Empty - otherwise"))

(@doc register-module!
  (@desc "Takes a file system path (first argument) and loads the module into the runner")
  (@params (
    (@param "File system path")))
  (@return "Unit atom"))

(@doc mod-space!
  (@desc "Returns the space of the module (first argument) and tries to load the module if it is not loaded into the module system")
  (@params (
    (@param "Module name")))
  (@return "Space name"))

(@doc print-mods!
  (@desc "Prints all modules with their correspondent spaces")
  (@params ())
  (@return "Unit atom"))

(@doc assertEqual
  (@desc "Compares (sets of) results of evaluation of two expressions")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if both expression after evaluation is equal, error - otherwise"))

(@doc assertEqualToResult
  (@desc "Same as assertEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation")
  (@params (
    (@param "First expression (it will be evaluated)")
    (@param "Second expression (it won't be evaluated)")))
  (@return "Unit atom if both expression after evaluation is equal, error - otherwise"))

(@doc superpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))

(@doc collapse
  (@desc "Converts a nondeterministic result into a tuple")
  (@params (
    (@param "Atom which will be evaluated")))
  (@return "Tuple"))

(@doc case
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating of Atom bound to met condition"))

(@doc capture
  (@desc "Wraps an atom and capture the current space")
  (@params (
    (@param "Function name which space need to be captured")))
  (@return "Function"))

(@doc pragma!
  (@desc "Changes global key's (first argument) value to a new one (second argument)")
  (@params (
    (@param "Key's name")
    (@param "New value")))
  (@return "Unit atom"))

(@doc import!
  (@desc "Imports module using its relative path (second argument) and binds it to the token (first argument) which will represent imported atomspace. If first argument is &self then everything will be imported to current atomspace")
  (@params (
    (@param "Symbol, which is turned into the token for accessing the imported module")
    (@param "Module name")))
  (@return "Unit atom"))

(@doc include
  (@desc "Works just like import! but with &self as a first argument. So everything from input file will be included in the current atomspace and evaluated")
  (@params (
    (@param "Name of metta script to import")))
  (@return "Unit atom"))

(@doc bind!
  (@desc "Registers a new token which is replaced with an atom during the parsing of the rest of the program")
  (@params (
    (@param "Token name")
    (@param "Atom, which is associated with the token after reduction")))
  (@return "Unit atom"))

(@doc trace!
  (@desc "Prints its first argument and returns second. Both arguments will be evaluated before processing")
  (@params (
    (@param "Atom to print")
    (@param "Atom to return")))
  (@return "Evaluated second input"))

(@doc println!
  (@desc "Prints a line of text to the console")
  (@params (
    (@param "Expression/atom to be printed out")))
  (@return "Unit atom"))

(@doc format-args
  (@desc "Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables")
  (@params (
    (@param "Expression with {} symbols to be replaced")
    (@param "Atoms to be placed inside expression instead of {}")))
  (@return "Expression with replaced {} with atoms"))

(@doc sealed
  (@desc "Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables")
  (@params (
    (@param "Variable list e.g. ($x $y)")
    (@param "Atom which uses those variables")))
  (@return "Second argument but with variables being replaced with unique variables"))

; TODO: help! not working for &self (segmentation fault)
;(@doc &self
;  (@desc "Returns reference to the current atomspace")
;  (@params ())
;  (@return "Reference to the current atomspace"))

; TODO: help! not working for operations which are defined in both Python and
; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==
(@doc +
  (@desc "Sums two numbers")
  (@params (
    (@param "Addend")
    (@param "Augend")))
  (@return "Sum"))

(@doc -
  (@desc "Subtracts second argument from first one")
  (@params (
    (@param "Minuend")
    (@param "Deductible")))
  (@return "Difference"))

(@doc *
  (@desc "Multiplies two numbers")
  (@params (
    (@param "Multiplier")
    (@param "Multiplicand")))
  (@return "Product"))

(@doc /
  (@desc "Divides first argument by second one")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Fraction"))

(@doc %
  (@desc "Modulo operator. It returns remainder of dividing first argument by second argument")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Remainder"))

(@doc <
  (@desc "Less than. Checks if first argument is less than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than second, False - otherwise"))

(@doc >
  (@desc "Greater than. Checks if first argument is greater than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than second, False - otherwise"))

(@doc <=
  (@desc "Less than or equal. Checks if first argument is less than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than or equal to second, False - otherwise"))

(@doc >=
  (@desc "Greater than or equal. Checks if first argument is greater than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than or equal to second, False - otherwise"))

(@doc ==
  (@desc "Checks equality for two arguments of the same type")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"))

(@doc xor
  (@desc "Logical exclusive or")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if one and only one of the inputs is True"))

(@doc flip
  (@desc "Random boolean value")
  (@params ())
  (@return "Returns uniformly distributed random boolean value"))

(@doc unique
  (@desc "Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]")
  (@params (
    (@param "Non-deterministic set of values")))
  (@return "Unique values from input set"))

(@doc union
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Union of sets"))

(@doc intersection
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Intersection of sets"))

(@doc subtraction
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Subtraction of sets"))

(@doc git-module!
  (@desc "Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search")
  (@params (
    (@param "URL to github repo")))
  (@return "Unit atom"))
