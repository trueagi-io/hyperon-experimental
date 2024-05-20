; TODO: for some reason get-doc doesn't work for '='
(@doc =
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern") )
  (@return "Not reduced itself unless custom equalities over equalities are added") )
)
(: = (-> $t $t Atom))

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

(@doc ErrorType (@desc "Type of the atom which contains error"))
(: ErrorType Type)

(@doc Error
  (@desc "Error constructor")
  (@params (
    (@param "Atom which contains error")
    (@param "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (@return "Error atom"))
(: Error (-> Atom Atom ErrorType))

(@doc add-reduct
  (@desc "Adds atom into the atomspace reducing it first")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
(: add-reduct (-> hyperon::space::DynSpace %Undefined% (->)))
(= (add-reduct $dst $atom) (add-atom $dst $atom))

(@doc quote
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))
(: quote (-> Atom Atom))

(@doc unify
  (@desc "Matches two atoms and returns $then if they are matched and $else otherwise")
  (@params (
    (@param "First atom to unify with")
    (@param "Second atom to unify with")
    (@param "Result if two atoms unified successfully")
    (@param "Result otherwise")))
  (@return "Unit atom"))
(: unify (-> Atom Atom Atom Atom %Undefined%))
(= (unify $a $a $then $else) $then)
(= (unify $a $b $then $else)
  (case (unify-or-empty $a $b) ((Empty $else))) )
(: unify-or-empty (-> Atom Atom Atom))
(= (unify-or-empty $a $a) unified)
(= (unify-or-empty $a $b) (empty))

(@doc empty
  (@desc "Removes current result from a non-deterministic result")
  (@return "Empty [] brackets"))
(: empty (-> %Undefined%))
(= (empty) (let a b never-happens))

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

(@doc @return
  (@desc "Used for documentation purposes. Description of function return value starts with @return as a part of @doc")
  (@params (
    (@param "String containing return value description")))
  (@return "Return value description"))
(: @return (-> String DocReturnInformal))

(@doc @doc-formal
  (@desc "Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)")
  (@params (
    (@param "DocItem. Function/Atom name for which documentation is to be displayed. Format (@item name)")
    (@param "DocKindAtom/DocKindFunction. Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed")
    (@param "DocType. Contains type notation of function/atom")
    (@param "DocDescription. Function/atom description")
    (@param "DocParameters (functions only). Description of function parameters")
    (@param "DocReturn (functions only). Description of function's return value")))
  (@return "Expression containing full documentation on function"))
(: @doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: @doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))

(@doc @item
  (@desc "Used for documentation purposes. Converts atom/function's name to DocItem")
  (@params (
    (@param "Atom/Function name to be documented")))
  (@return "(@item Atom) entity"))
(: @item (-> Atom DocItem))

; get-doc not showing docs for following two functions (@kind function) and (@kind atom) for some reason.
(@doc (@kind function)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case")
  (@return "(@kind function) entity"))
(: (@kind function) DocKindFunction)

(@doc (@kind atom)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case")
  (@return "(@kind atom) entity"))
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

(@doc @param
  (@desc "Used for function documentation purposes. Contains description of one of function's parameters")
  (@params (
    (@param "Description of parameter"))) ; seeing at type notation of @param it seems that two input should be but i don't get what is first one then
  (@return "DocParameter containing description of function's parameter in form of (@param description)"))
(: @param (-> DocType DocDescription DocParameter))

(@doc @return
  (@desc "Used for function documentation purposes. Contains description of function's return value")
  (@params (
    (@param "Description of return value"))) ; seeing at type notation of @return it seems that two input should be but i don't get what is first one then
  (@return "DocReturn containing description of function's return value in form of (@return description)"))
(: @return (-> DocType DocDescription DocReturn))

(@doc get-doc
  (@desc "Returns documentation for the given Atom/Function. It checks if given variable is an expression and evaluates get-doc-atom on the input value or get-doc-single-atom otherwise")
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
    (if (is-function-type $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom)))))

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

; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
(@doc is-function-type
  (@desc "Function checks if input type is a function type")
  (@params (
    (@param "Type notation")))
  (@return "True if input type notation is a function type, else - otherwise"))
(: is-function-type (-> Type Bool))
(= (is-function-type $type)
  (let $type-meta (get-metatype $type)
    (case $type-meta (
      (Expression
        (let $first (car-atom $type)
          (if (== $first ->) True False) ))
      ($_ False) ))))

(@doc help!
  (@desc "Function returns documentation for input value. It uses println! to output documentation")
  (@params (
    (@param "Input to get documentation for")))
  (@return "Unit value"))
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
  (@return "Unit value"))
(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (@param (@type $type) (@desc $desc)) $param
    (println! (format-args "  {} {}" ($type $desc))) ))

(@doc for-each-in-atom
  (@desc "Applies $func to each atom in Expression")
  (@params (
    (@param "Expression to each atom in which function will be applied")
    (@param "Function to apply")))
  (@return "Unit value"))
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
  (@return "True if not reduced atoms are equal, else - otherwise"))
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