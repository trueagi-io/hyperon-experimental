(@doc =
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern")))
  (@return "Not reduced itself unless custom equalities over equalities are added") )
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
  (@desc "Matches two first arguments and returns third argument if they are matched and forth argument otherwise")
  (@params (
    (@param "First atom to unify with")
    (@param "Second atom to unify with")
    (@param "Result if two atoms unified successfully")
    (@param "Result otherwise")))
  (@return "Third argument when first two atoms are matched of forth one otherwise"))
(: unify (-> Atom Atom Atom Atom %Undefined%))
(= (unify $a $a $then $else) $then)
(= (unify $a $b $then $else)
  (case (unify-or-empty $a $b) ((Empty $else))) )
(: unify-or-empty (-> Atom Atom Atom))
(= (unify-or-empty $a $a) unified)
(= (unify-or-empty $a $b) (empty))

(@doc empty
  (@desc "Cuts evaluation of the non-deterministic branch and removes it from the result")
  (@params ())
  (@return "Nothing"))
(: empty (-> %Undefined%))
(= (empty) (let a b never-happens))

(@doc unique
  (@desc "Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]")
  (@params (
    (@param "Non-deterministic set of values")))
  (@return "Unique values from input set"))
(: unique (-> Atom Atom))
(= (unique $arg) (let $c (collapse $arg) (let $u (unique-atom $c) (superpose $u))))

(@doc union
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Union of sets"))
(: union (-> Atom Atom Atom))
(= (union $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (union-atom $c1 $c2) (superpose $u)))))

(@doc intersection
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Intersection of sets"))
(: intersection (-> Atom Atom Atom))
(= (intersection $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (intersection-atom $c1 $c2) (superpose $u)))))

(@doc subtraction
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Subtraction of sets"))
(: subtraction (-> Atom Atom Atom))
(= (subtraction $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (subtraction-atom $c1 $c2) (superpose $u)))))

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

; TODO: help! for @param and @return returns duplicating result
;(@doc @param
;  (@desc "Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc")
;  (@params (
;    (@param "String containing parameter description")))
;  (@return "Parameter description"))
(: @param (-> String DocParameterInformal))
(: @param (-> DocType DocDescription DocParameter))

;(@doc @return
;  (@desc "Used for documentation purposes. Description of function return value starts with @return as a part of @doc")
;  (@params (
;    (@param "String containing return value description")))
;  (@return "Return value description"))
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
  (@return "True if input type notation is a function type, False - otherwise"))
(: is-function-type (-> Type Bool))
(= (is-function-type $type)
  (let $type-meta (get-metatype $type)
    (case $type-meta (
      (Expression
        (let $first (car-atom $type)
          (if (== $first ->) True False) ))
      ($_ False) ))))

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

(@doc match
  (@desc "Searches for all declared atoms corresponding to the given pattern (second argument) and produces the output pattern (third argument)")
  (@params (
    (@param "A grounded atom referencing a Space")
    (@param "Pattern atom to be matched")
    (@param "Output pattern typically containing variables from the input pattern")))
  (@return "If match was successfull it outputs pattern (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Nothing - otherwise"))

(@doc bind!
  (@desc "Registers a new token which is replaced with an atom during the parsing of the rest of the program")
  (@params (
    (@param "Token name")
    (@param "Atom, which is associated with the token after reduction")))
  (@return "Unit atom"))

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

(@doc car-atom
  (@desc "Extracts the first atom of an expression as a tuple")
  (@params (
    (@param "Expression")))
  (@return "First atom of an expression"))

(@doc cdr-atom
  (@desc "Extracts the tail of an expression (all except first atom)")
  (@params (
    (@param "Expression")))
  (@return "Tail of an expression"))

(@doc cons-atom
  (@desc "Constructs an expression using two arguments")
  (@params (
    (@param "Head of an expression")
    (@param "Tail of an expression")))
  (@return "New expression consists of two input arguments"))

(@doc size-atom
  (@desc "Returns size of an expression (first argument)")
  (@params (
    (@param "Expression")))
  (@return "Size of an expression"))

(@doc index-atom
  (@desc "Returns atom from an expression (first argument) using index (second argument) or error if index is out of bounds")
  (@params (
    (@param "Expression")
    (@param "Index")))
  (@return "Atom from an expression in the place defined by index. Error if index is out of bounds of an expression"))

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

(@doc trace!
  (@desc "Prints its first argument and returns second. Both arguments will be evaluated before processing")
  (@params (
    (@param "Atom to print")
    (@param "Atom to return")))
  (@return "Evaluated second input"))

(@doc nop
  (@desc "Outputs unit atom for any input")
  (@params (
    (@param "Anything")))
  (@return "Unit atom"))

(@doc let
  (@desc "Let function is utilized to establish temporary variable bindings within an expression. It allows introducing variables (first argument), assign values to them (second argument), and then use these values within the scope of the let block")
  (@params (
    (@param "Variable name (or several variables inside brackets ())")
    (@param "Expression to be bound to variable (it is being reduced before bind)")
    (@param "Expression which will be reduced and in which variable (first argument) could be used")))
  (@return "Result of third argument's evaluation"))

(@doc let*
  (@desc "Same as let, but first argument is a tuple containing tuples of variables and their bindings, e.g. (($v (+ 1 2)) ($v2 (* 5 6)))")
  (@params (
    (@param "Tuple of tuples with variables and their bindings")
    (@param "Expression which will be reduced and in which variable (first argument) could be used")))
  (@return "Result of second argument's evaluation"))

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

(@doc get-metatype
  (@desc "Returns metatype of the input atom")
  (@params (
    (@param "Atom to get metatype for")))
  (@return "Metatype of input atom"))

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

(@doc sealed
  (@desc "Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables")
  (@params (
    (@param "Variable list e.g. ($x $y)")
    (@param "Atom which uses those variables")))
  (@return "Second argument but with variables being replaced with unique variables"))

(@doc capture
  (@desc "Wraps an atom and capture the current space")
  (@params (
    (@param "Function name which space need to be captured")))
  (@return "Function"))

(@doc case
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating of Atom bound to met condition"))

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

(@doc collapse
  (@desc "Converts a nondeterministic result into a tuple")
  (@params (
    (@param "Atom which will be evaluated")))
  (@return "Tuple"))

(@doc superpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))

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

(@doc pragma!
  (@desc "Changes global key's (first argument) value to a new one (second argument)")
  (@params (
    (@param "Key's name")
    (@param "New value")))
  (@return "Unit atom"))

; TODO: Segmentation fault (core dumped) when calling !(help &self)
;(@doc &self
;  (@desc "Returns reference to the current atomspace")
;  (@params ())
;  (@return "Reference to the current atomspace"))

; TODO: get-doc/help! not working for +
(@doc +
  (@desc "Sums two numbers")
  (@params (
    (@param "Addend")
    (@param "Augend")))
  (@return "Sum"))

; TODO: get-doc/help! not working for -
(@doc -
  (@desc "Subtracts second argument from first one")
  (@params (
    (@param "Minuend")
    (@param "Deductible")))
  (@return "Difference"))

; TODO: get-doc/help! not working for *
(@doc *
  (@desc "Multiplies two numbers")
  (@params (
    (@param "Multiplier")
    (@param "Multiplicand")))
  (@return "Product"))

; TODO: get-doc/help! not working for /
(@doc /
  (@desc "Divides first argument by second one")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Fraction"))

; TODO: get-doc/help! not working for %
(@doc %
  (@desc "Modulo operator. It returns remainder of dividing first argument by second argument")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Remainder"))

; TODO: get-doc/help! not working for <
(@doc <
  (@desc "Less than. Checks if first argument is less than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than second, False - otherwise"))

; TODO: get-doc/help! not working for >
(@doc >
  (@desc "Greater than. Checks if first argument is greater than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than second, False - otherwise"))

; TODO: get-doc/help! not working for <=
(@doc <=
  (@desc "Less than or equal. Checks if first argument is less than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than or equal to second, False - otherwise"))

; TODO: get-doc/help! not working for >=
(@doc >=
  (@desc "Greater than or equal. Checks if first argument is greater than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than or equal to second, False - otherwise"))

; TODO: get-doc/help! not working for ==
(@doc ==
  (@desc "Checks equality for two arguments of the same type")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"))

; TODO: get-doc/help! not working for and
(@doc and
  (@desc "Logical conjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if both arguments are True, False - otherwise"))

; TODO: get-doc/help! not working for or
(@doc or
  (@desc "Logical disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "True if any of input arguments is True, False - otherwise"))

; TODO: get-doc/help! not working for not
(@doc not
  (@desc "Negation")
  (@params (
    (@param "Argument")))
  (@return "Negates boolean input argument (False -> True, True -> False)"))

(@doc xor
  (@desc "Exclusive disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Return values are the same as logical disjunction, but when both arguments are True xor will return False"))

(@doc flip
  (@desc "Produces random boolean value")
  (@params ())
  (@return "Random boolean value"))

(@doc unique-atom
  (@desc "Function takes tuple and returns only unique entities. E.g. (unique-atom (a b c d d)) -> (a b c d)")
  (@params (
    (@param "List of values")))
  (@return "Unique values from input set"))

(@doc union-atom
  (@desc "Function takes two tuples and returns their union. E.g. (union-atom (a b b c) (b c c d)) -> (a b b c b c c d)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Union of sets"))

(@doc intersection-atom
  (@desc "Function takes two tuples and returns their intersection. E.g. (intersection-atom (a b c c) (b c c c d)) -> (b c c)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Intersection of sets"))

(@doc subtraction-atom
  (@desc "Function takes two tuples and returns their subtraction. E.g. !(subtraction-atom (a b b c) (b c c d)) -> (a b)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Subtraction of sets"))

(@doc git-module!
  (@desc "Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search")
  (@params (
    (@param "URL to github repo")))
  (@return "Unit atom"))
