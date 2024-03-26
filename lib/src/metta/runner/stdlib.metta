(doc if
  (description "Replace itself by one of the arguments depending on condition.")
  (parameters (
    (parameter "Boolean condition")
    (parameter "Result when condition is True")
    (parameter "Result when condition is False")))
  (return "Second or third argument") )
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

(doc ErrorType (description "Type of the atom which contains error"))
(: ErrorType Type)

(doc Error
  (description "Error constructor")
  (parameters (
    (parameter "Atom which contains error")
    (parameter "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (return "Error atom"))
(: Error (-> Atom Atom ErrorType))

(doc add-reduct
  (description "Adds atom into the atomspace reducing it first")
  (parameters (
    (parameter "Atomspace to add atom into")
    (parameter "Atom to add")))
  (return "Unit atom"))
(: add-reduct (-> Space %Undefined% (->)))
(= (add-reduct $dst $atom)  (add-atom $dst $atom))

(doc add-reduct
  (description "Prevents atom from being reduced")
  (parameters (
    (parameter "Atom")))
  (return "Quoted atom"))
(: quote (-> Atom Atom))

; unify matches two atoms and returns $then if they are matched
; and $else otherwise.
(: unify (-> Atom Atom Atom Atom %Undefined%))
(= (unify $a $a $then $else) $then)
(= (unify $a $b $then $else)
  (case (unify-or-empty $a $b) ((%void%  $else))) )
(: unify-or-empty (-> Atom Atom Atom))
(= (unify-or-empty $a $a) unified)
(= (unify-or-empty $a $b) (empty))

; empty removes current result from a non-deterministic result
(: empty (-> %Undefined%))
(= (empty) (let a b never-happens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Documentation formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: doc (-> Atom DocDescription DocInformal))
(: doc (-> Atom DocDescription DocParameters DocReturnInformal DocInformal))
; TODO: First argument is String but we don't have such type yet
(: description (-> String DocDescription))
(: parameter (-> String DocParameterInformal))
(: return (-> String DocReturnInformal))

(: doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))
(: item (-> Atom DocItem))
(: (kind function) DocKindFunction)
(: (kind atom) DocKindAtom)
(: type (-> Type DocType))
(: parameters (-> Expression DocParameters))
(: parameter (-> DocType DocDescription DocParameter))
(: return (-> DocType DocDescription DocReturn))


(: get-doc (-> Atom Atom))
(= (get-doc $atom)
  (let $meta-type (get-metatype $atom)
    (case $meta-type (
      (Expression (get-doc-atom $atom))
      ($_ (get-doc-single-atom $atom)) ))))

(: get-doc-single-atom (-> Atom Atom))
(= (get-doc-single-atom $atom)
  (let $type (get-type $atom)
    (if (is-function-type $type)
      (get-doc-function $atom)
      (get-doc-atom $atom) )))

(: get-doc-function (-> Atom Atom))
(= (get-doc-function $name)
  (let $type (get-type $name)
    (let $param-types (cdr-atom $type)
      (unify &self (doc $name $desc (parameters $params) $ret)
        (let ($params' $ret') (get-doc-params $params $ret $param-types)
          (doc-formal (item $name) (kind function) (type $type) $desc (parameters $params') $ret'))
        (doc-formal (item $name) (kind function) (type $type) (description "No documentation")) ))))

(: get-doc-params (-> Expression Atom Expression (Expression Atom)))
(= (get-doc-params $params $ret $types)
  (let $head-type (car-atom $types)
  (let $tail-types (cdr-atom $types)
    (if (== () $params)
      (let (return $ret-desc) $ret
        (() (return (type $head-type) (description $ret-desc))) )
      (let (parameter $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (parameter (type $head-type) (description $param-desc)) $params')
          ($result-params $result-ret) ))))))))

(: get-doc-atom (-> Atom Atom))
(= (get-doc-atom $atom)
  (let $type (get-type $atom)
    (unify &self (doc $atom $desc)
      (doc-formal (item $atom) (kind atom) (type $type) $desc)
      (doc-formal (item $atom) (kind atom) (type $type) (description "No documentation")) )))

; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
(: is-function-type (-> Type Bool))
(= (is-function-type $type)
  (let $type-meta (get-metatype $type)
    (case $type-meta (
      (Expression
        (let $first (car-atom $type)
          (if (== $first ->) True False) ))
      ($_ False) ))))

(: help! (-> Atom (->)))
(= (help! $atom)
  (case (get-doc $atom) (
    ((doc-formal (item $item) (kind function) (type $type) (description $descr)
                 (parameters $params)
                 (return (type $ret-type) (description $ret-desc)))
      (let () (println! (format-args "Function {}: {} {}" ($item $type $descr)))
      (let () (println! (format-args "Parameters:" ()))
      (let () (for-each-in-atom $params help-param!)
      (let () (println! (format-args "Return: (type {}) {}" ($ret-type $ret-desc)))
      () )))))
    ((doc-formal (item $item) (kind function) (type $type) (description $descr))
      (let () (println! (format-args "Function {} (type {}) {}" ($item $type $descr)))
      () ))
    ((doc-formal (item $item) (kind atom) (type $type) (description $descr))
      (let () (println! (format-args "Atom {}: {} {}" ($item $type $descr)))
      () ))
    ($other (Error $other "Cannot match doc-formal structure") ))))

(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (parameter (type $type) (description $desc)) $param
    (println! (format-args "  {} {}" ($type $desc))) ))

(: for-each-in-atom (-> Expression Atom (->)))
(= (for-each-in-atom $expr $func)
  (if (noreduce-eq $expr ())
    ()
    (let $head (car-atom $expr)
      (let $tail (cdr-atom $expr)
      (let $_ ($func $head)
      (for-each-in-atom $tail $func) )))))

(: noreduce-eq (-> Atom Atom Bool))
(= (noreduce-eq $a $b) (== (quote $a) (quote $b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grounded function's documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(doc add-atom
  (description "Adds atom into the atomspace without reducing it")
  (parameters (
    (parameter "Atomspace to add atom into")
    (parameter "Atom to add")))
  (return "Unit atom"))
