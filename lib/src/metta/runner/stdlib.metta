(doc if
  (desc "Replace itself by one of the arguments depending on condition.")
  (params (
    (param "Boolean condition")
    (param "Result when condition is True")
    (param "Result when condition is False")))
  (ret "Second or third argument") )
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

(doc ErrorType (desc "Type of the atom which contains error"))
(: ErrorType Type)

(doc Error
  (desc "Error constructor")
  (params (
    (param "Atom which contains error")
    (param "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (ret "Error atom"))
(: Error (-> Atom Atom ErrorType))

(doc add-reduct
  (desc "Adds atom into the atomspace reducing it first")
  (params (
    (param "Atomspace to add atom into")
    (param "Atom to add")))
  (ret "Unit atom"))
(: add-reduct (-> Space %Undefined% (->)))
(= (add-reduct $dst $atom)  (add-atom $dst $atom))

(doc add-reduct
  (desc "Prevents atom from being reduced")
  (params (
    (param "Atom")))
  (ret "Quoted atom"))
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
(: desc (-> String DocDescription))
(: param (-> String DocParameterInformal))
(: ret (-> String DocReturnInformal))

(: doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))
(: item (-> Atom DocItem))
(: (kind function) DocKindFunction)
(: (kind atom) DocKindAtom)
(: type (-> Type DocType))
(: params (-> Expression DocParameters))
(: param (-> DocType DocDescription DocParameter))
(: ret (-> DocType DocDescription DocReturn))


(: get-doc (-> Atom Atom))
(= (get-doc $atom)
  (let $meta-type (get-metatype $atom)
    (case $meta-type (
      (Expression (get-doc-atom $atom))
      ($_ (get-doc-single-atom $atom)) ))))

(: get-doc-single-atom (-> Atom Atom))
(= (get-doc-single-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (if (is-function-type $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom) ))))

(: get-doc-function (-> Atom Type Atom))
(= (get-doc-function $name $type)
  (let $top-space (mod-space! top)
  (unify $top-space (doc $name $desc (params $params) $ret)
    (let $type' (if (== $type %Undefined%) (undefined-doc-function-type $params) (cdr-atom $type))
    (let ($params' $ret') (get-doc-params $params $ret $type')
      (doc-formal (item $name) (kind function) (type $type) $desc (params $params') $ret')))
    (doc-formal (item $name) (kind function) (type $type) (desc "No documentation")) )))

(: undefined-doc-function-type (-> Expression Type))
(= (undefined-doc-function-type $params)
  (if (== () $params) (%Undefined%)
    (let $params-tail (cdr-atom $params)
    (let $tail (undefined-doc-function-type $params-tail)
      (cons-atom %Undefined% $tail) ))))

(: get-doc-params (-> Expression Atom Expression (Expression Atom)))
(= (get-doc-params $params $ret $types)
  (let $head-type (car-atom $types)
  (let $tail-types (cdr-atom $types)
    (if (== () $params)
      (let (ret $ret-desc) $ret
        (() (ret (type $head-type) (desc $ret-desc))) )
      (let (param $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (param (type $head-type) (desc $param-desc)) $params')
          ($result-params $result-ret) ))))))))

(: get-doc-atom (-> Atom Atom))
(= (get-doc-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (unify $top-space (doc $atom $desc)
      (doc-formal (item $atom) (kind atom) (type $type) $desc)
      (unify $top-space (doc $atom $desc' (params $params) $ret)
        (get-doc-function $atom %Undefined%)
        (doc-formal (item $atom) (kind atom) (type $type) (desc "No documentation")) )))))

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
    ((doc-formal (item $item) (kind function) (type $type) (desc $descr)
                 (params $params)
                 (ret (type $ret-type) (desc $ret-desc)))
      (let () (println! (format-args "Function {}: {} {}" ($item $type $descr)))
      (let () (println! (format-args "Parameters:" ()))
      (let () (for-each-in-atom $params help-param!)
      (let () (println! (format-args "Return: (type {}) {}" ($ret-type $ret-desc)))
      () )))))
    ((doc-formal (item $item) (kind function) (type $type) (desc $descr))
      (let () (println! (format-args "Function {} (type {}) {}" ($item $type $descr)))
      () ))
    ((doc-formal (item $item) (kind atom) (type $type) (desc $descr))
      (let () (println! (format-args "Atom {}: {} {}" ($item $type $descr)))
      () ))
    ($other (Error $other "Cannot match doc-formal structure") ))))

(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (param (type $type) (desc $desc)) $param
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
  (desc "Adds atom into the atomspace without reducing it")
  (params (
    (param "Atomspace to add atom into")
    (param "Atom to add")))
  (ret "Unit atom"))
