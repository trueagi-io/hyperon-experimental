(@doc = 
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern") )
  (@return "Not reduced itself unless custom equalities over equalities are added") )
)
(: = (-> $t $t Atom))

(@doc ErrorType (@desc "Type of the atom which contains error"))
(: ErrorType Type)
(: Error (-> Atom Atom ErrorType))
(: return (-> $t $t))

(: function (-> Atom Atom))
(: eval (-> Atom Atom))
(: chain (-> Atom Variable Atom Atom))
(: unify (-> Atom Atom Atom Atom Atom))
(: cons-atom (-> Atom Expression Expression))
(: decons-atom (-> Expression Expression))
(: collapse-bind (-> Atom Expression))
(: superpose-bind (-> Expression Atom))

(: id (-> Atom Atom))
(= (id $x) $x)

(: apply (-> Atom Variable Atom Atom))
(= (apply $atom $var $templ)
  (function (chain (eval (id $atom)) $var (return $templ))) )

(: if-decons-expr (-> Expression Variable Variable Atom Atom Atom))
(= (if-decons-expr $atom $head $tail $then $else)
  (function (eval (if-equal $atom ()
    (return $else)
    (chain (decons-atom $atom) $list
      (unify $list ($head $tail) (return $then) (return $else)) )))))

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

(: return-on-error (-> Atom Atom Atom))
(= (return-on-error $atom $then)
  (function (eval (if-equal $atom Empty (return (return Empty))
    (eval (if-error $atom (return (return $atom))
      (return $then) ))))))

(: switch (-> %Undefined% Expression Atom))
(= (switch $atom $cases)
  (function (chain (decons-atom $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (chain (eval (if-equal $res NotReducible Empty $res)) $x (return $x)) ))))

(= (switch-internal $atom (($pattern $template) $tail))
  (function (unify $atom $pattern
    (return $template)
    (chain (eval (switch $atom $tail)) $ret (return $ret)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa interpreter implementation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(= (first-from-pair $pair)
  (function
    (unify $pair ($first $second)
      (return $first)
      (return (Error (first-from-pair $pair) "incorrect pair format")))))

(= (match-type-or $folded $next $type)
  (function
    (chain (eval (match-types $next $type True False)) $matched
      (chain (eval (or $folded $matched)) $or (return $or)) )))

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

; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
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

(: filter-atom (-> Expression Variable Atom Expression))
(= (filter-atom $list $var $filter)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (filter-atom $tail $var $filter)) $tail-filtered
      (chain (eval (apply $head $var $filter)) $filter-expr
        (chain $filter-expr $is-filtered
          (eval (if $is-filtered
            (chain (cons-atom $head $tail-filtered) $res (return $res))
            (return $tail-filtered) )))))
    (return ()) ))))

(: map-atom (-> Expression Variable Atom Expression))
(= (map-atom $list $var $map)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (map-atom $tail $var $map)) $tail-mapped
      (chain (eval (apply $head $var $map)) $map-expr
        (chain $map-expr $head-mapped
          (chain (cons-atom $head-mapped $tail-mapped) $res (return $res)) )))
    (return ()) ))))

(: foldl-atom (-> Expression Atom Variable Variable Atom Atom))
(= (foldl-atom $list $init $a $b $op)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (apply $init $a $op)) $op-init
      (chain (eval (apply $head $b $op-init)) $op-head
        (chain $op-head $head-folded
          (chain (eval (foldl-atom $tail $head-folded $a $b $op)) $res (return $res)) )))
    (return $init) ))))

(: separate-errors (-> Expression Expression Expression))
(= (separate-errors $succ-err $res)
  (function (unify $succ-err ($suc $err)
    (unify $res ($a $_b)
      (eval (if-error $a
        (chain (cons-atom $res $err) $err' (return ($suc $err')))
        (chain (cons-atom $res $suc) $suc' (return ($suc' $err))) ))
      (return $succ-err))
    (return $succ-err) )))

(= (check-alternatives $atom)
  (function
    (chain (collapse-bind $atom) $collapsed
      ;(chain (eval (print-alternatives! $atom $collapsed)) $_
      (chain (eval (foldl-atom $collapsed (() ()) $succ-err $res
        (eval (separate-errors $succ-err $res)))) $separated
        (unify $separated ($success $error)
          (chain (eval (if-equal $success () $error $success)) $filtered
            (chain (superpose-bind $filtered) $ret (return $ret)) )
          (return (Error (check-alternatives $atom) "list of results was not filtered correctly")) )))))

(= (interpret $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type Atom
      (return $atom)
      (eval (if-equal $type $meta
        (return $atom)
        (eval (switch ($type $meta) (
          (($type Variable) (return $atom))
          (($type Symbol)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($type Grounded)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($type Expression)
            (chain (eval (check-alternatives (eval (interpret-expression $atom $type $space)))) $ret (return $ret)))
        ))))))))))

(= (interpret-expression $atom $type $space)
  (function (eval (if-decons-expr $atom $op $args
    (chain (eval (get-type $op $space)) $op-type
      (chain (eval (is-function $op-type)) $is-func
        (unify $is-func True
          (chain (eval (interpret-func $atom $op-type $type $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) )
          (chain (eval (interpret-tuple $atom $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) ))))
    (chain (eval (type-cast $atom $type $space)) $ret (return $ret)) ))))

(= (interpret-func $expr $type $ret-type $space)
  (function (eval (if-decons-expr $expr $op $args
    (chain (eval (interpret $op $type $space)) $reduced-op
      (eval (return-on-error $reduced-op
        (eval (if-decons-expr $type $arrow $arg-types
          (chain (eval (interpret-args $expr $args $arg-types $ret-type $space)) $reduced-args
            (eval (return-on-error $reduced-args
              (chain (cons-atom $reduced-op $reduced-args) $r (return $r)))))
          (return (Error $type "Function type expected")) )))))
    (return (Error $expr "Non-empty expression atom is expected")) ))))

(= (interpret-args $atom $args $arg-types $ret-type $space)
  (function (unify $args ()
    (eval (if-decons-expr $arg-types $actual-ret-type $type-tail
      (chain (eval (== () $type-tail)) $correct-type-len
        (eval (if $correct-type-len
          (eval (match-types $actual-ret-type $ret-type
            (return ())
            (return (Error $atom BadType)) ))
          (return (Error $atom IncorrectNumberOfArguments)) )))
      (return (Error $atom IncorrectNumberOfArguments)) ))
    (eval (if-decons-expr $args $head $tail
      (eval (if-decons-expr $arg-types $head-type $tail-types
        (chain (eval (interpret $head $head-type $space)) $reduced-head
          ; check that head was changed otherwise Error or Empty in the head
          ; can be just an argument which is passed by intention
          (eval (if-equal $reduced-head $head
            (chain (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space)) $ret (return $ret))
            (eval (return-on-error $reduced-head
              (chain (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space)) $ret (return $ret)) )))))
        (return (Error $atom BadType)) ))
      (return (Error (interpret-atom $atom $args $arg-types $space) "Non-empty expression atom is expected")) )))))

(= (interpret-args-tail $atom $head $args-tail $args-tail-types $ret-type $space)
  (function (chain (eval (interpret-args $atom $args-tail $args-tail-types $ret-type $space)) $reduced-tail
    (eval (return-on-error $reduced-tail
      (chain (cons-atom $head $reduced-tail) $ret (return $ret)) )))))

(= (interpret-tuple $atom $space)
  (function (unify $atom ()
    (return $atom)
    (eval (if-decons-expr $atom $head $tail
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (eval (if-equal $rhead Empty (return Empty)
          (chain (eval (interpret-tuple $tail $space)) $rtail
            (eval (if-equal $rtail Empty (return Empty)
              (chain (cons-atom $rhead $rtail) $ret (return $ret)) ))))))
      (return (Error (interpret-tuple $atom $space) "Non-empty expression atom is expected as an argument")) )))))

(= (metta-call $atom $type $space)
  (function (eval (if-error $atom (return $atom)
    (chain (eval $atom) $result
      (eval (if-equal $result NotReducible (return $atom)
        (eval (if-equal $result Empty (return Empty)
          (eval (if-error $result (return $result)
            (chain (eval (interpret $result $type $space)) $ret (return $ret)) )))))))))))

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

(: or (-> Bool Bool Bool))
(= (or False False) False)
(= (or False True) True)
(= (or True False) True)
(= (or True True) True)

(: and (-> Bool Bool Bool))
(= (and False False) False)
(= (and False True) False)
(= (and True False) False)
(= (and True True) True)

(: not (-> Bool Bool))
(= (not True) False)
(= (not False) True)

(: let (-> Atom %Undefined% Atom %Undefined%))
(= (let $pattern $atom $template)
  (unify $atom $pattern $template Empty))

(: let* (-> Expression Atom %Undefined%))
(= (let* $pairs $template)
  (eval (if-decons-expr $pairs ($pattern $atom) $tail
    (let $pattern $atom (let* $tail $template))
    $template )))

(: add-reduct (-> Grounded %Undefined% (->)))
(= (add-reduct $dst $atom)  (add-atom $dst $atom))

(: car-atom (-> Expression Atom))
(= (car-atom $atom)
  (eval (if-decons-expr $atom $head $_
    $head
    (Error (car-atom $atom) "car-atom expects a non-empty expression as an argument") )))

(: cdr-atom (-> Expression Expression))
(= (cdr-atom $atom)
  (eval (if-decons-expr $atom $_ $tail
    $tail
    (Error (cdr-atom $atom) "cdr-atom expects a non-empty expression as an argument") )))

(: quote (-> Atom Atom))
(= (quote $atom) NotReducible)

(: unquote (-> %Undefined% %Undefined%))
(= (unquote (quote $atom)) $atom)

; TODO: there is no way to define operation which consumes any number of
; arguments  and returns unit
(= (nop) ())
(= (nop $x) ())

; TODO: MINIMAL added for compatibility, remove after migration
(= (empty) Empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Documentation formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: @doc (-> Atom DocDescription DocInformal))
(: @doc (-> Atom DocDescription DocParameters DocReturnInformal DocInformal))
(: @desc (-> String DocDescription))
(: @param (-> String DocParameterInformal))
(: @return (-> String DocReturnInformal))

(: @doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: @doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))
(: @item (-> Atom DocItem))
(: (@kind function) DocKindFunction)
(: (@kind atom) DocKindAtom)
(: @type (-> Type DocType))
(: @params (-> Expression DocParameters))
(: @param (-> DocType DocDescription DocParameter))
(: @return (-> DocType DocDescription DocReturn))


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
    (if (is-function $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom) ))))

(: get-doc-function (-> Atom Type Atom))
(= (get-doc-function $name $type)
  (let $top-space (mod-space! top)
  (unify $top-space (@doc $name $desc (@params $params) $ret)
    (let $type' (if (== $type %Undefined%) (undefined-doc-function-type $params) (cdr-atom $type))
    (let ($params' $ret') (get-doc-params $params $ret $type')
      (@doc-formal (@item $name) (@kind function) (@type $type) $desc (@params $params') $ret')))
    (@doc-formal (@item $name) (@kind function) (@type $type) (@desc "No documentation")) )))

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
      (let (@return $ret-desc) $ret
        (() (@return (@type $head-type) (@desc $ret-desc))) )
      (let (@param $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (@param (@type $head-type) (@desc $param-desc)) $params')
          ($result-params $result-ret) ))))))))

(: get-doc-atom (-> Atom Atom))
(= (get-doc-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (unify $top-space (@doc $atom $desc)
      (@doc-formal (@item $atom) (@kind atom) (@type $type) $desc)
      (unify $top-space (@doc $atom $desc' (@params $params) $ret)
        (get-doc-function $atom %Undefined%)
        (@doc-formal (@item $atom) (@kind atom) (@type $type) (@desc "No documentation")) )))))

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

(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (@param (@type $type) (@desc $desc)) $param
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

(@doc add-atom
  (@desc "Adds atom into the atomspace without reducing it")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
