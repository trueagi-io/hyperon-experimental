;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

(: Error (-> Atom Atom ErrorType))

(: if-non-empty-expression (-> Atom Atom Atom Atom))
(= (if-non-empty-expression $atom $then $else)
  (chain (eval (get-metatype $atom)) $type
    (eval (if-equal $type Expression
      (eval (if-equal $atom () $else $then))
      $else ))))

(: if-decons (-> Atom Variable Variable Atom Atom Atom))
(= (if-decons $atom $head $tail $then $else)
  (eval (if-non-empty-expression $atom
    (chain (decons $atom) $list
      (unify $list ($head $tail) $then $else) )
    $else )))

(: if-empty (-> Atom Atom Atom Atom))
(= (if-empty $atom $then $else)
  (eval (if-equal $atom Empty $then $else)))

(: if-not-reducible (-> Atom Atom Atom Atom))
(= (if-not-reducible $atom $then $else)
  (eval (if-equal $atom NotReducible $then $else)))

(: if-error (-> Atom Atom Atom Atom))
(= (if-error $atom $then $else)
  (eval (if-decons $atom $head $_
    (eval (if-equal $head Error $then $else))
    $else )))

(: return-on-error (-> Atom Atom Atom))
(= (return-on-error $atom $then)
  (eval (if-empty $atom Empty
    (eval (if-error $atom $atom
      $then )))))

(: switch (-> %Undefined% Expression Atom))
(= (switch $atom $cases)
  (chain (decons $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (eval (if-not-reducible $res Empty $res)) )))
(= (switch-internal $atom (($pattern $template) $tail))
  (unify $atom $pattern $template (eval (switch $atom $tail))))

; FIXME: subst and reduce are not used in interpreter implementation
; we could remove them

(: subst (-> Atom Variable Atom Atom))
(= (subst $atom $var $templ)
  (unify $atom $var $templ
    (Error (subst $atom $var $templ)
      "subst expects a variable as a second argument") ))

(: reduce (-> Atom Variable Atom Atom))
(= (reduce $atom $var $templ)
  (chain (eval $atom) $res
    (eval (if-empty $res Empty
      (eval (if-error $res $res
        (eval (if-not-reducible $res
          (eval (subst $atom $var $templ))
          (eval (reduce $res $var $templ)) ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa interpreter implementation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (match-types $type1 $type2 $then $else)
    (eval (if-equal $type1 %Undefined% $then
      (eval (if-equal $type2 %Undefined% $then
        (eval (if-equal $type1 Atom $then
          (eval (if-equal $type2 Atom $then
            (unify $type1 $type2 $then $else) )))))))))

(= (type-cast $atom $type $space)
  (chain (eval (get-type $atom $space)) $actual-type
    (chain (eval (get-metatype $atom)) $meta
      (eval (if-equal $type $meta $atom
        (eval (match-types $actual-type $type $atom (Error $atom BadType))) )))))

(= (is-function $type)
  (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta)
      (
        (($_ Expression)
          (chain (eval (car $type)) $head
            (unify $head -> True False) ))
        ($_ False) )))))

(= (interpret $atom $type $space)
  (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type Atom $atom
      (eval (if-equal $type $meta $atom
        (eval (switch ($type $meta)
          (
            (($_type Variable) $atom)
            (($_type Symbol) (eval (type-cast $atom $type $space)))
            (($_type Grounded) (eval (type-cast $atom $type $space)))
            (($_type Expression) (eval (interpret-expression $atom $type $space))) )))))))))

(= (interpret-expression $atom $type $space)
  (eval (if-decons $atom $op $args
    (chain (eval (get-type $op $space)) $op-type
      (chain (eval (is-function $op-type)) $is-func
        (unify $is-func True
          (chain (eval (interpret-func $atom $op-type $type $space)) $reduced-atom
            (eval (call $reduced-atom $type $space)) )
          (chain (eval (interpret-tuple $atom $space)) $reduced-atom
            (eval (call $reduced-atom $type $space)) ))))
    (eval (type-cast $atom $type $space)) )))

(= (interpret-func $expr $type $ret-type $space)
  (eval (if-decons $expr $op $args
    (chain (eval (interpret $op $type $space)) $reduced-op
      (eval (return-on-error $reduced-op
        (eval (if-decons $type $arrow $arg-types
          (chain (eval (interpret-args $expr $args $arg-types $ret-type $space)) $reduced-args
            (eval (return-on-error $reduced-args
              (cons $reduced-op $reduced-args) )))
          (Error $type "Function type expected") )))))
    (Error $expr "Non-empty expression atom is expected") )))

(= (interpret-args $atom $args $arg-types $ret-type $space)
  (unify $args ()
    (chain (eval (car $arg-types)) $actual-ret-type
      (eval (match-types $actual-ret-type $ret-type () (Error $atom BadType))))
    (eval (if-decons $args $head $tail
      (eval (if-decons $arg-types $head-type $tail-types
        (chain (eval (interpret $head $head-type $space)) $reduced-head
          ; check that head was changed otherwise Error or Empty in the head
          ; can be just an argument which is passed by intention
          (eval (if-equal $reduced-head $head
            (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space))
            (eval (return-on-error $reduced-head
              (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space)) )))))
        (Error $atom BadType) ))
      (Error (interpret-atom $atom $args $arg-types $space)
        "Non-empty expression atom is expected") ))))

(= (interpret-args-tail $atom $head $args-tail $args-tail-types $ret-type $space)
  (chain (eval (interpret-args $atom $args-tail $args-tail-types $ret-type $space)) $reduced-tail
    (eval (return-on-error $reduced-tail
      (cons $head $reduced-tail) ))))

(= (interpret-tuple $atom $space)
  (unify $atom ()
    $atom
    (eval (if-decons $atom $head $tail
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (eval (if-empty $rhead Empty
          (chain (eval (interpret-tuple $tail $space)) $rtail
            (eval (if-empty $rtail Empty
              (cons $rhead $rtail) ))))))
      (Error (interpret-tuple $atom $space) "Non-empty expression atom is expected as an argument") ))))

(= (call $atom $type $space)
  (eval (if-error $atom $atom
    (chain (eval $atom) $result
      (eval (if-not-reducible $result $atom
        (eval (if-empty $result Empty
          (eval (if-error $result $result
            (eval (interpret $result $type $space)) ))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Standard library written in MeTTa ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: match (-> Atom Atom Atom %Undefined%))
(= (match $space $pattern $template)
  (unify $pattern $space $template Empty))

(: let (-> Atom %Undefined% Atom Atom))
(= (let $pattern $atom $template)
  (unify $atom $pattern $template Empty))

(: let* (-> Expression Atom Atom))
(= (let* $pairs $template)
  (eval (if-decons $pairs ($pattern $atom) $tail
    (let $pattern $atom (let* $tail $template))
    $template )))

(: case (-> %Undefined% Expression Atom))
(= (case $atom $cases) (switch $atom $cases))

(: car (-> Expression Atom))
(= (car $atom)
  (eval (if-decons $atom $head $_
    $head
    (Error (car $atom) "car expects a non-empty expression as an argument") )))

(: cdr (-> Expression Expression))
(= (cdr $atom)
  (eval (if-decons $atom $_ $tail
    $tail
    (Error (cdr $atom) "cdr expects a non-empty expression as an argument") )))
