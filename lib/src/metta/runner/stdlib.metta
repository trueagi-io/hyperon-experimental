(: ErrorType Type)
(: Error (-> Atom Atom ErrorType))
(: ReturnType Type)
(: return (-> Atom ReturnType))

(: function (-> Atom Atom))
(: eval (-> Atom Atom))
(: chain (-> Atom Variable Atom Atom))
(: unify (-> Atom Atom Atom Atom Atom))
(: cons (-> Atom Atom Atom))
(: decons (-> Atom Atom))

(: id (-> Atom Atom))
(= (id $x) $x)

(: insert (-> Atom Variable Atom Atom))
(= (insert $atom $var $templ)
  (function (chain (eval (id $atom)) $var (return $templ))) )

(: if-non-empty-expression (-> Atom Atom Atom Atom))
(= (if-non-empty-expression $atom $then $else)
  (function (chain (eval (get-metatype $atom)) $type
    (eval (if-equal $type Expression
      (eval (if-equal $atom () (return $else) (return $then)))
      (return $else) )))))

(: if-decons (-> Atom Variable Variable Atom Atom Atom))
(= (if-decons $atom $head $tail $then $else)
  (function (eval (if-non-empty-expression $atom
    (chain (decons $atom) $list
      (unify $list ($head $tail) (return $then) (return $else)) )
    (return $else) ))))

(: if-empty (-> Atom Atom Atom Atom))
(= (if-empty $atom $then $else)
  (function (eval (if-equal $atom Empty (return $then) (return $else)))) )

(: if-not-reducible (-> Atom Atom Atom Atom))
(= (if-not-reducible $atom $then $else)
  (function (eval (if-equal $atom NotReducible (return $then) (return $else)))) )

(: if-error (-> Atom Atom Atom Atom))
(= (if-error $atom $then $else)
  (function (eval (if-decons $atom $head $_
    (eval (if-equal $head Error (return $then) (return $else)))
    (return $else) ))))

(: return-on-error (-> Atom Atom Atom))
(= (return-on-error $atom $then)
  (function (eval (if-empty $atom (return (return Empty))
    (eval (if-error $atom (return (return $atom))
      (return $then) ))))))

(: switch (-> %Undefined% Expression Atom))
(= (switch $atom $cases)
  (function (chain (decons $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (chain (eval (if-not-reducible $res Empty $res)) $x (return $x)) ))))

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

(= (type-cast $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type $meta
      (return $atom)
      (chain (eval (get-type $atom $space)) $actual-type
        (eval (match-types $actual-type $type
          (return $atom)
          (return (Error $atom BadType)) ))))))))

(= (is-function $type)
  (function (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta) (
      (($_ Expression)
        (eval (if-decons $type $head $_tail
          (unify $head -> (return True) (return False))
          (return (Error (is-function $type) "is-function non-empty expression as an argument")) )))
      ($_ (return False))
    ))))))

(= (interpret $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type Atom
      (return $atom)
      (eval (if-equal $type $meta
        (return $atom)
        (eval (switch ($type $meta) (
          (($_type Variable) (return $atom))
          (($_type Symbol)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($_type Grounded)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($_type Expression)
            (chain (eval (interpret-expression $atom $type $space)) $ret (return $ret)))
        ))))))))))

(= (interpret-expression $atom $type $space)
  (function (eval (if-decons $atom $op $args
    (chain (eval (get-type $op $space)) $op-type
      (chain (eval (is-function $op-type)) $is-func
        (unify $is-func True
          (chain (eval (interpret-func $atom $op-type $type $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) )
          (chain (eval (interpret-tuple $atom $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) ))))
    (chain (eval (type-cast $atom $type $space)) $ret (return $ret)) ))))

(= (interpret-func $expr $type $ret-type $space)
  (function (eval (if-decons $expr $op $args
    (chain (eval (interpret $op $type $space)) $reduced-op
      (eval (return-on-error $reduced-op
        (eval (if-decons $type $arrow $arg-types
          (chain (eval (interpret-args $expr $args $arg-types $ret-type $space)) $reduced-args
            (eval (return-on-error $reduced-args
              (chain (cons $reduced-op $reduced-args) $r (return $r)))))
          (return (Error $type "Function type expected")) )))))
    (return (Error $expr "Non-empty expression atom is expected")) ))))

(= (interpret-args $atom $args $arg-types $ret-type $space)
  (function (unify $args ()
    (eval (if-decons $arg-types $actual-ret-type $_tail
      (eval (match-types $actual-ret-type $ret-type
        (return ())
        (return (Error $atom BadType)) ))
      (return (Error (interpret-args $atom $args $arg-types $ret-type $space) "interpret-args expects a non-empty value for $arg-types argument")) ))
    (eval (if-decons $args $head $tail
      (eval (if-decons $arg-types $head-type $tail-types
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
      (chain (cons $head $reduced-tail) $ret (return $ret)) )))))

(= (interpret-tuple $atom $space)
  (function (unify $atom ()
    (return $atom)
    (eval (if-decons $atom $head $tail
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (eval (if-empty $rhead (return Empty)
          (chain (eval (interpret-tuple $tail $space)) $rtail
            (eval (if-empty $rtail (return Empty)
              (chain (cons $rhead $rtail) $ret (return $ret)) ))))))
      (return (Error (interpret-tuple $atom $space) "Non-empty expression atom is expected as an argument")) )))))

(= (metta-call $atom $type $space)
  (function (eval (if-error $atom (return $atom)
    (chain (eval $atom) $result
      (eval (if-not-reducible $result (return $atom)
        (eval (if-empty $result (return Empty)
          (eval (if-error $result (return $result)
            (chain (eval (interpret $result $type $space)) $ret (return $ret)) )))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Standard library written in MeTTa ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

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

(: quote (-> Atom Atom))
(= (quote $atom) NotReducible)

(: unquote (-> %Undefined% %Undefined%))
(= (unquote (quote $atom)) $atom)
