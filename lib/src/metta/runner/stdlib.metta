; `$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)
(: Error (-> Atom Atom ErrorType))

(: add-reduct (-> Grounded %Undefined% (->)))
(= (add-reduct $dst $atom)  (add-atom $dst $atom))

; quote prevents atom from being reduced
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
