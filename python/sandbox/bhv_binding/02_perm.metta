!(extend-py! bhv_binding)

!(bind! &a (bhv-new))
!(bind! &b (bhv-new))

!(bind! &perm1 (bhv-new-perm))
(= (perm1 $x) (bhv-apply-perm &perm1 $x))

(= (ab) (bhv-majority &a &b ))

; Should return False since a is permuted but ab is not permuted
!(bhv-is-related (perm1 &a) (ab))

;Should return True
!(bhv-is-related (perm1 &a) (perm1 (ab)))






