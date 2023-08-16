; NOTE: this behavior can change in the future
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Even at the very beginning of the main script `(get-atoms &self)`
; returns one atom, which wraps the space of stdlib.
; The type of this atom is the same as of `&self`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  ((let $x (get-atoms &self) (get-type $x)))
  ((get-type &self)))

; stdlib is already loaded
!(assertEqual
  (if (> 1 2) 1 2)
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Importing the module into new space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &m f1_moduleA.metta)

; It's first atom is a space
!(assertEqual
  (let* (($x (collapse (get-atoms &m)))
         ($y (car-atom $x)))
        (get-type $y))
  (get-type &self))

; FIXME? Now, it is moduleC space.
;        Should it be `stdlib` atom for a separately imported space
; !(let $x (collapse (get-atoms &m)) (car-atom $x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Without additional means like `(&m.f 2)` notation or `(interpret &m (f 2))`,
; we cannot execute functions from the separate space - we can only use `match`.
; Although `&m` imports another space with definition of `g`, it is not reduced
; because it is not defined in the context of `&self`. This is the expected
; behavior, but it shows that this way of importing spaces is not too useful
; for importing modules with functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  (match &m (= (f 2) $x) $x)
  (g 3))

; Importing the same space into `&self` should break nothing
; TODO? If stdlib space would be in `&m`, which should check that it is not
; there anymore since in should be removed after importing it to `&self`
!(import! &self f1_moduleA.metta)

; Now indirectly imported `g` works and `f` fully works
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)

; `&self` contains 3 atoms-spaces now:
; - stdlib
; - moduleC imported by moduleA and removed from A after its import to &self
; - moduleA itself, which is the same as &m
!(assertEqual &m
  (let* (($a (collapse (get-atoms &self)))
          ($x (cdr-atom $a))
          ($y (cdr-atom $x)))
         (car-atom $y)))

; NOTE: now the first atom, which was a space, is removed from `&m`,
; because we load modules only once, and we collect atoms-spaces to
; prevent duplication
!(assertEqual
  (== (let* (($x (collapse (get-atoms &m)))
             ($y (car-atom $x)))
            (get-type $y))
        (get-type &self))
   False)

; Let's check that `if` from stdlib is not duplicated and gives only one result
!(assertEqual
  (if (> 1 2) 1 2)
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's import one more module into `&self` with a diamond dependence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &self f1_moduleB.metta)
; `g` from moduleC imported via two paths as well as `f`, which uses `g`,
; are not duplicated and produce deterministic results
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)

; Function declared in different imported modules will still produce
; non-deterministic results
!(assertEqualToResult
  (dup 2)
  (12 102))

; Let's import f1_moduleB.metta once more using a different path.
; Such import should be ignored and thus f, g and dup should remain
; unchanged.
!(import! &self ../scripts/f1_moduleB.metta)
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)
!(assertEqualToResult
  (dup 2)
  (12 102))
