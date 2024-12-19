; NOTE: This test won't work under no python mode because it relies on
;  specific atoms in the space.  When running in Python, corelib and stdlib
;  are separate modules, but in no python mode there is no Python stdlib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; At the very beginning of the script `(get-atoms &self)`
; returns no atoms. In fact it imports one or two libraries:
; corelib is a necessary core library and stdlib if Python is enabled.
; But these libraries are considered to be a part of the space content.
; TODO: if get-deps is implemented we could check it returns corelib and stdlib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqualToResult
  ((let $x (get-atoms &self) (get-type $x)))
  ())


; stdlib is already loaded
!(assertEqual
  (if (> 1 2) 1 2)
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Importing the module into new space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &m f1_moduleA)

; Check whether passed expression contains atom for which condition is True
(: contains (-> Expression (-> Atom Bool) Bool))
(= (contains $list $condition)
  (if (== $list ()) False
    (let $head (car-atom $list)
      (if ($condition $head) True
        (let $tail (cdr-atom $list) (contains $tail $condition)) ))))

; Check whether atom is space comparing its type with type of the &self atom
(: is-space (-> Atom Bool))
(= (is-space $atom)
   (let* (($type (get-type $atom)) ($space (get-type &self))) (== $type $space)))

; It is not returned by get-atoms as well
; TODO: after implementing get-deps we can check new dependency is added
!(assertEqual
  (let $x (collapse (get-atoms &m)) (contains $x is-space))
  False)

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
!(import! &self f1_moduleA)

; Now indirectly imported `g` works and `f` fully works
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)

; `&self` contains 4 grounded sub-spaces now:
; - stdlib
; - corelib
; - moduleA itself, which is the same as &m
; - moduleC imported by moduleA and removed from A after its import to &self

; Assert that the &self space contains the same space as &m, which we imported from moduleA
; TODO: Comparing spaces like this doesn't work because the source module is the same, but a specialized
;   clone of the dependent space without transitive-dependencies was created during the import process.
;   Ideally, we can rework importing so that a special space copy isn't created, and then comparing
;   spaces will work again.  But, In my opinion comparing spaces is not a good way to check to see if a
;   module has been loaded.  I believe a better solution is accessor operations for loaded & imported modules
;
; Check whether atom is &m
;(: is-m (-> Atom Bool))
;(= (is-m $atom) (== $atom &m))
;!(assertEqual
;  (let $a (collapse (get-deps &self)) (contains $a is-m))
;  True)

; Check that the &self space contains the corelib child space
; Note: corelib doesn't import any modules into itself, so no space copy is needed
; TODO: this should be possible after implementing get-deps
;!(import! &corelib top:corelib)
;(: is-corelib (-> Atom Bool))
;(= (is-corelib $atom) (== $atom &corelib))
;!(assertEqual
  ;(let $a (collapse (get-deps &self)) (contains $a is-corelib))
  ;True)

; Let's check that `if` from stdlib is not duplicated and gives only one result
!(assertEqual
  (if (> 1 2) 1 2)
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's import one more module into `&self` with a diamond dependence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(import! &self f1_moduleB)
; `g` from moduleC imported via two paths as well as `f`, which uses `g`,
; are not duplicated and produce deterministic results
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)

; Function declared in different imported modules will still produce
; non-deterministic results
!(assertEqualToResult
  (dup 2)
  (12 102))

; Let's import f1_moduleB once more. Such import should be ignored and
; thus f, g and dup should remain unchanged.
!(import! &self f1_moduleB)
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)
!(assertEqualToResult
  (dup 2)
  (12 102))
