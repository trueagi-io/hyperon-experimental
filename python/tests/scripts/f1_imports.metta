; NOTE: This test won't work outside the test environment because it relies on
;  specific atoms in a specific order in the space, and loading the default environment's
;  init.metta will break the assumptions in this test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Even at the very beginning of the script `(get-atoms &self)`
; returns two atoms.  One is from the imported stdlib, and the other
; is corelib, which was a dependency of stdlib that has been promoted
; These atoms are both wrapped spaces, as is `&self`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
  ((let $x (get-atoms &self) (get-type $x)))
  (superpose (((get-type &self)) ((get-type &self)))))

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

; It's first atom is a space
!(assertEqual
  (let $x (collapse (get-atoms &m)) (contains $x is-space))
  True)

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
!(import! &self f1_moduleA)

; Now indirectly imported `g` works and `f` fully works
!(assertEqual (g 2) 102)
!(assertEqual (f 2) 103)

; `&self` contains 4 grounded sub-spaces now:
; - stdlib
; - corelib
; - moduleA itself, which is the same as &m
; - moduleC imported by moduleA and removed from A after its import to &self

; Check whether atom is &m
(: is-m (-> Atom Bool))
(= (is-m $atom) (== $atom &m))

; Assert that the &self space contains the same space as &m, which we imported from moduleA
; TODO: Comparing spaces like this doesn't work because the source module is the same, but a specialized
;   clone of the dependent space without transitive-dependencies was created during the import process.
;   Ideally, we can rework importing so that a special space copy isn't created, and then comparing
;   spaces will work again.  But, In my opinion comparing spaces is not a good way to check to see if a
;   module has been loaded.  I believe a better solution is accessor operations for loaded & imported modules
;
;!(assertEqual
;  (let $a (collapse (get-atoms &self)) (contains $a is-m))
;  True)

; Check that the &self space contains the corelib child space
; Note: corelib doesn't import any modules into itself, so no space copy is needed
!(import! &corelib corelib)
(: is-corelib (-> Atom Bool))
(= (is-corelib $atom) (== $atom &corelib))
!(assertEqual
  (let $a (collapse (get-atoms &self)) (contains $a is-corelib))
  True)

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
