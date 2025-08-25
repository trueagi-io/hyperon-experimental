; superpose
!(assertEqualToResult
   (superpose (red yellow green))
   (red yellow green))
(= (foo) FOO)
(= (bar) BAR)
!(assertEqualToResult
   (superpose ((foo) (bar) BAZ))
   (FOO BAR BAZ))

; case on empty results
!(assertEqualToResult
   (case Empty ((ok ok) (Empty nok)))
   (nok))
!(assertEqualToResult
   (case (unify (C B) (C B) ok  Empty) ((ok ok) (Empty nok)))
   (ok))
!(assertEqualToResult
   (case (unify (B C) (C B) ok  nok) ((ok ok) (nok nok)))
   (nok))
!(assertEqualToResult
   (case (unify (B C) (C B) ok  Empty) ((ok ok) (Empty nok)))
   (nok))

; case/switch/switch-minimal check if result is being reduced or not

!(assertEqual
    (case 5 ((4 False) (5 (+ 1 2))))
    3)

!(assertEqual
    (switch 5 ((4 False) (5 (+ 1 2))))
    3)

!(assertEqual
    (switch-minimal 5 ((4 False) (5 (+ 1 2))))
    (noeval (+ 1 2)))

; assertIncludes
!(assertEqualToResult
   (assertIncludes (superpose (41 42 43)) (42))
   (()))
!(assertEqualToResult
   (assertIncludes (superpose (41 42 43)) (42 43))
   (()))
!(assertEqualToResult
   (assertIncludes (superpose (41 42 43)) (A))
   ((Error (assertIncludes (superpose (41 42 43)) (A)) (assertIncludes error: (A) not included in result: (41 42 43)))))

!(assertEqualToResult
   (= (f) (+ 1 2))
   ((= (f) 3)))
!(assertEqualToResult
   (noeval (= (f) (+ 1 2)))
   ((= (f) (+ 1 2))))
!(assertEqualToResult
   (quote (= (f) (+ 1 2)))
   ((quote (= (f) (+ 1 2)))))

!(assertEqualToResult
   (noeval (+ 1 2))
   ((+ 1 2)))
!(assertEqualToResult
   (id (noeval (+ 1 2)))
   (3))

; assert

!(assert True)
!(assert (or True False))
!(assertEqual (assert (and True False)) (Error (assert (and True False)) ((and True False) not True)))

; asserts with custom messages
; first part of tests is to check if asserts works as intended when assert should output [()].
; So those tests are duplicate of tests for original asserts

!(assertEqualToResultMsg
   (noeval (+ 1 2))
   ((+ 1 2))
   "Test message")

!(assertEqualToResultMsg
   (= (f) (+ 1 2))
   ((= (f) 3))
   "Test message")

(= (f1 $x) $x)
(= (f2 $x) $x)

!(assertEqualMsg (+ 1 2) 3 "Test message")
!(assertAlphaEqualMsg (f1 $x) (f2 $x) "Test message")

(= (f3) $x)
!(assertAlphaEqualToResultMsg ((f3) (f3)) (($x $y)) "Test message")

; next part of tests is to test if message is being output on assert failure

!(assertEqual (assertEqualMsg (+ 1 2) 4 "Test message")
              (Error (assertEqualMsg (+ 1 2) 4) "Test message"))

!(assertEqual (assertAlphaEqualMsg (f1 A) (f2 B) "Test message")
              (Error (assertAlphaEqualMsg (f1 A) (f2 B)) "Test message"))

!(assertEqual (assertAlphaEqualToResultMsg ((foo) (foo)) (($x $x)) "Test message")
              (Error (assertAlphaEqualToResultMsg ((foo) (foo)) (($x $x))) "Test message"))

!(assertEqual (assertEqualToResultMsg (noeval (+ 1 2)) ((+ 1 3)) "Test message")
              (Error (assertEqualToResultMsg (noeval (+ 1 2)) ((+ 1 3))) "Test message"))

; Test foldl (issue 857) after adding sealed

(= (remove-857 $list $elem)
   (if-decons-expr $list $head $tail
      (unify $elem $head ($head $tail) (let ($res $ntail) (remove-857 $tail $elem) ($res (cons-atom $head $ntail))))
      (() $list)
))

(= (overlap-857 $list1 $list2)
   (foldl-atom $list1 (() () $list2) $accum $elem
    (let ($left $intersection $right) $accum
     (let ($res $nright) (remove-857 $right $elem)
      (if (== $res ())
       ((cons-atom $elem $left) $intersection $right)
       ($left (cons-atom $res $intersection) $nright))))))

!(assertEqual (overlap-857 (a b c) (b c d)) ((a) (c b) (d)))

!(assertEqual (let $f_hyps ((⟨wff⟩ ⟨P⟩) (⟨wff⟩ ⟨Q⟩)) (map-atom $f_hyps $f_hyp (let ($typecode $mvar) $f_hyp $mvar))) (⟨P⟩ ⟨Q⟩))

; Test match function (see issue 911)
(issue911 A B)
(= (isa911 $x $y) (match &self (issue911 $x $y) True))
!(assertEqual ((isa911 $a B) $a) (True A))
