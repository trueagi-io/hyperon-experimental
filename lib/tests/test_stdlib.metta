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

!(assertEqual (assertEqualMsg (+ 1 2) 4 "Test message") ("Test message"))
!(assertEqual (assertAlphaEqualMsg (f1 A) (f2 B) "Test message") ("Test message"))
!(assertEqual (assertAlphaEqualToResultMsg ((foo) (foo)) (($x $x)) "Test message") ("Test message"))
!(assertEqual (assertEqualToResultMsg
                 (noeval (+ 1 2))
                 ((+ 1 3))
                 "Test message")
              ("Test message"))
