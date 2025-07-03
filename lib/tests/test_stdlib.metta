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
