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
