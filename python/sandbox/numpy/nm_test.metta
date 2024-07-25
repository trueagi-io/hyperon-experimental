!(import! &self numme)

; bind a bunch of numpy objects to tokens for convenience
!(bind! &x (np.vector 1 2 3))
!(bind! &y (np.array (3 2 3)))
!(bind! &m (np.array ((6 1) (7 2) (8 3))))
!(bind! &mt (np.array ((1 0 0) (0 1 0))))

; FIXME: no exception?
!(np.vector 1 "S")
; !(np.array (1 (1 2))) ; exception because of bad shape

; wrapping into a function should work
; also works automatically with python numbers
; (no need to turn 1 into np.array)
(= (np-add-1 $x) (np.add $x 1))

!(np-add-1 (np-add-1 &x)) ; [3 4 5] ; not a pattern

; could work with untyped np objects,
; although with some problems (the resulting type would be Number),
; but doesn't work, because + expects Number
; !(+ &x &y)

!(== (np.add &y &x) (np.add &x &y)) ; True
!(== &x &y) ; False
; It becomes BadType now, because types (NPArray (2)) != (NPArray (3))
;!(== &x (np.vector 1 2)) ; False

! (np.add &m (np.vector 1 2)) ; ((7 3) (8 4) (9 5))
! (np.matmul &mt &m) ; ((6 1) (7 2))

; TODO: show that this is really a pattern
! (np.add &m $x) ; will be turned into pattern

! (case (np.array (((1 1) (2 2) (3 3)) ((2 3) (3 4) (4 5))))
    (
        (($a $b $c) $b)
        (($x ($y1 $y2 $y3)) (np.add $x $y2))
    )
  )

! (case (np.vector 1 2 3)
    (
        (&x "X")
    )
  )