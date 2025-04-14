(: PairType (-> $ta $tb Type))
(: Pair (-> $ta $tb (PairType $ta $tb)))

(@doc skel-swap-pair
  (@desc "Swap atoms of the pair")
  (@params (
    (@param "Pair of atoms") ))
  (@return "Pair with original atoms swapped") )
(: skel-swap-pair (-> (PairType $ta $tb) (PairType $tb $ta)))
(= (skel-swap-pair (Pair $a $b))
   (Pair $b $a))

(@doc skel-swap-pair-native
  (@desc "Swap atoms of the pair in Rust")
  (@params (
    (@param "Pair of atoms") ))
  (@return "Pair with original atoms swapped") )
