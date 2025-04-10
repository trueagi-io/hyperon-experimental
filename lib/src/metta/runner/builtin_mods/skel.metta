(@doc skel-swap-pair
  (@desc "Swap atoms of the pair")
  (@params (
    (@param "Pair of atoms") ))
  (@return "Pair with original atoms swapped") )
(: skel-swap-pair (-> ($ta $tb) ($tb $ta)))
(= (skel-swap-pair ($a $b))
   ($b $a))

(@doc skel-swap-pair-native
  (@desc "Swap atoms of the pair in Rust")
  (@params (
    (@param "Pair of atoms") ))
  (@return "Pair with original atoms swapped") )
