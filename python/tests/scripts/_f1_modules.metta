!(import! &m _d1_importA.metta)
!(match &m (= (f 2) $x) $x)

!(import! &self _d1_importA.metta)
!(f 2)

!(import! &self _d1_importB.metta)
!(f 2)
!(g 2)
!(dup 2)
