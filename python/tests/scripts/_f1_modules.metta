!(import! &m _f1_importA.metta)
!(match &m (= (f 2) $x) $x)

!(import! &self _f1_importA.metta)
!(f 2)

!(import! &self _f1_importB.metta)
!(f 2)
!(g 2)
!(dup 2)
