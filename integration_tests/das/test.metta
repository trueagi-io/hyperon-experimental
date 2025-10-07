!(import! &self das)

!(bind! &das (new-das! (0.0.0.0:42000-42499) (0.0.0.0:40002)))

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE is_animal) (CONCEPT $C)) $C)
    (monkey human triceratops earthworm chimp ent rhino snake))

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE $P) (CONCEPT snake)) $P)
    (is_animal is_reptile is_dinosaur))

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE no_legs) (CONCEPT $C)) $C)
    (earthworm))
