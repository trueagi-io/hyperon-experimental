!(import! &self das)

!(bind! &das (new-das! (localhost:52000-52099) (localhost:40002)))

; Check if pattern_matching service is ready, giving it some seconds for handshaking
!(das-service-status! pattern_matching_query)

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE is_animal) (CONCEPT $C)) $C)
    (monkey human triceratops earthworm chimp ent rhino snake))

; Give network some time between queries
!(das-helpers! sleep 2)

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE $P) (CONCEPT snake)) $P)
    (is_animal is_reptile is_dinosaur))

; Give network some time between queries
!(das-helpers! sleep 2)

!(assertEqualToResult
    (match &das (EVALUATION (PREDICATE no_legs) (CONCEPT $C)) $C)
    (earthworm))
