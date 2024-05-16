!(import! &self das_gate)

;!(bind! &das (new-das))
!(bind! &das (new-remote-das (Error "Put your IP here" ()) (Error "Put port number here" ())))


;!(match &das ($t MYBPP) ($t MYBPP))
!(match &das ($t MYBPP) ($t MYBPP))
!(match &das (translation_of (protein "O43264") (transcript "ENST00000200135")) OK)

!(assertEqualToResult
    (match &das (translation_of (protein "O43264") (transcript "ENST00000200135")) OK)
    ())

!(match &das (translation_of (protein O43264) (transcript ENST00000200135)) OK)

!(assertEqualToResult
    (match &das (translation_of (protein O43264) (transcript ENST00000200135)) OK)
    (OK))
;!(match &das (protein MYBPP) res)
;!(match &das
;            (,
;                (gene $gene)
;                (gene_type (gene $gene) protein_coding)
;            )
;            $gene
;        )
;!(match &das (, (gene_name (gene $ens) IRX3) (genes_pathways (gene $ens) $p)) $p)