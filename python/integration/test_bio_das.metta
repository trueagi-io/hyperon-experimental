!(import! &self das_gate)

;!(bind! &das (new-das))
;!(bind! &das (new-remote-das (Error "Put your IP here" ()) (Error "Put port number here" ())))
!(bind! &das (new-remote-das 64.176.184.225 8080))

;!(match &das ($t MYBPP) ($t MYBPP))
!(match &das ($t MYBPP) ($t MYBPP))
;!(match &das (protein MYBPP) res)
;!(match &das
;            (,
;                (gene $gene)
;                (gene_type (gene $gene) protein_coding)
;            )
;            $gene
;        )
;!(match &das (, (gene_name (gene $ens) IRX3) (genes_pathways (gene $ens) $p)) $p)