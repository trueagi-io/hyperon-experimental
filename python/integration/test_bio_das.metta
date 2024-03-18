!(extend-py! das_gate)

;!(bind! &das (new-das))
!(bind! &das (new-remote-das (Error "Put your IP here" ()) (Error "Put port number here" ())))

;!(match &das ($t MYBPP) ($t MYBPP))
!(match &das ($t MYBPP) ($t MYBPP))
;!(match &das (protein MYBPP) res)