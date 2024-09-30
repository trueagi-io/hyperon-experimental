!(import! &self agents)

!((create-agent agent.metta) (g 3)) ; 9

!((create-agent (quote (= (f $x) (+ 1 $x)))) (f 10)) ; 11
