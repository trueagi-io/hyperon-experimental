!(import! &self agents)

; checking that we can create passive agents and call their functions
; passive agents do nothing unless they are directly called

; basic agent from file
!(assertEqual
  ((create-agent agent.metta) (g 3))
  9)

; basic agent from expression
!(assertEqual
  ((create-agent (quote (= (f $x) (+ 1 $x)))) (f 10))
  11)

; event-agent has the same behavior when event_bus is not provided
!(add-reduct &self
  (= (my-agent) (event-agent agent.metta)))
; multiple consequent calls should work
!(assertEqual
   ((my-agent) (h 3 5))
   15)
!(assertEqual
   ((my-agent) (g 4))
   16)

; atm, we can access Python methods of the agent, although
; they are all supposed to be unwrap=True
; also, event-agent without event bus is not a daemon
!(assertEqual
   ((my-agent) .is_daemon)
   False)

