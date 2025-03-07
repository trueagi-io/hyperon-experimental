!(import! &self agents)
; NOTE: atm, events are not imported into agents automatically
!(import! &self agents:events)

!(bind! &events (basic-event-bus))
!(bind! &agent1
   (event-agent test_4_agent1.metta &events))
!(bind! &agent2
   (event-agent test_4_agent2.metta &events))

!(&agent1 .start)
!(&agent2 .start)

!(publish-event &events "command" Start)

; Add sleep time if assert doesn't work
!((py-atom time.sleep) 0.05)

!(assertEqual (get-state (&agent1 (success?)))
  True)

!(&agent1 .stop)
!(&agent2 .stop)
