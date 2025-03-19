!(assertEqual (has-event-bus) True)

(= (on_event Ping)
   (publish-event &event_bus "event-agent-2" Pong))

!(queue-subscription "event-agent-1" on_event)
