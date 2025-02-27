!(assertEqual (has-event-bus) True)

!(let $state (new-state False)
    (add-atom &self (= (success?) $state)))

(= (on_command Start)
   (let $_ ((py-atom time.sleep) 0.005)
     (publish-event &event_bus "event-agent-1" Ping)))

(= (on_event Pong)
   (change-state! (success?) True))

!(queue-subscription "command" on_command)
!(queue-subscription "event-agent-2" on_event)

