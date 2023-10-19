!(extend-py! llm_gate)

(= (append-expr $xs $x)
   (collapse (superpose ((superpose $xs) $x))))

; We could use $_, so this message would be automatically added to any prompt,
; but in this case it would be difficult to avoid its duplication
(= (message init)
   (system "You are the SingularityNet AI platform and marketplace assistant"))

; We need to duplicate this message
(= (message platform-desc)
   (message init))

(= (message platform-desc)
   (system "SingularityNET (SNET) is an open and decentralized network of AI services made accessible through the Blockchain. Developers publish their services to the SingularityNET network, where they can be used by anyone with an internet connection. Developers are able to charge for the use of their services using the native AGIX token.
Services can span the entire gamut of offerings in artificial intelligence and machine learning. Services can provide inference or model training across myriad domains such as image/video, speech, text, time-series, bio-AI, network analysis, etc. The services can be as simple as wrapping a well-known algorithm such as A* path planning, a complete end-to-end solution for an industry problem, or a standalone AI application. Developers can also deploy autonomous AI agents that interoperate with other services on the network.
The SingularityNET platform contains a number of critical components that work together to enable a decentralized network of AI services to flourish. The core components are designed to allow for a functional, scalable, and extensible system. We arrived at the current architecture through a careful process, guided by a few key decisions governing Blockchain interactions, AI service integration, and abstraction and by the goal of building an AI marketplace that is both open and compliant with regulatory and legal requirements.
First, we made the conscious choice to minimize our dependence on our current Blockchain, Ethereum. Both conceptual and practical issues motivated this decision. Conceptually, we desire to be Blockchain-agnostic and, if necessary, will consider building our own consensus algorithm based on reputation. The speed, reliability, and costs of Ethereum Blockchain interactions dictate that any scalable system built on top of it must minimize gas costs and the delays introduced by block-mining time. These decisions are reflected in our use of tools to abstract away all Blockchain interactions (the daemon, CLI, and SDK) and in our use of a multi-party escrow contract and atomic unidirectional channels for payments.
Second, on AI services integration, we wanted to abstract away as much of the network as possible, in order to reduce the learning curve and minimize the overhead associated with providing AI services via the network. This abstraction is achieved with a single flexible tool, the daemon, that will help us provide scalability, robustness, distribution, and management features to the entire community.
Finally, to make our marketplace compliant with regulations without compromising on openness, we implemented it separately from our fully decentralized registry of AI services currently available on the Blockchain."))

(= (message service-list)
   (system "Select the service, which is most relevant to the user question. The list of services with descriptions
     style-transfer : Provide two images and use this service to transfer the artistic-style of one image to the second image provided.
     image2text-handwritten : The service receives an image of a English-language handwritten text line and outputs the result of  image recognition as a text sequence.
     speech-emotions : Submit a WAV file (up to 4 MB and no longer than 90 seconds) with English speech and get an emotion label from the provided WAV file.
     super-resolution : The service takes a low-resolution image in binary format, uses it as input for a pre-trained model, and outputs the result as a higher-quality image.
   "))

(= (function init)
   set_subgoal)

(= (doc set_subgoal)
   (Doc
     (description "You should always call this function to answer the user")
     (parameters
      (goal "The goal of the user" ("greeting" "general question about platform" "search for particular service" "service list"))
     ))
)

; We suppose that the second argument has a certain structure built by compose-prompt
; so it can be deconstructed by the corresponding argument pattern
(= (set_subgoal
     ("general question about platform")
     (Messages $history $prev-state-msg $user-msg))
   (llm (compose-prompt platform-desc $history $user-msg))
   ; (llm (append-expr $msgs (message platform-desc))
)

; Extracting components of input messages, ignoring $prev-state-msg and calling
; compose-prompt is a standard pattern to keep the user message at the end
; of the message list inserting new system messages in-between
(= (set_subgoal
     ("search for particular service")
     (Messages $history $prev-state-msg $user-msg))
   (llm
     (compose-prompt service-list $history $user-msg)
     (Functions describe_service))
)

; In this example, we just append the list of messages with the new system message
; for further elaboration on the service.
(= (describe_service ($args) $msgs)
   (llm
      (append-expr $msgs (system ("You selected the service " $args " Please describe it.")))))
(= (doc describe_service)
   (Doc
     (description "You should call this function to describe the service.")
     (parameters
      (service "The service to be described"
        ("style-transfer" "image2text-handwritten" "speech-emotions" "super-resolution"))
     ))
)

(= (compose-prompt $state $history $new-msg)
   (let $msgs-state (collapse (message $state))
     (Messages
       $history
       (cons-atom Messages $msgs-state)
       $new-msg)))

(= (compose-functions $state)
   (let $fn-calls (collapse (function $state))
     (cons-atom Functions $fn-calls)))

; TODO? Assuming that $user-msg is not wrapped into (user ...) is convenient
; for the first call, but not convenient for using `respond` from functional calls
(= (respond $state $history $user-msg)
   (llm
     (compose-prompt $state $history (user $user-msg))
     (compose-functions $state)
    ))

; !(compose-prompt init () (user "What services do you have on the platform?"))
; !(compose-functions init)
;!(respond init () "What services do you have on the platform?")
;!(respond init () "What can I use for style transfer?")
;!(respond init () "Hello. What can you do?")
!(respond init () "What is the SingularityNET platform?")
