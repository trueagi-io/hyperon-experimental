!(import! &self snet_io)

; Only etherium endpoint is needed for retrieving metadata
; It should be either provided via init parameters or
; in the environment variable (ETH_RPC_ENDPOINT).
; In the latter case, it is not necessary to call `init`
; explicitly.
; !(snet-sdk init (eth_rpc_endpoint "YOUR ETH ENDPOINT"))

!(snet-sdk organization_list)

; Both positional and named arguments can be passed to snet-sdk
!(snet-sdk service_list (org_id "snet"))
!(snet-sdk service_list "naint")

; We can create a service client and get its details without
; using a secret key, opening a channel, or using free calls
!(let $service
   (snet-sdk create_service_client "naint" "question-answering-long-seq")
   ((py-dot $service get_service_messages)))

; we use `include` here, so `snet-sdk` is accessible in the included file
!(include snet:naint:image-generation)
!((py-dot (image-generation) get_service_messages))
; turn service description in textual metta code
!((py-dot (image-generation) generate_callers_text))
