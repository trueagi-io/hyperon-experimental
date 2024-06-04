!(import! &self snet_io)

!(add-reduct &self
  (= (code-generation)
     (snet-service "naint" "code-generation"
       ; Put your data here or use environment variables
       ;(Kwargs
       ;  (eth_rpc_endpoint ETH_RPC_ENDPOINT)
       ;  (email SNET_EMAIL)
       ;  (free_call_auth_token_bin )
       ;)
     )))

(: generate (-> String String))
(= (generate $request)
   ((code-generation) "generate" "Query"
    (Kwargs (request $request))
   )
)
