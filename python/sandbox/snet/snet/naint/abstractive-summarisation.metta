!(import! &self snet_io)

!(add-reduct &self
  (= (abstractive-summarisation)
     (snet-sdk create_service_client "naint" "abstractive-summarisation"
       ; Put your data here or use environment variables
       ;(Kwargs
       ;  (free_call_auth_token_bin FREE_CALL_AUTH_TOKEN_BIN)
       ;  (free_call_token_expiry_block FREE_CALL_TOKEN_EXPIRE_BLOCK)
       ;)
     )))
