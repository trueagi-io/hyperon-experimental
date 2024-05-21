!(bind! ChatOpenAI (py-atom langchain_openai.ChatOpenAI))
!(bind! ChatPromptTemplate (py-atom langchain_core.prompts.ChatPromptTemplate))
!(bind! StrOutputParser (py-atom langchain_core.output_parsers.StrOutputParser))

!(bind! model (ChatOpenAI (Kwargs (temperature 0) (model "gpt-3.5-turbo"))))

!(bind! prompt ( (py-dot ChatPromptTemplate from_template) "tell me a joke about cat"))

!(bind! chain1 (py-chain  (prompt model (StrOutputParser)) ))

!( (py-dot chain1 invoke) (py-dict ()))

!(bind! prompt2 ( (py-dot ChatPromptTemplate from_messages ) (py-tuple (("system" "You are very funny") ("user" "tell me joke about {foo}")))))

!(bind! chain2 (py-chain (prompt2 model (StrOutputParser)) ))

!((py-dot chain2 invoke) (py-dict (("foo" "dogs"))))

