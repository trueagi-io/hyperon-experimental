!(import! &self simple_import)

!(import_from langchain_openai import ChatOpenAI)
!(import_from langchain_core.prompts import ChatPromptTemplate)
!(import_from langchain_core.output_parsers import StrOutputParser)


!(bind! model (ChatOpenAI (temperature 0) (model "gpt-3.5-turbo")))

!(bind! prompt (call_dot ChatPromptTemplate from_template "tell me a joke about cat"))

!(bind! chain1 (chain prompt model (StrOutputParser) ))

!(__unwrap(call_dot chain1 invoke (pdict)))

!(bind! prompt2 (call_dot ChatPromptTemplate from_messages (ptuple ("system" "You are very funny") ("user" "tell me joke about {foo}"))))

!(bind! chain2 (chain prompt2 model (StrOutputParser) ))

!(__unwrap(call_dot chain2 invoke (pdict (foo "dogs") )))

