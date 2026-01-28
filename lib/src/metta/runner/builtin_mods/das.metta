(@doc new-das!
  (@desc "Creates a new Distributed AtomSpace (DAS) instance with specified server and known peer endpoints")
  (@params (
    (@param "Server endpoint with port range (e.g., 0.0.0.0:42000-42999 or localhost:52000-52099)")
    (@param "Known peer endpoint (e.g., 0.0.0.0:40002 or localhost:40002)")))
  (@return "A Space instance representing the DAS"))

(@doc das-join-network!
  (@desc "(re)Joins the DAS network using the configured parameters")
  (@params ())
  (@return "Unit atom"))

(@doc das-services!
  (@desc "Prints information about available DAS services")
  (@params ())
  (@return "Unit atom"))

(@doc das-service-status!
  (@desc "Checks the status of a specific DAS service by name")
  (@params (
    (@param "Service name (string)")))
  (@return "String: 'ServiceAvailable: <name>' if available, or error if not available or not registered"))

(@doc das-set-param!
  (@desc "Sets a DAS parameter value. The parameter must exist in the DAS configuration")
  (@params (
    (@param "Expression containing key-value pair, e.g., (max_answers 100)")))
  (@return "String: 'DAS Param Updated: <param_name>' or error"))

(@doc das-get-params!
  (@desc "Prints all current DAS parameters")
  (@params ())
  (@return "Unit atom"))

(@doc das-create-context!
  (@desc "Creates a context in the DAS with specified query, determiner schema, and stimulus schema")
  (@params (
    (@param "Context name (string)")
    (@param "ExpressionAtom containing schemas: ( (query) (determiner_schema) (stimulus_schema) )")))
  (@return "Result atom from context creation"))

(@doc das-evolution!
  (@desc "Performs DAS evolution operation with query, fitness function, correlation queries, replacements, and mappings")
  (@params (
    (@param "ExpressionAtom containing evolution configuration: ( (query) (fitness-function) (correlation-queries) (correlation-replacements) (correlation-mappings) )")
    (@param "Template atom to be filled with evolution results")))
  (@return "Non-deterministic result: template with bindings from evolution"))

(@doc das-link-creation!
  (@desc "Creates links in the DAS based on a query pattern and one or more templates")
  (@params (
    (@param "Query atom to match against")
    (@param "Template atoms (one or more) for link creation")))
  (@return "Result atom from link creation"))

(@doc das-helpers!
  (@desc "Executes helper functions for DAS operations. Currently supports 'sleep' helper")
  (@params (
    (@param "Helper function name (string), e.g., 'sleep'")
    (@param "Argument for the helper function (string), e.g., '5' for sleep duration in seconds")))
  (@return "Unit atom"))
