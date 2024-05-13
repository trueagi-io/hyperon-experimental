
; Use this file to configure the environment.  Settings from this file will be used in
; addition to settings configured programmatically through the EnvironmentBuilder API.
; The API will always take precedence over settings from this file if there is a conflict.

; NOTE: stdlib operations such as string manipulation and arithmetic are not available when
; running this file

(#includePath
    "{$cfgdir}/modules/"
)

(#gitCatalog
    ; TODO, Default catalog should be moved to trueagi github account
    (#name "luketpeterson-catalog")
    (#url "https://github.com/luketpeterson/metta-mod-catalog.git")
    (#refreshTime 259200) ; 3 days = 3 days * 24 hrs * 60 minutes * 60 seconds
)