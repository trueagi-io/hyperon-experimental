!(import! &self bhv_binding)

!(bind! &name (bhv-new))
!(bind! &capital_city (bhv-new))
!(bind! &money (bhv-new))
!(bind! &united_states (bhv-new))
!(bind! &washington_dc (bhv-new))
!(bind! &dollar (bhv-new))
!(bind! &mexico (bhv-new))
!(bind! &mexico_city (bhv-new))
!(bind! &peso (bhv-new))


!(bind! USA (bhv-majority (bhv-bind &name &united_states) (bhv-bind &capital_city &washington_dc) (bhv-bind &money &dollar) ))
!(bind! MEX (bhv-majority (bhv-bind &name &mexico) (bhv-bind &capital_city &mexico_city) (bhv-bind &money &peso) ))

!(bind! Pair (bhv-bind USA MEX))

; should return True here
!(bhv-is-related (bhv-bind USA &money) &dollar)


!(bind! dollar_of_mexico (bhv-bind &dollar Pair))

; should return True here. "Dollar of mexico" is peso
!(bhv-is-related dollar_of_mexico &peso)






