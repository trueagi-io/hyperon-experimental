!(import! &self bhv_binding)

!(bind! &a (bhv-new))
!(bind! &b (bhv-new))
!(bind! &c (bhv-new))

!(bind! &abc (bhv-majority &a &b &c ))

!(bhv-std-apart-relative &a &abc)
!(bhv-is-related &a &abc)


