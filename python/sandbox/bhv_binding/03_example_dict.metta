!(import! &self bhv_binding)

!(bind! &v1 (bhv-new))
!(bind! &v2 (bhv-new))
!(bind! &v3 (bhv-new))
!(bind! &k1 (bhv-new))
!(bind! &k2 (bhv-new))
!(bind! &k3 (bhv-new))


!(bind! dict (bhv-majority (bhv-bind &v1 &k1) (bhv-bind &v2 &k2) (bhv-bind &v3 &k3) ))
!(bind! v1_retrived (bhv-bind &k1 dict))

!(bhv-std-apart-relative v1_retrived &v1)
!(bhv-is-related v1_retrived &v1)


