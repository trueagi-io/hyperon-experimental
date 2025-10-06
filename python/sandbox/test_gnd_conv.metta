!(import! &self random)

; should work as passing Rust Bool to Rust function
!(xor (flip) (flip))

; should work via automatic Rust->Python conversion
!(and (flip) (flip))

; should work as well
!(and (flip) True)

; Works as well after https://github.com/trueagi-io/hyperon-experimental/pull/597
!(xor True (flip))
