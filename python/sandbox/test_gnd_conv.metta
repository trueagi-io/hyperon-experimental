; should work as passing Rust Bool to Rust function
!(xor (flip) (flip))

; should work via automatic Rust->Python conversion
!(and (flip) (flip))

; should work as well
!(and (flip) True)

; Should not work atm because of no backward conversion of Python Bool to Rust
!(xor True (flip))
