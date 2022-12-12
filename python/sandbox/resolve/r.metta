!(extend-py! resolve)
!(extend-py! r)

! r ; hyperon.runner.MeTTa object (`r` is registered 'manually' now)
! resolve ; just a symbol, because `resolve` extension doesn't 'register itself', but it could be possible to automate this

!(r::fact 5) ; 120

!(r::self-from-self) ; content

!(match &self ($x content) $x) ; []

; this returns `some` meaning that `content` from this scripts
; is not distinguished from `content` symbol in `r`
!(match r::&self ($x content) $x)

! r::content ; it is turned in OperationAtom, but it is not executed
! (r::content) ; its execution results in unreduced `(content)`
; the following doesn't work (it might be ok, but semantics of :: is weird overall)
!(match r::&self ($x r::content) $x)

; both work, so :: doesn't separate "namespaces" precisely
!(r::call_func fact 6)
!(r::call_func r::fact 6)
