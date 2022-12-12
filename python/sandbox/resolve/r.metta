!(extend-py! resolve)
!(extend-py! r)

! r ; hyperon.runner.MeTTa object (`r` is registered 'manually' now)
! resolve ; just a symbol, because `resolve` extension doesn't 'register itself', but it could be possible to automate this

!(r::fact 5) ; 120
