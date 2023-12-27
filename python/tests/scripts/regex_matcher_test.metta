(= (intent regex:"^Hello[[\.|!]]?$") (Intent Hello))
(= (intent regex:"Good~morning.*[[\\.|!]]?") (Intent Hello))
(= (intent $x) (empty))
!(assertEqual
   (intent "hello")
   (Intent Hello))
!(assertEqual
   (intent "hi")
   (empty))

!(assertEqual
   (intent "Good morning my friend!")
   (Intent Hello))


