(@doc get-value
  (@desc "Function takes space and key as input, checks if space contains key-value pairs in form of (key value) and returns value tied to the input key")
  (@params (
    (@param "Space")
    (@param "Key")))
  (@return "Value which tied to input key, empty if no such key in space"))
(: get-value (-> Grounded Atom %Undefined%))
(= (get-value $dictspace $key) (match $dictspace ($key $value) $value))

(@doc get-keys
  (@desc "Function takes space and returns all keys from (<key> <value>) tuples in space")
  (@params (
    (@param "Space")))
  (@return "All keys in the input space"))
(: get-keys (-> Grounded Expression))
(= (get-keys $dictspace)
   (function
     (chain (unify $dictspace ($key $value) $key Empty) $t (return $t)) ))

(@doc dict-space
  (@desc "Function takes key-value pairs in form of expression as input, creates space and adds key-value pairs into it")
  (@params (
    (@param "Expression")))
  (@return "Space"))
(: dict-space (-> Expression Grounded))
(= (dict-space $expr) (chain (eval (new-space)) $dictspace (let $res (add-reducts $dictspace $expr) $dictspace)))

(@doc json-encode
  (@desc "Function takes atom as an input and encodes it to json-string. Atom could be a string, number, expression, space and combination of those")
  (@params (
    (@param "Input atom")))
  (@return "Json string"))

(@doc json-decode
  (@desc "Function takes json string as an input and decodes it to the metta objects (list to expression, dictionary to space which will contain key-value pairs in form of (key value), string to string, number to number)")
  (@params (
    (@param "Json string")))
  (@return "Metta object"))