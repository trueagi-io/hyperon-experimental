!(extend-py! neurospace)

!(let $space (new-neural-space)
  (add-atom &self (= (my-nspace) $space)))

!(add-atom (my-nspace) (The user name is William))
!(add-atom (my-nspace) (William is 26 years old))
!(add-atom (my-nspace) (William has 2 brothers))
!(add-atom (my-nspace) (Brothers names are Mike and Nick))
!(add-atom (my-nspace) (Nick is 3 years older than William))
!(add-atom (my-nspace) (Mike is 5 years younger than Nick))


; !(match (my-nspace) (What is the user name $x) $x)
; !(match (my-nspace) (How old $x is William) $x)
; !(match (my-nspace) (How old ($x) is Mike) $x)
; !(match (my-nspace) (What $x is father name) $x)
; !(match (my-nspace) (What $x is Mike age) $x)

; (match (my-nspace) (What are the names $x of William brothers?) $x)

! (match (my-nspace) (What are the names $x of William brothers?)
  (let $query (How old $z is $x ?)
    (match (my-nspace) $query ($x is $z years old))))
