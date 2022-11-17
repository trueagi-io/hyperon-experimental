use hyperon::metta::*;
use hyperon::metta::interpreter::*;

fn main() {
    let space = metta_space("
        (: List (-> $a Type))
        (: Nil (List $a))
        (: Cons (-> $a (List $a) (List $a)))

        (: if (-> bool Any Any) Any)
        (= (if true $then $else) $then)
        (= (if false $then $else) $else)

        (= (insert $x Nil) (Cons $x Nil))
        (= (insert $x (Cons $head $tail)) (if (< $x $head)
                                              (Cons $x (Cons $head $tail))
                                              (Cons $head (insert $x $tail))))
    ");


    assert_eq!(interpret(&space, &metta_atom("(insert 1 Nil)")),
        Ok(vec![metta_atom("(Cons 1 Nil)")]));
    assert_eq!(interpret(&space, &metta_atom("(insert 3 (insert 2 (insert 1 Nil)))")),
        Ok(vec![metta_atom("(Cons 1 (Cons 2 (Cons 3 Nil)))")]));
}
