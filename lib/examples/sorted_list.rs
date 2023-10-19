use hyperon::metta::runner::*;

fn main() -> Result<(), String> {
    let metta = Metta::new(None);
    metta.run_program_str("
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
    ")?;

    assert_eq!(metta.run_program_str("!(insert 1 Nil)")?[0],
        vec![metta.parse_one_atom("(Cons 1 Nil)")?]);
    assert_eq!(metta.run_program_str("(insert 3 (insert 2 (insert 1 Nil)))")?[0],
        vec![metta.parse_one_atom("(Cons 1 (Cons 2 (Cons 3 Nil)))")?]);

    Ok(())
}
