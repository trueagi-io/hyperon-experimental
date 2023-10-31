use hyperon::metta::{runner::*, text::SExprParser};

fn main() -> Result<(), String> {
    let metta = Metta::new(None);
    metta.run(SExprParser::new("
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
    "))?;

    assert_eq!(metta.run(SExprParser::new("!(insert 1 Nil)"))?[0],
        vec![SExprParser::new("(Cons 1 Nil)").parse(&*metta.tokenizer().borrow())?.unwrap()]);
    assert_eq!(metta.run(SExprParser::new("(insert 3 (insert 2 (insert 1 Nil)))"))?[0],
        vec![SExprParser::new("(Cons 1 (Cons 2 (Cons 3 Nil)))").parse(&*metta.tokenizer().borrow())?.unwrap()]);

    Ok(())
}
