use hyperon_atom::*;
use hyperon::metta::runner::*;
use hyperon_atom::gnd::number::Number;
use hyperon::metta::text::SExprParser;

fn main() -> Result<(), String> {
    let metta = Metta::new(None);
    metta.run(SExprParser::new("
        (: List (-> $a Type))
        (: Nil (List $a))
        (: Cons (-> $a (List $a) (List $a)))

        (: insert (-> $a (List $a) (List $a)))
        (= (insert $x Nil) (Cons $x Nil))
        (= (insert $x (Cons $head $tail)) (if (< $x $head)
                                              (Cons $x (Cons $head $tail))
                                              (Cons $head (insert $x $tail))))
    "))?;

    assert_eq!(metta.run(SExprParser::new("!(insert 1 Nil)"))?[0],
        vec![expr!("Cons" {Number::Integer(1)} "Nil")]);
    assert_eq!(metta.run(SExprParser::new("!(insert 3 (insert 2 (insert 1 Nil)))"))?[0],
        vec![expr!("Cons" {Number::Integer(1)} ("Cons" {Number::Integer(2)} ("Cons" {Number::Integer(3)} "Nil")))]);

    Ok(())
}
