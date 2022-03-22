use crate::*;
use std::ops::Not;
use crate::atom::matcher::Bindings;
use crate::space::grounding::GroundingSpace;

#[derive(Debug, PartialEq, Eq)]
pub enum AtomType {
    Undefined,
    Specific(Atom),
}

fn type_query(atom: &Atom, typ: &Atom) -> Atom {
    Atom::expr(&[Atom::sym(":"), atom.clone(), typ.clone()])
}

fn query_super_type(space: &GroundingSpace, sub_type: &Atom, super_type: &Atom) -> Vec<Bindings> {
    space.query(&type_query(sub_type, super_type))
}

fn query_sub_type(space: &GroundingSpace, super_type: &Atom) -> Vec<Atom> {
    // TODO: query should check that sub type is a type and not another typed symbol
    let var_x = VariableAtom::from("%X%");
    let mut sub_types = space.query(&type_query(&Atom::Variable(var_x.clone()), &super_type));
    sub_types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect()
}

fn check_sub_types(space: &GroundingSpace, atom: &Atom, super_type: &Atom) -> bool {
    query_sub_type(space, super_type).iter()
        .map(|typ| check_specific_type(space, atom, &typ))
        .any(std::convert::identity)
}

fn check_types(space: &GroundingSpace, atoms: &[Atom], types: &[Atom]) -> bool {
    atoms.len() == types.len() &&
        std::iter::zip(atoms, types)
            .map(|(atom, typ)| check_specific_type(space, atom, typ))
            .all(std::convert::identity)
}

fn is_func(typ: &Atom) -> bool {
    match typ {
        Atom::Expression(expr) => {
            expr.children().first() == Some(&Atom::sym("->"))
        },
        _ => false,
    }
}

fn check_specific_type(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> bool {
    log::debug!("check_specific_type: atom: {:?}, typ: {:?}", atom, typ);
    let result = match (atom, typ) {
        // expression type case
        (Atom::Expression(expr), Atom::Expression(typ_expr)) =>
            check_types(space, expr.children().as_slice(), typ_expr.children().as_slice()),
        // expression atom case
        (Atom::Expression(expr), typ) => {
            let op = get_op(expr);
            let args = get_args(expr);
            get_types(space, op).iter()
                .map(|fn_typ| {
                    is_func(fn_typ) && {
                        let (arg_types, ret) = get_arg_types(fn_typ);
                        //log::trace!("check_specific_type: arg_types: {:?}, ret: {:?}", arg_types, ret);
                        check_types(space, args, arg_types) && (
                            typ == ret
                            || query_super_type(space, ret, typ).is_empty().not()
                            || check_sub_types(space, ret, typ)
                    )}
                }).any(std::convert::identity)
            || query_super_type(space, atom, typ).is_empty().not()
            || check_sub_types(space, atom, typ)
        },
        // single atom case
        (atom, typ) => {
            let types = get_types(space, atom);
            types.is_empty()
                || types.contains(typ)
                || check_sub_types(space, atom, typ)
        },
    };
    log::debug!("check_specific_type: result: {}", result);
    result
}

pub fn check_type(space: &GroundingSpace, atom: &Atom, typ: &AtomType) -> bool {
    match typ {
        AtomType::Undefined => true,
        AtomType::Specific(typ) => check_specific_type(space, atom, typ),
    }
}

fn get_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::from("X");
    let mut types = query_super_type(space, atom, &Atom::Variable(var_x.clone()));
    types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect()
}

fn get_arg_types<'a>(fn_typ: &'a Atom) -> (&'a [Atom], &'a Atom) {
    //log::trace!("get_arg_types: {}", fn_typ);
    match fn_typ {
        Atom::Expression(expr) => {
            let children = expr.children().as_slice();
            match children {
                [op,  args @ .., res] if *op == Atom::sym("->") => (args, res),
                _ => panic!("Incorrect function type: {}", fn_typ)
            }
        },
        _ => panic!("Incorrect function type: {}", fn_typ)
    }
}

fn get_op(expr: &ExpressionAtom) -> &Atom {
    expr.children().get(0).expect("Non-empty expression is expected")
}

fn get_args(expr: &ExpressionAtom) -> &[Atom] {
    &expr.children().as_slice()[1..]
}

pub fn validate_atom(space: &GroundingSpace, atom: &Atom) -> bool {
    log::debug!("validate_atom: atom: {}", atom);
    match atom {
        Atom::Expression(expr) => {
            let op = get_op(expr);
            let args = get_args(expr);
            get_types(space, op).iter()
                .map(|fn_typ| {
                    //log::trace!("try type: {}", fn_typ);
                    let (arg_types, _) = get_arg_types(fn_typ);
                    check_types(space, args, arg_types)
                })
                .any(std::convert::identity)
        },
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::metta_space;
    use crate::metta::metta_atom as atom;
    
    fn init_logger() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    fn grammar_space() -> GroundingSpace {
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "answer", ("->", "Sent", "Sent")));
        space.add(expr!(":", "Quest", "Sent"));
        space.add(expr!(":", ("Aux", "Subj", "Verb", "Obj"), "Quest"));
        space.add(expr!(":", "Pron", "Subj"));
        space.add(expr!(":", "NG", "Subj"));
        space.add(expr!(":", "Pron", "Obj"));
        space.add(expr!(":", "NG", "Obj"));
        space.add(expr!(":", ("Det", "Noun"), "NG"));
        space.add(expr!(":", "you", "Pron"));
        space.add(expr!(":", "do", "Aux"));
        space.add(expr!(":", "do", "Verb"));
        space.add(expr!(":", "like", "Verb"));
        space.add(expr!(":", "a", "Det"));
        space.add(expr!(":", "pizza", "Noun"));
        space
    }

    #[test]
    fn test_check_type() {
        init_logger();
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "do", "Verb"));
        space.add(expr!(":", "do", "Aux"));

        let aux = AtomType::Specific(Atom::sym("Aux"));
        let verb = AtomType::Specific(Atom::sym("Verb"));

        let nonsense = Atom::sym("nonsense");
        assert!(check_type(&space, &nonsense, &AtomType::Undefined));
        assert!(check_type(&space, &nonsense, &aux));

        let _do = Atom::sym("do");
        assert!(check_type(&space, &_do, &AtomType::Undefined));
        assert!(check_type(&space, &_do, &aux));
        assert!(check_type(&space, &_do, &verb));
        assert!(!check_type(&space, &_do, &AtomType::Specific(Atom::sym("Noun"))));

    }

    #[test]
    fn check_var_type() {
        init_logger();
        let space = metta_space("
            (: $x A)
            (: A B)
        ");
        
        let x = Atom::var("x");
        assert!(check_type(&space, &x, &AtomType::Undefined));
        assert!(check_type(&space, &x, &AtomType::Specific(Atom::sym("A"))));
        assert!(check_type(&space, &x, &AtomType::Specific(Atom::sym("B"))));
    }

    #[ignore]
    #[test]
    fn var_type_does_not_match_other_atoms() {
        init_logger();
        let space = metta_space("
            (: $x A)
        ");
        
        assert!(!check_type(&space, &Atom::sym("a"), &AtomType::Specific(Atom::sym("A"))));
    }

    #[test]
    fn test_check_expr_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "i", "Pron"));
        space.add(expr!(":", "like", "Verb"));
        space.add(expr!(":", "music", "Noun"));
        space.add(expr!(":", ("do", "you", "like", "music"), "Quest"));
        space.add(expr!(":", ("Pron", "Verb", "Noun"), "Statement"));

        let i_like_music = expr!("i", "like", "music");
        assert!(check_type(&space, &i_like_music, &AtomType::Undefined));
        assert!(check_type(&space, &i_like_music, &AtomType::Specific(expr!("Pron", "Verb", "Noun"))));
        assert!(check_type(&space, &i_like_music, &AtomType::Specific(Atom::sym("Statement"))));

        assert!(check_type(&space, &expr!("do", "you", "like", "music"), &AtomType::Specific(Atom::sym("Quest"))));
    }

    #[test]
    fn nested_type() {
        init_logger();
        let space = metta_space("
            (: a A)
            (: A B)
            (: B C)
            (: C D)
        ");

        assert!(check_type(&space, &atom("a"), &AtomType::Specific(atom("D"))));
    }

    #[test]
    fn nested_loop_type() {
        init_logger();
        let space = metta_space("
            (: B A)
            (: a A)
            (: A B)
            (: B C)
        ");

        assert!(check_type(&space, &atom("a"), &AtomType::Specific(atom("C"))));
    }

    #[test]
    fn test_validate_atom() {
        init_logger();
        let space = grammar_space();
        let expr = expr!("answer", ("do", "you", "like", ("a", "pizza")));

        assert!(validate_atom(&space, &expr));
    }

    #[test]
    fn validate_symbol() {
        let space = GroundingSpace::new();
        assert!(validate_atom(&space, &Atom::sym("a")));
    }

    #[test]
    fn simple_types() {
        init_logger();
        let space = metta_space("
            (: blue Color)
            (: balloon Object)
        ");

        assert!(check_type(&space, &atom("(blue balloon)"),
            &AtomType::Specific(atom("(Color Object)"))));
    }

    #[test]
    fn arrow_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(check_type(&space, &atom("a"), &AtomType::Specific(atom("(-> B A)"))));
    }

    #[test]
    fn arrow_allows_specific_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
            (: b B)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn arrow_allows_undefined_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn arrow_has_type_of_returned_value() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &AtomType::Specific(atom("A"))));
    }

    #[test]
    fn nested_arrow_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
            (: h (-> (-> B A) C))
        ");

        assert!(validate_atom(&space, &atom("(h a)")));
    }

    #[test]
    fn nested_return_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
            (: b B)
            (: h (-> A C))
        ");

        assert!(validate_atom(&space, &atom("(h (a b))")));
    }
}
