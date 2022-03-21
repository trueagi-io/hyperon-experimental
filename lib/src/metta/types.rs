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

fn query_super_type(space: &GroundingSpace, sub: &Atom, sup: &Atom) -> Vec<Bindings> {
    space.query(&type_query(sub, sup))
}

fn query_sub_type(space: &GroundingSpace, typ: &Atom) -> Vec<Atom> {
    // TODO: query should check that sub type is a type and not another typed symbol
    let var_x = VariableAtom::from("X");
    let mut types = space.query(&type_query(&Atom::Variable(var_x.clone()), &typ));
    types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect()
}

fn match_type_slice(space: &GroundingSpace, atoms: &[Atom], types: &[Atom]) -> bool {
    match (atoms, types) {
        ([atom, atoms @ ..], [typ, types @ ..]) =>
            check_specific_type(space, atom, typ)
                && match_type_slice(space, atoms, types),
        ([], []) => true,
        _ => false,
    }
}

fn check_sub_types(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> bool {
    query_sub_type(space, typ).iter()
        .map(|typ| check_specific_type(space, atom, &typ))
        .any(std::convert::identity)
}

fn check_specific_type(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> bool {
    log::debug!("check_specific_type: atom: {:?}, typ: {:?}", atom, typ);
    let result = match (atom, typ) {
        (_, Atom::Symbol(_)) | (Atom::Symbol(_), _) =>
            query_super_type(space, atom, typ).is_empty().not()
                || check_sub_types(space, atom, typ),
        (Atom::Expression(expr), Atom::Expression(typ_expr)) =>
            match_type_slice(space, expr.children().as_slice(), typ_expr.children().as_slice()),
        _ => false,
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

fn get_arg_types(fn_typ: &Atom) -> Vec<AtomType> {
    let fn_typ = fn_typ.clone();
    match fn_typ {
        Atom::Expression(mut expr) => {
            let len = expr.children().len();
            assert!(len > 0);
            let mut children = expr.children_mut()
                .drain(0..(len - 1));
            assert_eq!(children.next().unwrap(), Atom::sym("->"));
            children.map(|typ| AtomType::Specific(typ)).collect()
        },
        _ => panic!("Non function type")
    }
}

fn get_op(expr: &ExpressionAtom) -> &Atom {
    expr.children().get(0).expect("Non-empty expression is expected")
}

fn get_args(expr: &ExpressionAtom) -> &[Atom] {
    &expr.children().as_slice()[1..]
}

pub fn validate_expr(space: &GroundingSpace, atom: &Atom) -> bool {
    match atom {
        Atom::Expression(expr) => {
            let op = get_op(expr);
            let args = get_args(expr);
            get_types(space, op).iter().map(|fn_typ| {
                let arg_types = get_arg_types(fn_typ);
                log::trace!("arg_types: {:?}", arg_types);
                if args.len() == arg_types.len() {
                    std::iter::zip(args, arg_types).map(|(arg, typ)| {
                        get_types(space, arg).len() == 0 || check_type(space, arg, &typ)
                    }).all(std::convert::identity)
                } else {
                    false
                }
            }).any(std::convert::identity)
        },
        _ => panic!("Atom::Expression is expected as an argument"),
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
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "do", "Verb"));
        space.add(expr!(":", "do", "Aux"));
        space.add(expr!(":", var, "Verb"));

        let aux = AtomType::Specific(Atom::sym("Aux"));
        let verb = AtomType::Specific(Atom::sym("Verb"));

        let nonsense = Atom::sym("nonsense");
        assert_eq!(check_type(&space, &nonsense, &AtomType::Undefined), true);
        assert_eq!(check_type(&space, &nonsense, &aux), false);

        let _do = Atom::sym("do");
        assert_eq!(check_type(&space, &_do, &AtomType::Undefined), true);
        assert_eq!(check_type(&space, &_do, &aux), true);
        assert_eq!(check_type(&space, &_do, &verb), true);
        assert_eq!(check_type(&space, &_do, &AtomType::Specific(Atom::sym("Noun"))), false);

        let var = Atom::var("var");
        assert_eq!(check_type(&space, &var, &AtomType::Undefined), true);
        assert_eq!(check_type(&space, &var, &aux), true);
        assert_eq!(check_type(&space, &var, &verb), true);
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
        assert_eq!(check_type(&space, &i_like_music, &AtomType::Undefined), true);
        assert_eq!(check_type(&space, &i_like_music, &AtomType::Specific(expr!("Pron", "Verb", "Noun"))), true);
        assert_eq!(check_type(&space, &i_like_music, &AtomType::Specific(Atom::sym("Statement"))), true);

        assert_eq!(check_type(&space, &expr!("do", "you", "like", "music"), &AtomType::Specific(Atom::sym("Quest"))), true);
    }

    #[test]
    fn test_validate_expr() {
        init_logger();
        let space = grammar_space();
        let expr = expr!("answer", ("do", "you", "like", ("a", "pizza")));

        assert_eq!(validate_expr(&space, &expr), true);
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

        assert!(validate_expr(&space, &atom("(a b)")));
    }

    #[test]
    fn arrow_allows_undefined_type() {
        init_logger();
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(validate_expr(&space, &atom("(a b)")));
    }

    #[ignore]
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

        assert!(validate_expr(&space, &atom("(h a)")));
    }
}
