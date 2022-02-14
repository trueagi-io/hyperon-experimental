use crate::*;
use std::ops::Not;
use crate::atom::matcher::Bindings;
use crate::space::grounding::GroundingSpace;

#[derive(Debug, PartialEq, Eq)]
enum AtomType {
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
        (_, Atom::Symbol(_)) =>
            query_super_type(space, atom, typ).is_empty().not()
                || check_sub_types(space, atom, typ),
        (Atom::Expression(expr), Atom::Expression(typ_expr)) =>
            match_type_slice(space, expr.children().as_slice(), typ_expr.children().as_slice()),
        _ => false,
    };
    log::debug!("check_specific_type: result: {}", result);
    result
}

fn check_type(space: &GroundingSpace, atom: &Atom, typ: &AtomType) -> bool {
    match typ {
        AtomType::Undefined => true,
        AtomType::Specific(typ) => check_specific_type(space, atom, typ),
    }
}

fn first_arg_typ(fn_typ: &Atom) -> (AtomType, Option<&Atom>) {
    match fn_typ {
        Atom::Expression(expr) => {
            let children = expr.children();
            assert_eq!(children[0], Atom::sym("->"));
            let typ = children[1].clone();
            let fn_typ = if children.len() == 3 { Option::from(&children[2]) } else { None };
            (AtomType::Specific(typ), fn_typ)
        },
        _ => panic!("Non function type")
    }
}

fn check_arg_types(space: &GroundingSpace, args: &[Atom], fn_typ: &Atom) -> bool {
    log::debug!("check_args_type:, args: {:?}, fn_typ: {:?}", args, fn_typ);
    let is_expr = matches!(fn_typ, Atom::Expression(_));
    let args_empty = args.is_empty();
    if is_expr && !args_empty {
        let (typ, fn_typ) = first_arg_typ(fn_typ);
        check_type(space, &args[0], &typ) && check_arg_types(space, &args[1..], fn_typ.unwrap_or(&Atom::expr(&[])))
    } else {
        !is_expr && args_empty
    }
}

fn get_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::from("X");
    let mut types = query_super_type(space, atom, &Atom::Variable(var_x.clone()));
    types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect()
}

fn get_op(expr: &ExpressionAtom) -> &Atom {
    expr.children().get(0).expect("Non-empty expression is expected")
}

fn validate_expr(space: &GroundingSpace, atom: &Atom) -> bool {
    match atom {
        Atom::Expression(expr) => {
            let op = get_op(expr);
            let args = &expr.children().as_slice()[1..];
            get_types(space, op).iter().map(|typ| check_arg_types(space, args, typ))
                .any(std::convert::identity)
        },
        _ => panic!("Atom::Expression is expected as an argument"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
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
}
