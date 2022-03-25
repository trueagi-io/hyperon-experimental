use crate::*;
use crate::atom::matcher::Bindings;
use crate::space::grounding::GroundingSpace;

#[derive(Debug, PartialEq, Eq)]
pub enum AtomType {
    Undefined,
    Specific(Atom),
}

fn typeof_query(atom: &Atom, typ: &Atom) -> Atom {
    Atom::expr(&[Atom::sym(":"), atom.clone(), typ.clone()])
}

fn isa_query(sub_type: &Atom, super_type: &Atom) -> Atom {
    Atom::expr(&[Atom::sym("<"), sub_type.clone(), super_type.clone()])
}

fn query_has_type(space: &GroundingSpace, sub_type: &Atom, super_type: &Atom) -> Vec<Bindings> {
    space.query(&typeof_query(sub_type, super_type))
}

fn query_super_types(space: &GroundingSpace, sub_type: &Atom) -> Vec<Atom> {
    // TODO: query should check that sub type is a type and not another typed symbol
    let var_x = VariableAtom::from("%X%");
    let mut super_types = space.query(&isa_query(&sub_type, &Atom::Variable(var_x.clone())));
    super_types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect()
}

fn add_super_types(space: &GroundingSpace, sub_types: &mut Vec<Atom>, from: usize) {
    let mut types = Vec::new();
    sub_types.iter().skip(from).for_each(|typ| {
        for typ in query_super_types(space, typ) {
            if !sub_types.contains(&typ) {
                types.push(typ);
            }
        }
    });
    if !types.is_empty() {
        let from = sub_types.len();
        sub_types.append(&mut types.clone());
        add_super_types(space, sub_types, from);
    }
}

fn check_types(actual: &[Vec<Atom>], expected: &[Atom], bindings: &mut Bindings) -> bool {
    log::trace!("check_types: actual: {:?}, expected: {:?}, bindings: {}", actual, expected, bindings);
    match (actual, expected) {
        ([actual, actual_tail @ ..], [expected, expected_tail @ ..]) => {
            actual.iter().map(|actual| {
                match_reducted_types(actual, expected, bindings)
                    && check_types(actual_tail, expected_tail, bindings)
            }).any(std::convert::identity)
        },
        ([], []) => true,
        _ => false,
    }
}

fn is_func(typ: &&Atom) -> bool {
    match typ {
        Atom::Expression(expr) => {
            expr.children().first() == Some(&Atom::sym("->"))
        },
        _ => false,
    }
}

fn query_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::from("%X%");
    let mut types = query_has_type(space, atom, &Atom::Variable(var_x.clone()));
    let mut types = types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect();
    add_super_types(space, &mut types, 0);
    types
}

fn get_arg_types<'a>(fn_typ: &'a Atom) -> (&'a [Atom], &'a Atom) {
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

fn get_reducted_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    log::trace!("get_reducted_types: atom: {}", atom);
    let types = match atom {
        Atom::Variable(_) | Atom::Grounded(_) => vec![Atom::sym("%Undefined%")],
        Atom::Symbol(_) => {
            let types = query_types(space, atom);
            if !types.is_empty() {
                types
            } else {
                vec![Atom::sym("%Undefined%")]
            }
        },
        Atom::Expression(expr) => {
            // tuple type
            let mut types = vec![Atom::expr(&[])];
            for (i, child) in expr.children().iter().enumerate() {
                let child_types = get_reducted_types(space, child);
                let child_types = child_types.iter()
                    .filter(|typ| i != 0 || !is_func(typ));
                types = types.drain(0..).flat_map(|prev| -> Vec<Atom> {
                    match prev {
                        Atom::Expression(expr) => {
                            child_types.clone().map(|typ| {
                                let mut children = expr.children().clone();
                                children.push(typ.clone());
                                Atom::expr(children.as_slice())
                            }).collect()
                        },
                        _ => vec![prev],
                    }
                }).collect();
            }
            types.append(&mut query_types(space, atom));
            add_super_types(space, &mut types, 0);

            // functional types
            let op = get_op(expr);
            let args = get_args(expr);
            let actual_arg_types: Vec<Vec<Atom>> = args.iter().map(|arg| get_reducted_types(space, arg)).collect();
            let fn_types = get_reducted_types(space, op);
            let fn_types = fn_types.iter().filter(is_func);
            for fn_type in fn_types {
                let (expected_arg_types, ret) = get_arg_types(fn_type);
                if check_types(actual_arg_types.as_slice(), expected_arg_types, &mut Bindings::new()) {
                    types.push(ret.clone());
                }
            }

            types
        },
    };
    log::trace!("get_reducted_types: atom: {} return {:?}", atom, types);
    types
}

pub fn match_reducted_types(type1: &Atom, type2: &Atom, bindings: &mut Bindings) -> bool {
    log::trace!("match_reducted_types: type1: {}, type2: {}, bindings: {}", type1, type2, bindings);
    let result = match (type1, type2) {
        (Atom::Variable(_), Atom::Variable(_)) => false,
        (Atom::Grounded(_), _) | (_, Atom::Grounded(_)) => false,
        (Atom::Symbol(sym1), Atom::Symbol(sym2)) => {
            type1 == type2 || sym1.name() == "%Undefined%" || sym2.name() == "%Undefined%"
        },
        (Atom::Variable(var), typ) | (typ, Atom::Variable(var)) => {
            bindings.check_and_insert_binding(var, typ)
        },
        (Atom::Expression(expr1), Atom::Expression(expr2)) => {
            std::iter::zip(expr1.children().iter(), expr2.children().iter())
                .map(|(child1, child2)| match_reducted_types(child1, child2, bindings))
                .reduce(|a, b| a && b)
                .unwrap_or(true)
        },
        (Atom::Expression(_), Atom::Symbol(sym))
            | (Atom::Symbol(sym), Atom::Expression(_))
            if sym.name() == "%Undefined%" => true,
        _ => false,
    };
    log::trace!("match_reducted_types: type1: {}, type2: {}, bindings: {} return {}", type1, type2, bindings, result);
    result
}

fn get_matched_types(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> Vec<(Atom, Bindings)> {
    let mut types = get_reducted_types(space, atom);
    types.drain(0..).filter_map(|t| {
        let mut bindings = Bindings::new();
        if match_reducted_types(&t, typ, &mut bindings) {
            Some((t, bindings))
        } else {
            None
        }
    }).collect()
}

pub fn check_type(space: &GroundingSpace, atom: &Atom, typ: &AtomType) -> bool {
    let undefined = Atom::sym("%Undefined%");
    let typ = match typ {
        AtomType::Undefined => &undefined,
        AtomType::Specific(atom) => atom,
    };
    !get_matched_types(space, atom, typ).is_empty()
}

pub fn validate_atom(space: &GroundingSpace, atom: &Atom) -> bool {
    !get_reducted_types(space, atom).is_empty()
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
        space.add(expr!("<", "Quest", "Sent"));
        space.add(expr!("<", ("Aux", "Subj", "Verb", "Obj"), "Quest"));
        space.add(expr!("<", "Pron", "Subj"));
        space.add(expr!("<", "NG", "Subj"));
        space.add(expr!("<", "Pron", "Obj"));
        space.add(expr!("<", "NG", "Obj"));
        space.add(expr!("<", ("Det", "Noun"), "NG"));
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
    fn test_check_expr_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "i", "Pron"));
        space.add(expr!(":", "like", "Verb"));
        space.add(expr!(":", "music", "Noun"));
        space.add(expr!(":", ("do", "you", "like", "music"), "Quest"));
        space.add(expr!("<", ("Pron", "Verb", "Noun"), "Statement"));

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
            (< A B)
            (< B C)
            (< C D)
        ");

        assert!(check_type(&space, &atom("a"), &AtomType::Specific(atom("D"))));
    }

    #[test]
    fn nested_loop_type() {
        init_logger();
        let space = metta_space("
            (< B A)
            (: a A)
            (< A B)
            (< B C)
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
            (: c C)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
        assert!(!validate_atom(&space, &atom("(a c)")));
    }

    #[test]
    fn validate_basic_expr() {
        init_logger();
        let space = GroundingSpace::new();
        assert!(validate_atom(&space, &expr!({5})));
        assert!(validate_atom(&space, &expr!("+", {3}, {5})));
        assert!(validate_atom(&space, &expr!("=", ("f", x), x)));
    }

    #[test]
    fn simple_dep_types() {
        init_logger();
        let space = metta_space("
            (: = (-> $t $t Prop))
            (: Entity Prop)
            (: Human (-> Entity Prop))
            (: Socrates Entity)
            (: (Human Socrates) Prop)
            (: Plato Entity)
            (: Mortal (-> Entity Prop))
            (: HumansAreMortal (-> (Human $t) (Mortal $t)))
            (: Time NotEntity)
            (: SocratesIsHuman (Human Socrates))
        ");
        let t = &AtomType::Specific(atom("Prop"));
        assert!(check_type(&space, &atom("(Human Socrates)"), t));
        assert!(check_type(&space, &atom("(Human Plato)"), t));
        assert!(!check_type(&space, &atom("(Human Time)"), t));
        assert!(!validate_atom(&space, &atom("(Human Time)")));
        assert!(!check_type(&space, &atom("(Human Time)"), &AtomType::Specific(atom("((-> Entity Prop) NotEntity)"))));
        assert!(check_type(&space, &atom("(= Socrates Socrates)"), t));
        assert!(check_type(&space, &atom("(= Socrates Plato)"), t));
        // TODO: should we type check this as (= Any Any) or (= Atom Atom)?
        assert!(!check_type(&space, &atom("(= Socrates Untyped)"), t));
        assert!(!check_type(&space, &atom("(= Socrates Time)"), t));

        assert!(validate_atom(&space, &atom("(HumansAreMortal SocratesIsHuman)")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Socrates))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Plato))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal (Human Time))")));
        assert!(!validate_atom(&space, &atom("(HumansAreMortal Human)")));
        assert!(!check_type(&space, &atom("(HumansAreMortal (Human Socrates))"),
                           &AtomType::Specific(atom("(Mortal Socrates)"))));
        assert!(check_type(&space, &atom("(HumansAreMortal SocratesIsHuman)"),
                           &AtomType::Specific(atom("(Mortal Socrates)"))));

        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Socrates))")));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Plato))")));
        assert!(!check_type(&space, &atom("(= SocratesIsHuman (Human Socrates))"), t));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Time))")));
    }

    #[test]
    fn dep_types_prop() {
        init_logger();
        let space = metta_space("
            (: Sam Entity)
            (: Frog (-> Entity Prop))
            (: Green (-> Entity Prop))
            (: Croaks (-> Entity Prop))
            (: GreenAndCroaksIsFrog (-> (Green $t) (Croaks $t) (Frog $t)))
            (: SamIsGreen (Green Sam))
            (: SamCroaks (Croaks Sam))
        ");
        assert!(validate_atom(&space, &atom("(GreenAndCroaksIsFrog SamIsGreen SamCroaks)")));
        assert!(check_type(&space, &atom("(GreenAndCroaksIsFrog SamIsGreen SamCroaks)"),
                           &AtomType::Specific(atom("(Frog Sam)"))));
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

    #[test]
    fn validate_non_functional_expression() {
        init_logger();
        let space = metta_space("
            (: a A)
            (: b B)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn check_type_non_functional_expression() {
        init_logger();
        let space = metta_space("
            (: a (-> C D))
            (: a A)
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &AtomType::Specific(atom("(A B)"))));
    }
}
