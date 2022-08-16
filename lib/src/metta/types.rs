use super::*;
use crate::atom::matcher::{Bindings, apply_bindings_to_atom};
use crate::space::grounding::GroundingSpace;

fn typeof_query(atom: &Atom, typ: &Atom) -> Atom {
    Atom::expr(vec![HAS_TYPE_SYMBOL, atom.clone(), typ.clone()])
}

fn isa_query(sub_type: &Atom, super_type: &Atom) -> Atom {
    Atom::expr(vec![SUB_TYPE_SYMBOL, sub_type.clone(), super_type.clone()])
}

fn query_has_type(space: &GroundingSpace, sub_type: &Atom, super_type: &Atom) -> Vec<Bindings> {
    space.query(&typeof_query(sub_type, super_type))
}

fn query_super_types(space: &GroundingSpace, sub_type: &Atom) -> Vec<Atom> {
    // TODO: query should check that sub type is a type and not another typed symbol
    let var_x = VariableAtom::new("%X%");
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

pub fn is_func(typ: &Atom) -> bool {
    match typ {
        Atom::Expression(expr) => {
            expr.children().first() == Some(&ARROW_SYMBOL)
        },
        _ => false,
    }
}

fn query_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    let var_x = VariableAtom::new("%X%");
    let mut types = query_has_type(space, atom, &Atom::Variable(var_x.clone()));
    let mut types = types.drain(0..).map(|mut bindings| bindings.remove(&var_x).unwrap()).collect();
    add_super_types(space, &mut types, 0);
    types
}

pub fn get_arg_types<'a>(fn_typ: &'a Atom) -> (&'a [Atom], &'a Atom) {
    match fn_typ {
        Atom::Expression(expr) => {
            let children = expr.children().as_slice();
            match children {
                [op,  args @ .., res] if *op == ARROW_SYMBOL => (args, res),
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

pub fn get_atom_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    let types = get_reducted_types(space, atom);
    log::debug!("get_atom_types: atom: {}, types: {:?}", atom, types);
    types
}

fn get_reducted_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    log::trace!("get_reducted_types: atom: {}", atom);
    let types = match atom {
        Atom::Variable(_) => vec![ATOM_TYPE_UNDEFINED],
        Atom::Grounded(gnd) => {
            let types = vec![gnd.type_()];
            types
        },
        Atom::Symbol(_) => {
            let mut types = query_types(space, atom);
            if types.is_empty() {
                types.push(ATOM_TYPE_UNDEFINED)
            }
            types
        },
        Atom::Expression(expr) => {
            // tuple type
            let mut tuples = vec![vec![]];
            for (i, child) in expr.children().iter().enumerate() {
                // TODO: it is not straightforward, if (: a (-> B C)) then
                // what should we return for (d (a b)): (D ((-> B C) B)) or
                // (D C) or both? Same question for a function call.
                let child_types = get_reducted_types(space, child);
                let not_a_function_call = |typ: &&Atom| { i != 0 || !is_func(typ) };
                let child_types = child_types.iter().filter(not_a_function_call);
                tuples = child_types.flat_map(|typ| -> Vec<Vec<Atom>> {
                    tuples.iter().map(|prev| {
                        let mut next = prev.clone();
                        next.push(typ.clone());
                        next
                    }).collect()
                }).collect();
            }
            // if all members of tuple is Undefined then whole tuple is Undefined
            let mut types: Vec<Atom> = tuples.drain(0..)
                .filter(|children| children.iter().any(|child| *child != ATOM_TYPE_UNDEFINED))
                .map(Atom::expr).collect();
            types.append(&mut query_types(space, atom));
            add_super_types(space, &mut types, 0);
            log::trace!("get_reducted_types: tuple {} types {:?}", atom, types);

            // functional types
            let mut only_tuple = true;
            if !expr.children().is_empty() {
                let op = get_op(expr);
                let args = get_args(expr);
                let actual_arg_types: Vec<Vec<Atom>> = args.iter()
                    .map(|arg| {
                        let mut types = get_reducted_types(space, arg);
                        types.push(ATOM_TYPE_ATOM);
                        types.push(get_meta_type(arg));
                        types
                    }).collect();
                let mut fn_types = get_reducted_types(space, op);
                let fn_types = fn_types.drain(0..).filter(is_func);
                for fn_type in fn_types {
                    only_tuple = false;
                    let (expected_arg_types, ret_typ) = get_arg_types(&fn_type);
                    let mut bindings = Bindings::new();
                    if check_types(actual_arg_types.as_slice(), expected_arg_types, &mut bindings) {
                        types.push(apply_bindings_to_atom(&ret_typ, &bindings));
                    }
                }
                log::trace!("get_reducted_types: tuple + function {} types {:?}", atom, types);
            }

            // TODO: Three cases here:
            // - tuple type
            // - function call type with correct arg types
            // - fnction call type with incorrect arg types
            // if (1) we should return [ Undefined ]; if (2) we should
            // return full type of the function if (3) we should return
            // empty (otherwise validate doesn't make sense).
            // This is tricky logic. To simplify it we could physically 
            // separate tuples and calls in separate Atom types. Or use
            // embedded atom to designate function call.
            if only_tuple && types.is_empty() {
                types.push(ATOM_TYPE_UNDEFINED)
            }
            types
        },
    };
    log::trace!("get_reducted_types: return atom {} types {:?}", atom, types);
    types
}


pub fn match_reducted_types(type1: &Atom, type2: &Atom, bindings: &mut Bindings) -> bool {
    fn match_reducted_types_recursive(type1: &Atom, type2: &Atom,
            bindings: &mut Bindings, reverse_bindings: &mut Bindings) -> bool {
        let result = match (type1, type2) {
            (Atom::Variable(f), Atom::Variable(s)) => {
                bindings.check_and_insert_binding(f, type2) &&
                    reverse_bindings.check_and_insert_binding(s, type1)
            },
            (Atom::Grounded(_), Atom::Grounded(_)) => type1 == type2,
            (Atom::Symbol(sym1), Atom::Symbol(sym2)) => {
                type1 == type2 || sym1.name() == "%Undefined%" || sym2.name() == "%Undefined%"
            },
            (Atom::Variable(var), typ) | (typ, Atom::Variable(var)) => {
                bindings.check_and_insert_binding(var, typ)
            },
            (Atom::Grounded(_), _) | (_, Atom::Grounded(_)) => false,
            (Atom::Expression(expr1), Atom::Expression(expr2)) => {
                std::iter::zip(expr1.children().iter(), expr2.children().iter())
                    .map(|(child1, child2)| match_reducted_types_recursive(child1, child2, bindings, reverse_bindings))
                    .reduce(|a, b| a && b)
                    .unwrap_or(true)
            },
            (Atom::Expression(_), Atom::Symbol(sym))
                | (Atom::Symbol(sym), Atom::Expression(_))
                if sym.name() == "%Undefined%" => true,
                    _ => false,
        };
        result
    }
    let result = match_reducted_types_recursive(type1, type2, bindings, &mut Bindings::new());
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

pub fn check_type(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> bool {
    check_meta_type(atom, typ) || !get_matched_types(space, atom, typ).is_empty()
}

pub fn check_type_bindings(space: &GroundingSpace, atom: &Atom, typ: &Atom) -> Vec<(Atom, Bindings)> {
    let mut result = Vec::new();
    if check_meta_type(atom, typ) {
        result.push((typ.clone(), Bindings::new()));
    }
    result.append(&mut get_matched_types(space, atom, typ));
    if result.len() > 1 {
        result = result.drain(0..).filter(|(typ, _)| *typ != ATOM_TYPE_UNDEFINED).collect();
    }
    result
}

fn get_meta_type(atom: &Atom) -> Atom {
    match atom {
        Atom::Symbol(_) => ATOM_TYPE_SYMBOL,
        Atom::Variable(_) => ATOM_TYPE_VARIABLE,
        Atom::Grounded(_) => ATOM_TYPE_GROUNDED,
        Atom::Expression(_) => ATOM_TYPE_EXPRESSION,
    }
}

fn check_meta_type(atom: &Atom, typ: &Atom) -> bool {
    *typ == ATOM_TYPE_ATOM || *typ == get_meta_type(atom)
}

pub fn validate_atom(space: &GroundingSpace, atom: &Atom) -> bool {
    !get_reducted_types(space, atom).is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::metta_space;
    use crate::metta::metta_atom as atom;
    
    fn grammar_space() -> GroundingSpace {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "answer" ("->" "Sent" "Sent")));
        space.add(expr!(":<" "Quest" "Sent"));
        space.add(expr!(":<" ("Aux" "Subj" "Verb" "Obj") "Quest"));
        space.add(expr!(":<" "Pron" "Subj"));
        space.add(expr!(":<" "NG" "Subj"));
        space.add(expr!(":<" "Pron" "Obj"));
        space.add(expr!(":<" "NG" "Obj"));
        space.add(expr!(":<" ("Det" "Noun") "NG"));
        space.add(expr!(":" "you" "Pron"));
        space.add(expr!(":" "do" "Aux"));
        space.add(expr!(":" "do" "Verb"));
        space.add(expr!(":" "like" "Verb"));
        space.add(expr!(":" "a" "Det"));
        space.add(expr!(":" "pizza" "Noun"));
        space
    }

    #[test]
    fn test_check_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "do" "Verb"));
        space.add(expr!(":" "do" "Aux"));

        let aux = sym!("Aux");
        let verb = sym!("Verb");

        let nonsense = sym!("nonsense");
        assert!(check_type(&space, &nonsense, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &nonsense, &aux));

        let _do = sym!("do");
        assert!(check_type(&space, &_do, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &_do, &aux));
        assert!(check_type(&space, &_do, &verb));
        assert!(!check_type(&space, &_do, &sym!("Noun")));

    }

    #[test]
    fn test_check_expr_type() {
        let mut space = GroundingSpace::new();
        space.add(expr!(":" "i" "Pron"));
        space.add(expr!(":" "like" "Verb"));
        space.add(expr!(":" "music" "Noun"));
        space.add(expr!(":" ("do" "you" "like" "music") "Quest"));
        space.add(expr!(":<" ("Pron" "Verb" "Noun") "Statement"));

        let i_like_music = expr!("i" "like" "music");
        assert!(check_type(&space, &i_like_music, &ATOM_TYPE_UNDEFINED));
        assert!(check_type(&space, &i_like_music, &expr!("Pron" "Verb" "Noun")));
        assert!(check_type(&space, &i_like_music, &sym!("Statement")));

        assert!(check_type(&space, &expr!("do" "you" "like" "music"), &sym!("Quest")));
    }

    #[test]
    fn nested_type() {
        let space = metta_space("
            (: a A)
            (:< A B)
            (:< B C)
            (:< C D)
        ");

        assert!(check_type(&space, &atom("a"), &atom("D")));
    }

    #[test]
    fn nested_loop_type() {
        let space = metta_space("
            (:< B A)
            (: a A)
            (:< A B)
            (:< B C)
        ");

        assert!(check_type(&space, &atom("a"), &atom("C")));
    }

    #[test]
    fn test_validate_atom() {
        let space = grammar_space();
        let expr = expr!("answer" ("do" "you" "like" ("a" "pizza")));

        assert!(validate_atom(&space, &expr));
    }

    #[test]
    fn validate_symbol() {
        let space = GroundingSpace::new();
        assert!(validate_atom(&space, &sym!("a")));
    }

    #[test]
    fn simple_types() {
        let space = metta_space("
            (: blue Color)
            (: balloon Object)
        ");

        assert!(check_type(&space, &atom("(blue balloon)"),
            &atom("(Color Object)")));
    }

    #[test]
    fn arrow_type() {
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(check_type(&space, &atom("a"), &atom("(-> B A)")));
    }

    #[test]
    fn arrow_allows_specific_type() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
            (: c C)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
        assert!(check_type(&space, &atom("(a b)"), &ATOM_TYPE_UNDEFINED));
        assert!(!validate_atom(&space, &atom("(a c)")));
        assert!(!check_type(&space, &atom("(a c)"), &ATOM_TYPE_UNDEFINED));
    }

    #[test]
    fn validate_basic_expr() {
        let space = GroundingSpace::new();
        assert!(validate_atom(&space, &expr!({5})));
        assert!(validate_atom(&space, &expr!("+" {3} {5})));
        assert!(validate_atom(&space, &expr!("=" ("f" x) x)));
    }

    #[test]
    fn simple_dep_types() {
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
            (: SocratesIsMortal (Mortal Socrates))
        ");
        let t = &atom("Prop");
        assert!(check_type(&space, &atom("(Human Socrates)"), t));
        assert!(check_type(&space, &atom("(Human Plato)"), t));
        assert!(!check_type(&space, &atom("(Human Time)"), t));
        assert!(!validate_atom(&space, &atom("(Human Time)")));
        assert!(!check_type(&space, &atom("(Human Time)"), &atom("((-> Entity Prop) NotEntity)")));
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
                           &atom("(Mortal Socrates)")));
        assert!(check_type(&space, &atom("(HumansAreMortal SocratesIsHuman)"),
                           &atom("(Mortal Socrates)")));

        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Socrates))")));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Plato))")));
        assert!(!check_type(&space, &atom("(= SocratesIsHuman (Human Socrates))"), t));
        assert!(!validate_atom(&space, &atom("(= SocratesIsHuman (Human Time))")));
        assert!(validate_atom(&space, &atom("(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))")));
        assert!(validate_atom(&space, &atom("(= (Mortal Socrates) (Mortal Plato))")));
    }

    #[test]
    fn dep_types_prop() {
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
                           &atom("(Frog Sam)")));
    }

    #[test]
    fn arrow_allows_undefined_type() {
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn arrow_has_type_of_returned_value() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &atom("A")));
    }

    #[test]
    fn nested_arrow_type() {
        let space = metta_space("
            (: a (-> B A))
            (: h (-> (-> B A) C))
        ");

        assert!(validate_atom(&space, &atom("(h a)")));
    }

    #[test]
    fn nested_return_type() {
        let space = metta_space("
            (: a (-> B A))
            (: b B)
            (: h (-> A C))
        ");

        assert!(validate_atom(&space, &atom("(h (a b))")));
    }

    #[test]
    fn validate_non_functional_expression() {
        let space = metta_space("
            (: a A)
            (: b B)
        ");

        assert!(validate_atom(&space, &atom("(a b)")));
    }

    #[test]
    fn check_type_non_functional_expression() {
        let space = metta_space("
            (: a (-> C D))
            (: a A)
            (: b B)
        ");

        assert!(check_type(&space, &atom("(a b)"), &atom("(A B)")));
    }

    #[test]
    fn get_reducted_types_undefined_expression_type() {
        let space = metta_space("
            (: a A)
        ");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![atom("(A %Undefined%)")]);

        let space = metta_space("");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![ATOM_TYPE_UNDEFINED]);

        let space = metta_space("
            (: a (-> C D))
            (: b B)
        ");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![]);
    }

    #[test]
    fn get_reducted_types_function_call() {
        let space = metta_space("
            (: a (-> B C))
            (: b B)
        ");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![atom("C")]);
    }

    #[test]
    fn get_reducted_types_tuple() {
        let space = metta_space("
            (: a A)
            (: b B)
        ");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![atom("(A B)")]);
    }

    #[test]
    fn get_reducted_types_function_call_and_tuple() {
        let space = metta_space("
            (: a (-> B C))
            (: a A)
            (: b B)
        ");
        assert_eq!(get_reducted_types(&space, &atom("(a b)")), vec![atom("(A B)"), atom("C")]);
    }

    #[test]
    fn get_reducted_types_grounded_atom() {
        let space = GroundingSpace::new();
        assert_eq!(get_reducted_types(&space, &Atom::value(3)), vec![atom("i32")]);
    }

    #[test]
    fn get_reducted_types_empty_expression() {
        let space = GroundingSpace::new();
        assert_eq!(get_reducted_types(&space, &Atom::expr([])), vec![ATOM_TYPE_UNDEFINED]);
    }

    #[test]
    fn get_atom_types_empty_expression_type() {
        let space = metta_space("
            (: a (-> C D))
            (: b B)
        ");
        assert_eq!(get_atom_types(&space, &atom("(a b)")), vec![]);
    }

    #[test]
    fn check_type_parameterized_type_no_variable_bindings() {
        let space = metta_space("
            (: Pair (-> $a $b Type))
            (: A (Pair $c $d))
        ");

        assert!(check_type(&space, &atom("A"), &atom("(Pair $e $f)")));
        assert!(!check_type(&space, &atom("A"), &atom("(Pair $f $f)")));
    }

    #[test]
    fn check_type_dependent_type_symbol_param() {
        let space = metta_space("
            (: === (-> $a $a Type))
            (: Refl (-> $x (=== $x $x)))

            (: TermSym A)
        ");

        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A A)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A B)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== 42 A)")));
        assert!(!check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A 42)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a A)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== A $a)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a $a)")));
        assert!(check_type(&space, &atom("(Refl TermSym)"), &atom("(=== $a $b)")));
    }

    #[test]
    fn check_type_dependent_type_grounded_param() {
        let space = metta_space("
            (: === (-> $a $a Type))
            (: Refl (-> $x (=== $x $x)))

            (: TermGnd 42)
        ");

        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 42)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 24)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 A)")));
        assert!(!check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== A 42)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a 42)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== 42 $a)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a $a)")));
        assert!(check_type(&space, &atom("(Refl TermGnd)"), &atom("(=== $a $b)")));
    }

    #[test]
    fn check_type_accept_meta_type() {
        let type_r = &atom("R");
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: aF (-> A R))
            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(check_type(&space, &atom("(aF a)"), type_r));
        assert!(!check_type(&space, &atom("(aF b)"), type_r));
        // TODO: (aF b) is incorrectly typed, but it is an Atom and check_type
        // returns True
        assert!(check_type(&space, &atom("(aF b)"), &ATOM_TYPE_ATOM));
        assert!(check_type(&space, &atom("(aF b)"), &ATOM_TYPE_EXPRESSION));
        assert!(!check_type(&space, &atom("(aF b)"), &ATOM_TYPE_SYMBOL));

        assert!(check_type(&space, &atom("(atomF a)"), type_r));
        assert!(check_type(&space, &atom("(atomF ())"), type_r));
        assert!(check_type(&space, &atom("(exprF ())"), type_r));
        assert!(!check_type(&space, &atom("(exprF a)"), type_r));
        assert!(check_type(&space, &atom("(gndF 1)"), type_r));
        assert!(!check_type(&space, &atom("(gndF a)"), type_r));
        assert!(check_type(&space, &atom("(symF a)"), type_r));
        assert!(!check_type(&space, &atom("(symF 1)"), type_r));
        assert!(check_type(&space, &atom("(varF $a)"), type_r));
        assert!(!check_type(&space, &atom("(varF a)"), type_r));
    }

    #[test]
    fn check_type_return_meta_type() {
        let type_r = &atom("R");
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: atomR (-> A Atom))
            (: exprR (-> A Expression))
            (: gndR (-> A Grounded))
            (: symR (-> A Symbol))
            (: varR (-> A Variable))

            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(check_type(&space, &atom("(atomF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (exprR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (gndR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (symR a))"), type_r));
        assert!(check_type(&space, &atom("(atomF (varR a))"), type_r));

        assert!(check_type(&space, &atom("(exprF (exprR a))"), type_r));
        // TODO: it is incorrectly typed, but (atomR a) is an Expression and
        // check_type returns True
        assert!(check_type(&space, &atom("(exprF (atomR a))"), type_r));
        
        assert!(check_type(&space, &atom("(gndF (gndR a))"), type_r));
        assert!(!check_type(&space, &atom("(gndF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(symF (symR a))"), type_r));
        assert!(!check_type(&space, &atom("(symF (atomR a))"), type_r));
        assert!(check_type(&space, &atom("(varF (varR a))"), type_r));
        assert!(!check_type(&space, &atom("(varF (atomR a))"), type_r));
    }

    #[test]
    fn validate_atom_accept_meta_type() {
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: aF (-> A R))
            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(validate_atom(&space, &atom("(aF a)")));
        assert!(!validate_atom(&space, &atom("(aF b)")));

        assert!(validate_atom(&space, &atom("(atomF a)")));
        assert!(validate_atom(&space, &atom("(atomF ())")));
        assert!(validate_atom(&space, &atom("(exprF ())")));
        assert!(!validate_atom(&space, &atom("(exprF a)")));
        assert!(validate_atom(&space, &atom("(gndF 1)")));
        assert!(!validate_atom(&space, &atom("(gndF a)")));
        assert!(validate_atom(&space, &atom("(symF a)")));
        assert!(!validate_atom(&space, &atom("(symF 1)")));
        assert!(validate_atom(&space, &atom("(varF $a)")));
        assert!(!validate_atom(&space, &atom("(varF a)")));
    }

    #[test]
    fn validate_atom_return_meta_type() {
        let space = metta_space("
            (: R Type)
            (: A Type)
            (: B Type)
            (: a A)
            (: b B)

            (: atomR (-> A Atom))
            (: exprR (-> A Expression))
            (: gndR (-> A Grounded))
            (: symR (-> A Symbol))
            (: varR (-> A Variable))

            (: atomF (-> Atom R))
            (: exprF (-> Expression R))
            (: gndF (-> Grounded R))
            (: symF (-> Symbol R))
            (: varF (-> Variable R))
        ");

        assert!(validate_atom(&space, &atom("(atomF (atomR a))")));
        assert!(validate_atom(&space, &atom("(atomF (exprR a))")));
        assert!(validate_atom(&space, &atom("(atomF (gndR a))")));
        assert!(validate_atom(&space, &atom("(atomF (symR a))")));
        assert!(validate_atom(&space, &atom("(atomF (varR a))")));

        assert!(validate_atom(&space, &atom("(exprF (exprR a))")));
        // TODO: (exprF (atomR a)) is incorrectly typed, but (atomR a)
        // is an Expression and validate_atom returns True
        assert!(validate_atom(&space, &atom("(exprF (atomR a))")));
        
        assert!(validate_atom(&space, &atom("(gndF (gndR a))")));
        assert!(!validate_atom(&space, &atom("(gndF (atomR a))")));
        assert!(validate_atom(&space, &atom("(symF (symR a))")));
        assert!(!validate_atom(&space, &atom("(symF (atomR a))")));
        assert!(validate_atom(&space, &atom("(varF (varR a))")));
        assert!(!validate_atom(&space, &atom("(varF (atomR a))")));
    }
}
