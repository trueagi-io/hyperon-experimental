use crate::*;
use crate::atom::matcher::{Bindings, apply_bindings_to_atom};
use crate::space::grounding::GroundingSpace;

use std::fmt::{Debug, Display};

#[inline]
fn has_type_symbol() -> Atom { sym!(":") }
#[inline]
fn sub_type_symbol() -> Atom { sym!("<") }
#[inline]
fn undefined_symbol() -> Atom { sym!("%Undefined%") }
#[inline]
fn arrow_symbol() -> Atom { sym!("->") }

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AtomType {
    Undefined,
    Specific(Atom),
}

impl AtomType {
    pub fn map_or<R, F>(&self, f: F, default: R) -> R where
            F: FnOnce(&Atom) -> R {
        match self {
            AtomType::Specific(ref typ) => f(typ),
            AtomType::Undefined => default,
        }
    }
}

impl Display for AtomType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

fn typeof_query(atom: &Atom, typ: &Atom) -> Atom {
    Atom::expr(vec![has_type_symbol(), atom.clone(), typ.clone()])
}

fn isa_query(sub_type: &Atom, super_type: &Atom) -> Atom {
    Atom::expr(vec![sub_type_symbol(), sub_type.clone(), super_type.clone()])
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
            expr.children().first() == Some(&arrow_symbol())
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
                [op,  args @ .., res] if *op == arrow_symbol() => (args, res),
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

pub fn get_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    log::trace!("get_types: atom: {}", atom);
    let types = match atom {
        Atom::Variable(_) | Atom::Grounded(_) => vec![undefined_symbol()],
        Atom::Symbol(_) => {
            let mut types = query_types(space, atom);
            if types.is_empty() {
                types.push(undefined_symbol())
            }
            types
        },
        Atom::Expression(expr) => {
            // tuple type
            let mut tuples = vec![vec![]];
            for (i, child) in expr.children().iter().enumerate() {
                // TODO: it is not straightforward, if (: a (-> B C)) then
                // what should we return for (d (a b)): (D (-> B C)) or
                // (D C) or both? Same question for a function call.
                let child_types = get_types(space, child);
                let child_types = child_types.iter()
                    .filter(|typ| i != 0 || !is_func(typ));
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
                .filter(|children| children.iter().any(|child| *child != undefined_symbol()))
                .map(Atom::expr).collect();
            types.append(&mut query_types(space, atom));
            add_super_types(space, &mut types, 0);

            // functional types
            let op = get_op(expr);
            let args = get_args(expr);
            let actual_arg_types: Vec<Vec<Atom>> = args.iter().map(|arg| get_reducted_types(space, arg)).collect();
            let mut fn_types = get_types(space, op);
            let fn_types = fn_types.drain(0..).filter(is_func);
            let mut only_tuple = true;
            for fn_type in fn_types {
                only_tuple = false;
                let (expected_arg_types, _) = get_arg_types(&fn_type);
                let mut bindings = Bindings::new();
                if check_types(actual_arg_types.as_slice(), expected_arg_types, &mut bindings) {
                    types.push(apply_bindings_to_atom(&fn_type, &bindings));
                }
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
                types.push(undefined_symbol())
            }
            types
        },
    };
    log::trace!("get_types: atom: {} return {:?}", atom, types);
    types
}


pub fn get_reducted_types(space: &GroundingSpace, atom: &Atom) -> Vec<Atom> {
    get_types(space, atom).drain(0..).map(|typ| {
        if matches!(atom, Atom::Expression(_)) && is_func(&typ) {
            let (_, ret) = get_arg_types(&typ);
            ret.clone()
        } else {
            typ
        }
    }).collect()
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
    let undefined = undefined_symbol();
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
        let mut space = GroundingSpace::new();
        space.add(expr!(":", "do", "Verb"));
        space.add(expr!(":", "do", "Aux"));

        let aux = AtomType::Specific(sym!("Aux"));
        let verb = AtomType::Specific(sym!("Verb"));

        let nonsense = sym!("nonsense");
        assert!(check_type(&space, &nonsense, &AtomType::Undefined));
        assert!(check_type(&space, &nonsense, &aux));

        let _do = sym!("do");
        assert!(check_type(&space, &_do, &AtomType::Undefined));
        assert!(check_type(&space, &_do, &aux));
        assert!(check_type(&space, &_do, &verb));
        assert!(!check_type(&space, &_do, &AtomType::Specific(sym!("Noun"))));

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
        assert!(check_type(&space, &i_like_music, &AtomType::Specific(sym!("Statement"))));

        assert!(check_type(&space, &expr!("do", "you", "like", "music"), &AtomType::Specific(sym!("Quest"))));
    }

    #[test]
    fn nested_type() {
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
        let space = grammar_space();
        let expr = expr!("answer", ("do", "you", "like", ("a", "pizza")));

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
            &AtomType::Specific(atom("(Color Object)"))));
    }

    #[test]
    fn arrow_type() {
        let space = metta_space("
            (: a (-> B A))
        ");

        assert!(check_type(&space, &atom("a"), &AtomType::Specific(atom("(-> B A)"))));
    }

    #[test]
    fn arrow_allows_specific_type() {
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
        let space = GroundingSpace::new();
        assert!(validate_atom(&space, &expr!({5})));
        assert!(validate_atom(&space, &expr!("+", {3}, {5})));
        assert!(validate_atom(&space, &expr!("=", ("f", x), x)));
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
                           &AtomType::Specific(atom("(Frog Sam)"))));
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

        assert!(check_type(&space, &atom("(a b)"), &AtomType::Specific(atom("A"))));
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

        assert!(check_type(&space, &atom("(a b)"), &AtomType::Specific(atom("(A B)"))));
    }

    #[test]
    fn get_type_undefined_expression_type() {
        let space = metta_space("
            (: a A)
        ");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![atom("(A %Undefined%)")]);

        let space = metta_space("");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![undefined_symbol()]);

        let space = metta_space("
            (: a (-> C D))
            (: b B)
        ");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![]);
    }

    #[test]
    fn get_type_function_call() {
        let space = metta_space("
            (: a (-> B C))
            (: b B)
        ");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![atom("(-> B C)")]);
    }

    #[test]
    fn get_type_tuple() {
        let space = metta_space("
            (: a A)
            (: b B)
        ");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![atom("(A B)")]);
    }

    #[test]
    fn get_type_function_call_and_tuple() {
        let space = metta_space("
            (: a (-> B C))
            (: a A)
            (: b B)
        ");
        assert_eq!(get_types(&space, &atom("(a b)")), vec![atom("(A B)"), atom("(-> B C)")]);
    }
}
