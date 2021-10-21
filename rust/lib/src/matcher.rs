use super::*;

fn check_and_insert_binding(bindings: &mut Bindings, var: &VariableAtom,
        value: &Atom) -> bool{
    // TODO: replace by logger
    println!("check_and_insert_binding({:?}, {}, {})", bindings, var, value);
    match bindings.get(var) {
        Some(prev) => prev == value,
        None => {
            bindings.insert(var.clone(), value.clone());
            true
        }
    }
}

fn match_atoms_recursively(atom: &Atom, pattern: &Atom,
        a_bindings: &mut Bindings, b_bindings: &mut Bindings) -> bool {
    match (atom, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) => a == b,
        (Atom::Grounded(a), Atom::Grounded(b)) => a.eq_gnd(&**b),
        (a, Atom::Variable(v)) => check_and_insert_binding(b_bindings, v, a),
        (Atom::Variable(v), b) => check_and_insert_binding(a_bindings, v, b),
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b })) => {
            if a.len() != b.len() {
                false
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && match_atoms_recursively(pair.0, pair.1,
                        a_bindings, b_bindings))
            }
        },
        _ => false,
    }
}

pub fn match_atoms(atom: &Atom, pattern: &Atom) -> Option<(Bindings, Bindings)> {
    let mut a_bindings = Bindings::new();
    let mut b_bindings = Bindings::new();
    if match_atoms_recursively(atom, pattern, &mut a_bindings, &mut b_bindings) {
        Some((a_bindings, b_bindings))
    } else {
        None
    }
}

pub fn apply_bindings_to_atom(atom: &Atom, bindings: &Bindings) -> Atom {
    match atom {
        Atom::Symbol(_)|Atom::Grounded(_) => atom.clone(),
        Atom::Variable(v) => {
            if let Some(binding) = bindings.get(v) {
                binding.clone()
            } else {
                Atom::Variable(v.clone())
            }
        },
        Atom::Expression(ExpressionAtom{ children }) => {
            let children = children.iter().map(|a| apply_bindings_to_atom(a, bindings)).collect::<Vec<Atom>>();
            Atom::expr(&children[..])
        },
    }
}

pub fn apply_bindings_to_bindings(from: &Bindings, to: &Bindings) -> Bindings {
    let mut res = Bindings::new();
    for (key, value) in to {
        let applied = apply_bindings_to_atom(value, from);
        res.insert(key.clone(), applied);
    }
    res
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_match_variables_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", b, c)), &expr!("+", "A", ("*", "B", "C"))),
            Some((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{})));
    }

    #[test]
    fn test_match_different_value_for_variable_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", a, c)), &expr!("+", "A", ("*", "B", "C"))),
            None);
    }
}
