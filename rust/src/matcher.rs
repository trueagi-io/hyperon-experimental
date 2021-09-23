use super::*;

fn check_and_insert_binding(bindings: &mut Bindings, var: &VariableAtom,
        value: &Atom) -> bool{
    match bindings.get(var) {
        Some(prev) => prev == value,
        None => {
            bindings.insert(var.clone(), value.clone());
            true
        }
    }
}

fn match_atoms_recursively(a: &Atom, b: &Atom,
        a_bindings: &mut Bindings, b_bindings: &mut Bindings) -> bool {
    match (a, b) {
        (Atom::Symbol{ symbol: a }, Atom::Symbol{ symbol: b }) => a == b,
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

pub fn match_atoms(a: &Atom, b: &Atom) -> Option<(Bindings, Bindings)> {
    let mut a_bindings = Bindings::new();
    let mut b_bindings = Bindings::new();
    if match_atoms_recursively(a, b, &mut a_bindings, &mut b_bindings) {
        Some((a_bindings, b_bindings))
    } else {
        None
    }
}


