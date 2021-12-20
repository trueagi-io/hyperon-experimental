use super::*;

fn check_and_insert_binding(bindings: &mut Bindings, var: &VariableAtom,
        value: &Atom) -> bool{
    let compatible = match bindings.get(var){
        Some(prev) => prev == value,
        None => true,
    };
    if compatible {
        bindings.insert(var.clone(), value.clone());
    }
    compatible
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchResult {
    pub candidate_bindings: Bindings,
    pub pattern_bindings: Bindings,
}

impl MatchResult {
    fn new() -> Self {
        MatchResult {
            candidate_bindings: Bindings::new(),
            pattern_bindings: Bindings::new()
        }
    }
}

impl From<(Bindings, Bindings)> for MatchResult {
    fn from((candidate_bindings, pattern_bindings): (Bindings, Bindings)) -> Self {
        Self { candidate_bindings, pattern_bindings }
    }
}

fn match_atoms_recursively(candidate: &Atom, pattern: &Atom, res: &mut MatchResult) -> bool {
    match (candidate, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) => a == b,
        (Atom::Grounded(a), Atom::Grounded(b)) => a.eq_gnd(&**b),
        (Atom::Variable(_), Atom::Variable(v)) => {
            // We stick to prioritize pattern bindings in this case
            // because otherwise the $X in (= (...) $X) will not be matched with
            // (= (if True $then) $then)
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, candidate);
            check_and_insert_binding(&mut res.pattern_bindings, v, candidate)
        }
        (Atom::Variable(v), b) => {
            log::trace!("check_and_insert_binding for candidate({:?}, {}, {})", res.candidate_bindings, v, b);
            check_and_insert_binding(&mut res.candidate_bindings, v, b)
        }
        (a, Atom::Variable(v)) => {
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, a);
            check_and_insert_binding(&mut res.pattern_bindings, v, a)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b })) => {
            if a.len() != b.len() {
                false
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && match_atoms_recursively(pair.0, pair.1, res))
            }
        },
        _ => false,
    }
}

pub fn match_atoms(candidate: &Atom, pattern: &Atom) -> Option<MatchResult> {
    log::trace!("match_atoms: candidate: {}, pattern: {}", candidate, pattern);
    let mut res = MatchResult::new();
    if match_atoms_recursively(candidate, pattern, &mut res) {
        Some(res)
    } else {
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnificationPair {
    pub candidate: Atom,
    pub pattern: Atom,
}

impl From<(Atom, Atom)> for UnificationPair {
    fn from((candidate, pattern): (Atom, Atom)) -> Self {
        Self { candidate, pattern }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnifyResult {
    pub candidate_bindings: Bindings,
    pub pattern_bindings: Bindings,
    pub unifications: Vec<UnificationPair>,
}

impl UnifyResult {
    fn new() -> Self {
        UnifyResult {
            candidate_bindings: Bindings::new(),
            pattern_bindings: Bindings::new(),
            unifications: Vec::new(),
        }
    }
}

fn unify_atoms_recursively(candidate: &Atom, pattern: &Atom, res: &mut UnifyResult, depth: u32) -> bool {
    match (candidate, pattern) {
        (Atom::Symbol(a), Atom::Symbol(b)) => a == b,
        (Atom::Grounded(a), Atom::Grounded(b)) => a.eq_gnd(&**b),
        (Atom::Variable(_), Atom::Variable(v)) => {
            // We stick to prioritize pattern bindings in this case
            // because otherwise the $X in (= (...) $X) will not be matched with
            // (= (if True $then) $then)
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, candidate);
            check_and_insert_binding(&mut res.pattern_bindings, v, candidate)
        }
        (Atom::Variable(v), b) => {
            log::trace!("check_and_insert_binding for candidate({:?}, {}, {})", res.candidate_bindings, v, b);
            check_and_insert_binding(&mut res.candidate_bindings, v, b)
        }
        (a, Atom::Variable(v)) => {
            log::trace!("check_and_insert_binding for pattern({:?}, {}, {})", res.pattern_bindings, v, a);
            check_and_insert_binding(&mut res.pattern_bindings, v, a)
        },
        (Atom::Expression(ExpressionAtom{ children: a }), Atom::Expression(ExpressionAtom{ children: b })) => {
            if a.len() != b.len() {
                if depth == 1 {
                    false
                } else {
                    res.unifications.push((candidate.clone(), pattern.clone()).into());
                    true
                }
            } else {
                a.iter().zip(b.iter()).fold(true,
                    |succ, pair| succ && unify_atoms_recursively(pair.0, pair.1, res, depth + 1))
            }
        },
        (Atom::Expression(_), _) | (_, Atom::Expression(_)) => {
            res.unifications.push((candidate.clone(), pattern.clone()).into());
            true
        }
        _ => false,
    }
}

pub fn unify_atoms(candidate: &Atom, pattern: &Atom) -> Option<UnifyResult> {
    log::trace!("unify_atoms: candidate: {}, pattern: {}", candidate, pattern);
    let mut res = UnifyResult::new();
    if unify_atoms_recursively(candidate, pattern, &mut res, 0) {
        Some(res)
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

fn atom_contains_variable(atom: &Atom, var: &VariableAtom) -> bool {
    match atom {
        Atom::Expression(ExpressionAtom{ children }) =>
            children.iter().any(|sub| atom_contains_variable(sub, var)),
        Atom::Variable(v) => v == var,
        _ => false,
    }
}

pub fn apply_bindings_to_bindings(from: &Bindings, to: &Bindings) -> Result<Bindings, ()> {
    let mut res = Bindings::new();
    for (key, value) in to {
        let applied = apply_bindings_to_atom(value, from);
        // Check that variable is not expressed via itself, if so it is
        // a task for unification not for matching
        if !matches!(applied, Atom::Variable(_)) && atom_contains_variable(&applied, key) {
            return Err(())
        }
        if !check_and_insert_binding(&mut res, key, &applied) {
            return Err(())
        }
    }
    return Ok(res);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_match_variables_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", b, c)), &expr!("+", "A", ("*", "B", "C"))),
            Some(MatchResult::from((bind!{a: expr!("A"), b: expr!("B"), c: expr!("C") }, bind!{}))));
    }

    #[test]
    fn test_match_different_value_for_variable_in_data() {
        assert_eq!(
            match_atoms(&expr!("+", a, ("*", a, c)), &expr!("+", "A", ("*", "B", "C"))),
            None);
    }
}
