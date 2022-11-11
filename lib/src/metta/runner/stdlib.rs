use crate::*;
use crate::matcher::MatchResultIter;
use crate::metta::*;
use crate::metta::space::grounding::GroundingSpace;
use crate::metta::text::{Tokenizer, SExprParser};
use crate::metta::interpreter::interpret;
use crate::metta::runner::Metta;
use crate::common::shared::Shared;

use std::fmt::Display;
use std::path::PathBuf;
use std::collections::HashMap;
use regex::Regex;

// TODO: remove hiding errors completely after making it possible passing
// them to the user
fn interpret_no_error(space: Shared<GroundingSpace>, expr: &Atom) -> Result<Vec<Atom>, String> {
    let result = interpret(space, expr);
    log::debug!("interpret_no_error: interpretation expr: {}, result {:?}", expr, result);
    match result {
        Ok(result) => Ok(result),
        Err(_) => Ok(vec![]),
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ImportOp {
    cwd: PathBuf,
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
}

impl ImportOp {
    pub fn new(cwd: PathBuf, space: Shared<GroundingSpace>, tokenizer: Shared<Tokenizer>) -> Self {
        Self{ cwd, space, tokenizer }
    }
}

impl Display for ImportOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import!")
    }
}

impl Grounded for ImportOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("import! expects two arguments: space and file path");
        let space = args.get(0).ok_or_else(arg_error)?;
        let file = args.get(1).ok_or_else(arg_error)?;

        let mut path = self.cwd.clone();
        // TODO: replace Symbol by grounded String?
        if let Atom::Symbol(file) = file {
            path.push(file.name());
            log::trace!("import! load file, full path: {}", path.display());
        } else {
            return Err("import! expects a file path as a second argument".into())
        }

        let space: Result<Shared<GroundingSpace>, String> = match space {
            Atom::Symbol(space) => {
                let name = space.name();
                let space = Shared::new(GroundingSpace::new());
                let space_atom = Atom::value(space.clone());
                let regex = Regex::new(name)
                    .map_err(|err| format!("Could convert space name {} into regex: {}", name, err))?;
                self.tokenizer.borrow_mut()
                    .register_token(regex, move |_| { space_atom.clone() });
                Ok(space)
            },
            Atom::Grounded(_) => {
                let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("import! expects a space as a first argument")?;
                Ok(space.clone())
            },
            _ => Err("import! expects space as a first argument".into()),
        };
        let space = space?;
        let mut next_cwd = path.clone();
        next_cwd.pop();
        let metta = Metta::from_space_cwd(space.clone(), next_cwd);
        let program = std::fs::read_to_string(&path)
            .map_err(|err| format!("Could not read file {}: {}", path.display(), err))?;
        let _result = metta.run(&mut SExprParser::new(program.as_str()))?;
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct MatchOp {}

impl Display for MatchOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match")
    }
}

impl Grounded for MatchOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::trace!("match_op: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("match expects a space as a second argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct BindOp {
    tokenizer: Shared<Tokenizer>,
}

impl BindOp {
    pub fn new(tokenizer: Shared<Tokenizer>) -> Self {
        Self{ tokenizer }
    }
}

impl Display for BindOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bind!")
    }
}

fn atom_as_sym(atom: &Atom) -> Option<&SymbolAtom> {
    match atom {
        Atom::Symbol(sym) => Some(sym),
        _ => None,
    }
}

impl Grounded for BindOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("bind! expects two arguments: token and atom");
        let token = atom_as_sym(args.get(0).ok_or_else(arg_error)?).ok_or("bind! expects symbol atom as a token")?.name();
        let atom = args.get(1).ok_or_else(arg_error)?.clone();

        let token_regex = Regex::new(token).map_err(|err| format!("Could convert token {} into regex: {}", token, err))?;
        self.tokenizer.borrow_mut().register_token(token_regex, move |_| { atom.clone() });
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NewSpaceOp {}

impl Display for NewSpaceOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "new-space")
    }
}

impl Grounded for NewSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>()])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::value(Shared::new(GroundingSpace::new()));
            Ok(vec![space])
        } else {
            Err("new-space doesn't expect arguments".into())
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CaseOp {
    space: Shared<GroundingSpace>,
}

impl CaseOp {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Self{ space }
    }

    fn first_case_matched(atom: &Atom, cases: &ExpressionAtom) -> Result<Vec<Atom>, ExecError> {
        let mut space = GroundingSpace::new();
        space.add(atom.clone());
        for c in cases.children() {
            log::trace!("CaseOp::first_case_matched: next case: {}", c);
            let next_case = atom_as_expr(c).ok_or("case expects expression of pairs as a second argument")?.children();
            let result = space.subst(&next_case[0], &next_case[1]);
            if !result.is_empty() {
                return Ok(result)
            }
        }
        return Ok(vec![])
    }
}

impl Display for CaseOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "case")
    }
}

// FIXME: move it into hyperon::atom module?
fn atom_as_expr(atom: &Atom) -> Option<&ExpressionAtom> {
    match atom {
        Atom::Expression(expr) => Some(expr),
        _ => None,
    }
}

impl Grounded for CaseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("case expects two arguments: atom and expression of cases");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let cases = atom_as_expr(args.get(1).ok_or_else(arg_error)?).ok_or("case expects expression of cases as a second argument")?;
        log::trace!("CaseOp::execute: atom: {}, cases: {}", atom, cases);

        let result = interpret_no_error(self.space.clone(), atom);
        log::trace!("case: interpretation result {:?}", result);
        match result {
            Ok(result) if result.is_empty() => {
                for c in cases.children() {
                    let next_case = atom_as_expr(c).ok_or("case expects expression of pairs as a second argument")?.children();
                    if next_case[0] == VOID_SYMBOL {
                        return Ok(vec![next_case[1].clone()])
                    }
                }
                Ok(vec![])
            },
            Ok(result) => {
                let mut triggered = vec![];
                for atom in result {
                    triggered.append(&mut CaseOp::first_case_matched(&atom, cases)?)
                }
                Ok(triggered)
            },
            Err(message) => Err(format!("Error: {}", message).into()),
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Vec<Atom> {
    log::trace!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    for r in actual {
        if !expected.contains(r) {
            return vec![Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::sym(format!("{}\nExcessive result: {}", report, r))])]
        }
    }
    for r in expected {
        if !actual.contains(r) {
            return vec![Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::sym(format!("{}\nMissed result: {}", report, r))])]
        }
    }
    if expected.len() != actual.len() {
        return vec![Atom::expr([ERROR_SYMBOL, Atom::sym(format!("{}\nDifferent number of elements", report))])]
    }
    return vec![]
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssertEqualOp {
    space: Shared<GroundingSpace>,
}

impl AssertEqualOp {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Self{ space }
    }
}

impl Display for AssertEqualOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assertEqual")
    }
}

impl Grounded for AssertEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("assertEqual expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;
        let expected = interpret_no_error(self.space.clone(), expected_atom)?;

        Ok(assert_results_equal(&actual, &expected, actual_atom))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssertEqualToResultOp {
    space: Shared<GroundingSpace>,
}

impl AssertEqualToResultOp {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Self{ space }
    }
}

impl Display for AssertEqualToResultOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "assertEqualToResult")
    }
}

impl Grounded for AssertEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("assertEqualToResult expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = atom_as_expr(args.get(1).ok_or_else(arg_error)?)
            .ok_or("assertEqualToResult expects expression of results as a second argument")?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;

        Ok(assert_results_equal(&actual, expected, actual_atom))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CollapseOp {
    space: Shared<GroundingSpace>,
}

impl CollapseOp {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Self{ space }
    }
}

impl Display for CollapseOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "collapse")
    }
}

impl Grounded for CollapseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("collapse expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        let result = interpret_no_error(self.space.clone(), atom)?;

        Ok(vec![Atom::expr(result)])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SuperposeOp { }

impl Display for SuperposeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "collapse")
    }
}

impl Grounded for SuperposeOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("superpose expects single expression as an argument");
        let expr = atom_as_expr(args.get(0).ok_or_else(arg_error)?).ok_or(arg_error())?;

        Ok(expr.children().clone())
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PragmaOp {
    settings: Shared<HashMap<String, String>>,
}

impl PragmaOp {
    pub fn new(settings: Shared<HashMap<String, String>>) -> Self {
        Self{ settings }
    }
}

impl Display for PragmaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pragma!")
    }
}

impl Grounded for PragmaOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = atom_as_sym(args.get(0).ok_or_else(arg_error)?).ok_or("pragma! expects symbol atom as a key")?.name();
        let value = atom_as_sym(args.get(1).ok_or_else(arg_error)?).ok_or("pragma! expects symbol atom as a value")?.name();

        self.settings.borrow_mut().insert(key.into(), value.into());

        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_superpose() {
        let superpose = SuperposeOp{};
        assert_eq!(superpose.execute(&mut vec![expr!("A" ("B" "C"))]),
            Ok(vec![sym!("A"), expr!("B" "C")]));
    }
}
