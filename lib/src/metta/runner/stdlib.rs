use crate::*;
use crate::matcher::MatchResultIter;
use crate::metta::*;
use crate::metta::space::grounding::GroundingSpace;
use crate::metta::text::Tokenizer;
use crate::metta::interpreter::interpret;
use crate::metta::runner::Metta;
use crate::metta::types::get_atom_types;
use crate::common::shared::Shared;
use crate::common::assert::vec_eq_no_order;

use std::fmt::Display;
use std::path::PathBuf;
use std::collections::HashMap;
use std::iter::FromIterator;
use regex::Regex;

use super::arithmetics::*;

pub const VOID_SYMBOL : Atom = sym!("%void%");

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
    metta: Shared<Metta>,
}

impl ImportOp {
    pub fn new(metta: Shared<Metta>, cwd: PathBuf) -> Self {
        Self{ metta, cwd }
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
        let tokenizer = &self.metta.borrow().tokenizer;

        let mut path = self.cwd.clone();
        // TODO: replace Symbol by grounded String?
        if let Atom::Symbol(file) = file {
            path.push(file.name());
            log::debug!("import! load file, full path: {}", path.display());
        } else {
            return Err("import! expects a file path as a second argument".into())
        }

        let tokenizer_before_adding_imported_space = tokenizer.cloned();
        let space: Result<Shared<GroundingSpace>, String> = match space {
            Atom::Symbol(space) => {
                let name = space.name();
                let space = Shared::new(GroundingSpace::new());
                let space_atom = Atom::gnd(space.clone());
                let regex = Regex::new(name)
                    .map_err(|err| format!("Could convert space name {} into regex: {}", name, err))?;
                tokenizer.borrow_mut()
                    .register_token(regex, move |_| { space_atom.clone() });
                Ok(space)
            },
            Atom::Grounded(_) => {
                let space = Atom::as_gnd::<Shared<GroundingSpace>>(space)
                    .ok_or("import! expects a space as a first argument")?;
                Ok(space.clone())
            },
            _ => Err("import! expects space as a first argument".into()),
        };
        let space = space?;
        let mut next_cwd = path.clone();
        next_cwd.pop();
        let metta = Metta::from_space_cwd(space, tokenizer_before_adding_imported_space, next_cwd);
        metta.load_module(path);
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
        log::debug!("match_op: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("match expects a space as the first argument")?;
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

// TODO: move it into hyperon::atom module?
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
            let space = Atom::gnd(Shared::new(GroundingSpace::new()));
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
pub struct AddAtomOp {}

impl Display for AddAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "add-atom")
    }
}

impl Grounded for AddAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>(),
            ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("add-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("add-atom expects a space as the first argument")?;
        space.borrow_mut().add(atom.clone());
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct RemoveAtomOp {}

impl Display for RemoveAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "remove-atom")
    }
}

impl Grounded for RemoveAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>(),
            ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("remove-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("remove-atom expects a space as the first argument")?;
        space.borrow_mut().remove(atom);
        // TODO? return Bool
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct GetAtomsOp {}

impl Display for GetAtomsOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-atoms")
    }
}

impl Grounded for GetAtomsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<Shared<GroundingSpace>>(),
            ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-atoms expects one argument: space");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<Shared<GroundingSpace>>(space).ok_or("get-atoms expects a space as its argument")?;
        Ok(space.borrow().content().clone())
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CarAtomOp {}

impl Display for CarAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "car-atom")
    }
}

impl Grounded for CarAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("car-atom expects one argument: expression");
        let expr = args.get(0).ok_or_else(arg_error)?;
        let chld = atom_as_expr(expr).ok_or_else(arg_error)?.children();
        let car = chld.get(0).ok_or("car-atom expects non-empty expression")?;
        Ok(vec![car.clone()])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct CdrAtomOp {}

impl Display for CdrAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cdr-atom")
    }
}

impl Grounded for CdrAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("cdr-atom expects one argument: expression");
        let expr = args.get(0).ok_or_else(arg_error)?;
        let chld = atom_as_expr(expr).ok_or_else(arg_error)?.children();
        let cdr = Vec::from_iter(chld[1..].iter().cloned());
        Ok(vec![Atom::expr(cdr)])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ConsAtomOp {}

impl Display for ConsAtomOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cons-atom")
    }
}

impl Grounded for ConsAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("cons-atom expects two arguments: atom and expression");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let expr = args.get(1).ok_or_else(arg_error)?;
        let chld = atom_as_expr(expr).ok_or_else(arg_error)?.children();
        let mut res = vec![atom.clone()];
        res.extend(chld.clone());
        Ok(vec![Atom::expr(res)])
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
            log::debug!("CaseOp::first_case_matched: next case: {}", c);
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

// TODO: move it into hyperon::atom module?
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
        log::debug!("CaseOp::execute: atom: {}, cases: {}", atom, cases);

        let result = interpret_no_error(self.space.clone(), atom);
        log::debug!("case: interpretation result {:?}", result);
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

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
    log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual, expected) {
        Ok(()) => Ok(vec![]),
        Err(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
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

        assert_results_equal(&actual, &expected, actual_atom)
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

        assert_results_equal(&actual, expected, actual_atom)
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
        let arg_error = || ExecError::from("collapse expects single executable atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        // TODO: Calling interpreter inside the operation is not too good
        // Could it be done via StepResult?
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
        // `superpose` receives one atom (expression) in order to make composition
        // `(superpose (collapse ...))` possible
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

        // TODO: add support for Grounded values when needed
        self.settings.borrow_mut().insert(key.into(), value.into());

        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct GetTypeOp {
    space: Shared<GroundingSpace>,
}

impl GetTypeOp {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Self{ space }
    }
}

impl Display for GetTypeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-type")
    }
}

impl Grounded for GetTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        Ok(get_atom_types(&self.space.borrow(), atom))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PrintlnOp {}

impl Display for PrintlnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "println!")
    }
}

impl Grounded for PrintlnOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, sym!("IO")])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("println! expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        println!("{}", atom);
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NopOp {}

impl Display for NopOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "nop")
    }
}

impl Grounded for NopOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        Ok(vec![])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LetOp {}

impl Display for LetOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let")
    }
}

impl Grounded for LetOp {
    fn type_(&self) -> Atom {
        // TODO: Undefined for the argument is necessary to make argument reductable.
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("let expects three arguments: pattern, atom and template");
        let pattern = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;

        log::debug!("LetOp::execute: pattern: {}, atom: {}, template: {}", pattern, atom, template);

        let mut space = GroundingSpace::new();
        space.add(atom.clone());
        Ok(space.subst(pattern, template))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LetVarOp { }

impl Display for LetVarOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let*")
    }
}

impl Grounded for LetVarOp {
    fn type_(&self) -> Atom {
        // The first argument is an Atom, because it has to be evaluated iteratively
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("let* list of couples and template as arguments");
        let expr = atom_as_expr(args.get(0).ok_or_else(arg_error)?).ok_or(arg_error())?;
        let template = args.get(1).ok_or_else(arg_error)?.clone();
        log::debug!("LetVarOp::execute: expr: {}, template: {}", expr, template);

        let children = expr.children().as_slice();
        match children {
            [] => Ok(vec![template]),
            [couple] => {
                let couple = atom_as_expr(couple).ok_or_else(arg_error)?.children();
                let pattern = couple.get(0).ok_or_else(arg_error)?.clone();
                let atom = couple.get(1).ok_or_else(arg_error)?.clone();
                Ok(vec![Atom::expr([Atom::gnd(LetOp{}), pattern, atom, template])])
            },
            [couple, tail @ ..] => {
                let couple = atom_as_expr(couple).ok_or_else(arg_error)?.children();
                let pattern = couple.get(0).ok_or_else(arg_error)?.clone();
                let atom = couple.get(1).ok_or_else(arg_error)?.clone();
                Ok(vec![Atom::expr([Atom::gnd(LetOp{}), pattern, atom,
                    Atom::expr([Atom::gnd(LetVarOp{}), Atom::expr(tail), template])])])
            },
        }
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

pub fn register_common_tokens(metta: &Metta, cwd: PathBuf) {
    let space = &metta.space;
    let tokenizer = &metta.tokenizer;
    let smetta = metta.clone();

    let mut common_tokens = Tokenizer::new();
    let tref = &mut common_tokens;

    let match_op = Atom::gnd(MatchOp{});
    tref.register_token(regex(r"match"), move |_| { match_op.clone() });
    let import_op = Atom::gnd(ImportOp::new(smetta.clone(), cwd.clone()));
    tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
    let bind_op = Atom::gnd(BindOp::new(tokenizer.clone()));
    tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
    let new_space_op = Atom::gnd(NewSpaceOp{});
    tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
    let add_atom_op = Atom::gnd(AddAtomOp{});
    tref.register_token(regex(r"add-atom"), move |_| { add_atom_op.clone() });
    let remove_atom_op = Atom::gnd(RemoveAtomOp{});
    tref.register_token(regex(r"remove-atom"), move |_| { remove_atom_op.clone() });
    let get_atoms_op = Atom::gnd(GetAtomsOp{});
    tref.register_token(regex(r"get-atoms"), move |_| { get_atoms_op.clone() });
    let car_atom_op = Atom::gnd(CarAtomOp{});
    tref.register_token(regex(r"car-atom"), move |_| { car_atom_op.clone() });
    let cdr_atom_op = Atom::gnd(CdrAtomOp{});
    tref.register_token(regex(r"cdr-atom"), move |_| { cdr_atom_op.clone() });
    let cons_atom_op = Atom::gnd(ConsAtomOp{});
    tref.register_token(regex(r"cons-atom"), move |_| { cons_atom_op.clone() });
    let case_op = Atom::gnd(CaseOp::new(space.clone()));
    tref.register_token(regex(r"case"), move |_| { case_op.clone() });
    let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
    tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
    let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
    tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
    let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
    tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
    let superpose_op = Atom::gnd(SuperposeOp{});
    tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
    let pragma_op = Atom::gnd(PragmaOp::new(metta.settings.clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
    let get_type_op = Atom::gnd(GetTypeOp::new(space.clone()));
    tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
    let println_op = Atom::gnd(PrintlnOp{});
    tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
    let nop_op = Atom::gnd(NopOp{});
    tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
    let let_op = Atom::gnd(LetOp{});
    tref.register_token(regex(r"let"), move |_| { let_op.clone() });
    let let_var_op = Atom::gnd(LetVarOp{});
    tref.register_token(regex(r"let\*"), move |_| { let_var_op.clone() });

    metta.tokenizer.borrow_mut().move_front(&mut common_tokens);

    // &self should be updated
    // TODO: adding &self might be done not by stdlib, but by MeTTa itself
    let space_val = Atom::gnd(metta.space.clone());
    metta.tokenizer.borrow_mut().register_token(regex(r"&self"), move |_| { space_val.clone() });
}

pub fn register_rust_tokens(metta: &Metta) {
    let mut rust_tokens = Tokenizer::new();
    let tref = &mut rust_tokens;

    tref.register_token(regex(r"\d+"),
        |token| { Atom::gnd(Number::from_int_str(token)) });
    tref.register_token(regex(r"\d+(.\d+)([eE][\-\+]?\d+)?"),
        |token| { Atom::gnd(Number::from_float_str(token)) });
    tref.register_token(regex(r"True|False"),
        |token| { Atom::gnd(Bool::from_str(token)) });
    let sum_op = Atom::gnd(SumOp{});
    tref.register_token(regex(r"\+"), move |_| { sum_op.clone() });
    let sub_op = Atom::gnd(SubOp{});
    tref.register_token(regex(r"\-"), move |_| { sub_op.clone() });
    let mul_op = Atom::gnd(MulOp{});
    tref.register_token(regex(r"\*"), move |_| { mul_op.clone() });
    let div_op = Atom::gnd(DivOp{});
    tref.register_token(regex(r"/"), move |_| { div_op.clone() });
    let mod_op = Atom::gnd(ModOp{});
    tref.register_token(regex(r"%"), move |_| { mod_op.clone() });

    metta.tokenizer.borrow_mut().move_front(&mut rust_tokens);
}

pub fn metta_code() -> &'static str {
    // `$then`, `$else` should be of `Atom` type to avoid evaluation
    // and infinite cycle in inference
    "
    (: if (-> Bool Atom Atom $t))
    (= (if True $then $else) $then)
    (= (if False $then $else) $else)
    "
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn match_op() {
        let space = Shared::new(metta_space("
            (A B)
            !(match &self (A B) (B A))
        "));

        let match_op = MatchOp{};

        assert_eq!(match_op.execute(&mut vec![expr!({space}), expr!("A" "B"), expr!("B" "A")]),
            Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn new_space_op() {
        let res = NewSpaceOp{}.execute(&mut vec![]).expect("No result returned");
        let space = res.get(0).expect("Result is empty");
        let space = space.as_gnd::<Shared<GroundingSpace>>().expect("Result is not space");
        assert_eq!(*space.borrow().content(), vec![]);
    }

    #[test]
    fn add_atom_op() {
        let space = Shared::new(GroundingSpace::new());
        let satom = Atom::gnd(space.clone());
        let res = AddAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        assert!(res.is_empty());
        assert_eq!(*space.borrow().content(), vec![expr!(("foo" "bar"))]);
    }

    #[test]
    fn remove_atom_op() {
        let space = Shared::new(metta_space("
            (foo bar)
            (bar foo)
        "));
        let satom = Atom::gnd(space.clone());
        let res = RemoveAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        // REM: can return Bool in future
        assert!(res.is_empty());
        assert_eq!(*space.borrow().content(), vec![expr!(("bar" "foo"))]);
    }

    #[test]
    fn get_atoms_op() {
        let space = Shared::new(metta_space("
            (foo bar)
            (bar foo)
        "));
        let satom = Atom::gnd(space.clone());
        let res = GetAtomsOp{}.execute(&mut vec![satom]).expect("No result returned");
        assert_eq!(res, *space.borrow().content());
        assert_eq!(res, vec![expr!(("foo" "bar")), expr!(("bar" "foo"))]);
    }

    #[test]
    fn car_atom_op() {
        let res = CarAtomOp{}.execute(&mut vec![expr!(("A" "C") "B")]).expect("No result returned");
        assert_eq!(res, vec![expr!("A" "C")]);
    }

    #[test]
    fn cdr_atom_op() {
        let res = CdrAtomOp{}.execute(&mut vec![expr!(("A"))]).expect("No result returned");
        assert_eq!(res, vec![expr!()]);
        let res = CdrAtomOp{}.execute(&mut vec![expr!(("A" "C") ("D" "E") "B")]).expect("No result returned");
        assert_eq!(res, vec![expr!(("D" "E") "B")]);
    }

    #[test]
    fn cons_atom_op() {
        let res = ConsAtomOp{}.execute(&mut vec![expr!("A"), expr!()]).expect("No result returned");
        assert_eq!(res, vec![expr!(("A"))]);
        let res = ConsAtomOp{}.execute(&mut vec![expr!("A" "F"), expr!(("B" "C") "D")]).expect("No result returned");
        assert_eq!(res, vec![expr!(("A" "F") ("B" "C") "D")]);
    }

    #[test]
    fn bind_new_space_op() {
        let tokenizer = Shared::new(Tokenizer::new());

        let bind_op = BindOp::new(tokenizer.clone());

        assert_eq!(bind_op.execute(&mut vec![sym!("&my"), sym!("definition")]), Ok(vec![]));
        let borrowed = tokenizer.borrow();
        let constr = borrowed.find_token("&my");
        assert!(constr.is_some());
        assert_eq!(constr.unwrap()("&my"), sym!("definition"));
    }

    #[test]
    fn case_op() {
        let space = Shared::new(metta_space("
            (= (foo) (A B))
        "));

        let case_op = CaseOp::new(space.clone());

        assert_eq!(case_op.execute(&mut vec![expr!(("foo")),
                expr!(((n "B") n) ("%void%" "D"))]),
            Ok(vec![Atom::sym("A")]));
        assert_eq!(case_op.execute(&mut vec![expr!({MatchOp{}} {space} ("B" "C") ("C" "B")),
                expr!(((n "C") n) ("%void%" "D"))]),
            Ok(vec![Atom::sym("D")]));
    }

    fn assert_runtime_error(actual: Result<Vec<Atom>, ExecError>, expected: Regex) {
        match actual {
            Err(ExecError::Runtime(msg)) => assert!(expected.is_match(msg.as_str()),
                "Incorrect error message:\nexpected: {:?}\n  actual: {:?}", expected.to_string(), msg),
            _ => assert!(false, "Error is expected as result, {:?} returned", actual),
        }
    }

    #[test]
    fn assert_equal_op() {
        let space = Shared::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
            (= (bar) (B C))
            (= (bar) (A B))
            (= (err) (A B))
        "));

        let assert_equal_op = AssertEqualOp::new(space);

        assert_eq!(assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]), Ok(vec![]));

        let actual = assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("err"))]);
        let expected = Regex::new("\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)").unwrap();
        assert_runtime_error(actual, expected);

        let actual = assert_equal_op.execute(&mut vec![expr!(("err")), expr!(("foo"))]);
        let expected = Regex::new("\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)").unwrap();
        assert_runtime_error(actual, expected);
    }

    #[test]
    fn assert_equal_to_result_op() {
        let space = Shared::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
        "));
        let assert_equal_to_result_op = AssertEqualToResultOp::new(space);

        assert_eq!(assert_equal_to_result_op.execute(&mut vec![
                expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
                Ok(vec![]));
    }

    #[test]
    fn collapse_op() {
        let space = Shared::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
        "));
        let collapse_op = CollapseOp::new(space);

        let actual = collapse_op.execute(&mut vec![expr!(("foo"))]).unwrap();
        assert_eq!(actual.len(), 1);
        assert_eq_no_order!(
            atom_as_expr(&actual[0]).unwrap().children(),
            vec![expr!("B" "C"), expr!("A" "B")]);
    }

    #[test]
    fn superpose_op() {
        let superpose_op = SuperposeOp{};
        assert_eq!(superpose_op.execute(&mut vec![expr!("A" ("B" "C"))]),
            Ok(vec![sym!("A"), expr!("B" "C")]));
    }

    #[test]
    fn get_type_op() {
        let space = Shared::new(metta_space("
            (: B Type)
            (: C Type)
            (: A B)
            (: A C)
        "));

        let get_type_op = GetTypeOp::new(space);
        assert_eq_no_order!(get_type_op.execute(&mut vec![sym!("A")]).unwrap(),
            vec![sym!("B"), sym!("C")]);
    }

    #[test]
    fn println_op() {
        assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), Ok(vec![]));
    }

    #[test]
    fn nop_op() {
        assert_eq!(NopOp{}.execute(&mut vec![]), Ok(vec![]));
    }

    #[test]
    fn let_op() {
        assert_eq!(LetOp{}.execute(&mut vec![expr!(a b), expr!("A" "B"), expr!(b a)]),
            Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn let_var_op() {
        assert_eq!(LetVarOp{}.execute(&mut vec![expr!(), sym!("B")]),
            Ok(vec![sym!("B")]));
        assert_eq!(LetVarOp{}.execute(&mut vec![expr!(((a "A"))), expr!(a)]),
            Ok(vec![expr!({LetOp{}} a "A" a)]));
        assert_eq!(LetVarOp{}.execute(&mut vec![expr!((a "A") (b "B")), expr!(b a)]),
            Ok(vec![expr!({LetOp{}} a "A" ({LetVarOp{}} ((b "B")) (b a)))]));
    }
}
