use crate::*;
use crate::common::shared::Shared;

use super::*;
use super::space::grounding::GroundingSpace;
use super::text::{Tokenizer, SExprParser};
use super::interpreter::interpret;
use super::types::validate_atom;

use regex::Regex;
use std::path::PathBuf;
use std::collections::HashMap;

const EXEC_SYMBOL : Atom = sym!("!");

pub struct Metta {
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
    settings: Shared<HashMap<String, String>>,
}

enum Mode {
    ADD,
    INTERPRET,
}

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

impl Metta {
    pub fn new(space: Shared<GroundingSpace>) -> Self {
        Metta::from_space_cwd(space, PathBuf::from("."))
    }

    pub fn from_space_cwd(space: Shared<GroundingSpace>, cwd: PathBuf) -> Self {
        let settings = Shared::new(HashMap::new());
        let tokenizer = Shared::new(Tokenizer::new());
        {
            fn regex(regex: &str) -> Regex {
                Regex::new(regex).unwrap()
            }

            let mut tref = tokenizer.borrow_mut();
            let match_op = Atom::gnd(MatchOp{});
            tref.register_token(regex(r"match"), move |_| { match_op.clone() });
            let space_val = Atom::value(space.clone());
            tref.register_token(regex(r"&self"), move |_| { space_val.clone() });
            let import_op = Atom::gnd(ImportOp::new(cwd.clone(), space.clone(), tokenizer.clone()));
            tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
            let bind_op = Atom::gnd(BindOp::new(tokenizer.clone()));
            tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
            let new_space_op = Atom::gnd(NewSpaceOp{});
            tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
            let case_op = Atom::gnd(CaseOp::new(space.clone()));
            tref.register_token(regex(r"case"), move |_| { case_op.clone() });
            let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
            tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
            let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
            tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
            let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
            tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
            let pragma_op = Atom::gnd(PragmaOp::new(settings.clone()));
            tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
        }
        Self{ space, tokenizer, settings }
    }

    pub fn space(&self) -> Shared<GroundingSpace> {
        self.space.clone()
    }

    pub fn tokenizer(&self) -> Shared<Tokenizer> {
        self.tokenizer.clone()
    }

    fn get_setting(&self, key: &str) -> Option<String> {
        self.settings.borrow().get(key.into()).cloned()
    }

    pub fn run(&self, parser: &mut SExprParser) -> Result<Vec<Vec<Atom>>, String> {
        let mut mode = Mode::ADD;
        let mut results: Vec<Vec<Atom>> = Vec::new();

        loop {
            let atom = parser.parse(&self.tokenizer.borrow());
            match atom {
                Some(atom) => {
                    if atom == EXEC_SYMBOL {
                        mode = Mode::INTERPRET;
                        continue;
                    }
                    match self.interp_atom(mode, atom) {
                        Err(msg) => return Err(msg),
                        Ok(Some(result)) => results.push(result),
                        _ => {},
                    }
                    mode = Mode::ADD;
                },
                None => break,
            }
        }
        Ok(results)
    }

    fn interp_atom(&self, mode: Mode, atom: Atom) -> Result<Option<Vec<Atom>>, String> {
        // FIXME: how to make it look better?
        if self.get_setting("type-check").as_ref().map(String::as_str) == Some("auto") {
            if !validate_atom(&self.space.borrow(), &atom) {
                return Ok(Some(vec![Atom::expr([ERROR_SYMBOL, atom, BAD_TYPE_SYMBOL])]))
            }
        }
        match mode {
            Mode::ADD => {
                log::trace!("Metta::run: adding atom: {} into space: {:?}", atom, self.space);
                self.space.borrow_mut().add(atom);
                Ok(None) 
            },
            Mode::INTERPRET => {
                log::trace!("Metta::run: interpreting atom: {}", atom);
                let result = interpret_no_error(self.space.clone(), &atom);
                log::trace!("Metta::run: interpretation result {:?}", result);
                match result {
                    Ok(result) => Ok(Some(result)),
                    Err(message) => Err(format!("Error: {}", message)),
                }
            },
        }
    }

}

use crate::matcher::MatchResultIter;
use std::fmt::Display;

#[derive(Clone, PartialEq, Debug)]
struct ImportOp {
    cwd: PathBuf,
    space: Shared<GroundingSpace>,
    tokenizer: Shared<Tokenizer>,
}

impl ImportOp {
    fn new(cwd: PathBuf, space: Shared<GroundingSpace>, tokenizer: Shared<Tokenizer>) -> Self {
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
struct MatchOp {}

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
struct BindOp {
    tokenizer: Shared<Tokenizer>,
}

impl BindOp {
    fn new(tokenizer: Shared<Tokenizer>) -> Self {
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
struct NewSpaceOp {}

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
struct CaseOp {
    space: Shared<GroundingSpace>,
}

impl CaseOp {
    fn new(space: Shared<GroundingSpace>) -> Self {
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
struct AssertEqualOp {
    space: Shared<GroundingSpace>,
}

impl AssertEqualOp {
    fn new(space: Shared<GroundingSpace>) -> Self {
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
struct AssertEqualToResultOp {
    space: Shared<GroundingSpace>,
}

impl AssertEqualToResultOp {
    fn new(space: Shared<GroundingSpace>) -> Self {
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
struct CollapseOp {
    space: Shared<GroundingSpace>,
}

impl CollapseOp {
    fn new(space: Shared<GroundingSpace>) -> Self {
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
struct PragmaOp {
    settings: Shared<HashMap<String, String>>,
}

impl PragmaOp {
    fn new(settings: Shared<HashMap<String, String>>) -> Self {
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
    fn test_space() {
        let program = "
            (= (And T T) T)
            (= (frog $x)
                (And (croaks $x)
                     (eat_flies $x)))
            (= (croaks Fritz) T)
            (= (eat_flies Fritz) T)
            (= (green $x) (frog $x))
            !(green Fritz)
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::sym("T")]]));
    }

    #[test]
    fn test_match() {
        let program = "
            (A B)
            !(match &self (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
    }

    #[test]
    fn new_space() {
        let program = "
            (A B)
            !(match (new-space) (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn bind_new_space() {
        let program = "
            (A B)
            !(bind! &my (new-space))
            !(match &my (A B) (B A))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![], vec![]]));
    }

    #[test]
    fn case() {
        let program = "
            (= (foo) (A B))
            !(case (foo) (
                (($n B) $n)
                (%void% D)
            ))
            !(case (match &self (B C) (B C)) (
                (($n C) $n)
                (%void% D)
            ))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![Atom::sym("A")], vec![Atom::sym("D")]]));
    }

    fn assert_error(atom: Atom, message: &str) -> Atom {
        Atom::expr([ERROR_SYMBOL, atom, Atom::sym(message)])
    }

    #[test]
    fn assert_equal() {
        let program = "
            (= (foo) (A B))
            (= (foo) (B C))
            (= (bar) (B C))
            (= (bar) (A B))
            (= (err) (A B))
            !(assertEqual (foo) (bar))
            !(assertEqual (foo) (err))
            !(assertEqual (err) (foo))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![
            vec![],
            vec![assert_error(expr!("A" "B"), "\nExpected: [(A B)]\nGot: [(A B), (B C)]\nExcessive result: (B C)"),
                 assert_error(expr!("B" "C"), "\nExpected: [(A B)]\nGot: [(A B), (B C)]\nExcessive result: (B C)")],
            vec![assert_error(expr!("A" "B"), "\nExpected: [(A B), (B C)]\nGot: [(A B)]\nMissed result: (B C)")]
        ]));
    }

    #[test]
    fn assert_equal_to_result() {
        let program = "
            (= (foo) (A B))
            (= (foo) (B C))
            !(assertEqualToResult (foo) ((B C) (A B)))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn collapse() {
        let program = "
            (= (foo) (A B))
            (= (foo) (B C))
            !(collapse (foo))
        ";

        let metta = Metta::new(Shared::new(GroundingSpace::new()));
        let result = metta.run(&mut SExprParser::new(program));
        assert_eq!(result, Ok(vec![vec![expr!(("A" "B") ("B" "C"))]]));
    }
}
