use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::common::shared::Shared;
use crate::metta::runner::{Metta, RunContext, ResourceKey};
use crate::metta::runner::string::Str;
use crate::common::CachingMapper;
#[cfg(feature = "pkg_mgmt")]

use std::convert::TryInto;
use std::collections::HashMap;
use regex::Regex;

use crate::metta::runner::string::*;
use crate::metta::runner::stdlib_minimal::grounded_op;
use crate::metta::runner::stdlib_minimal::unit_result;
use crate::metta::runner::stdlib_minimal::interpret_no_error;
use crate::metta::runner::stdlib_minimal::assert_results_equal;
use crate::metta::runner::stdlib_minimal::interpret;
use crate::metta::runner::stdlib_minimal::atom_to_string;

#[derive(Clone, Debug)]
pub struct AssertEqualOp {
    space: DynSpace,
}

grounded_op!(AssertEqualOp, "assertEqual");

impl AssertEqualOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for AssertEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqual expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected_atom = args.get(1).ok_or_else(arg_error)?;

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;
        let expected = interpret_no_error(self.space.clone(), expected_atom)?;

        assert_results_equal(&actual, &expected, actual_atom)
    }
}

#[derive(Clone, Debug)]
pub struct AssertEqualToResultOp {
    space: DynSpace,
}

grounded_op!(AssertEqualToResultOp, "assertEqualToResult");

impl AssertEqualToResultOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for AssertEqualToResultOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AssertEqualToResultOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        log::debug!("AssertEqualToResultOp::execute: {:?}", args);
        let arg_error = || ExecError::from("assertEqualToResult expects two atoms as arguments: actual and expected");
        let actual_atom = args.get(0).ok_or_else(arg_error)?;
        let expected = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)
            .map_err(|_| arg_error())?
            .children();

        let actual = interpret_no_error(self.space.clone(), actual_atom)?;

        assert_results_equal(&actual, &expected.into(), actual_atom)
    }
}

#[derive(Clone, Debug)]
pub struct SuperposeOp {
    space: DynSpace,
}

grounded_op!(SuperposeOp, "superpose");

impl SuperposeOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for SuperposeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SuperposeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("superpose expects single expression as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let expr  = TryInto::<&ExpressionAtom>::try_into(atom).map_err(|_| arg_error())?;

        if expr.children().is_empty() {
            Ok(vec![EMPTY_SYMBOL])
        } else {
            let mut superposed = Vec::new();
            for atom in expr.children() {
                match interpret_no_error(self.space.clone(), atom) {
                    Ok(results) => { superposed.extend(results); },
                    Err(message) => { return Err(format!("Error: {}", message).into()) },
                }
            }
            Ok(superposed)
        }
    }
}

#[derive(Clone, Debug)]
pub struct CollapseOp {
    space: DynSpace,
}

grounded_op!(CollapseOp, "collapse");

impl CollapseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CollapseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CollapseOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("collapse expects single executable atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        // TODO: Calling interpreter inside the operation is not too good
        // Could it be done via returning atom for the further interpretation?
        let result = interpret_no_error(self.space.clone(), atom)?;

        Ok(vec![Atom::expr(result)])
    }
}

#[derive(Clone, Debug)]
pub struct CaptureOp {
    space: DynSpace,
}

grounded_op!(CaptureOp, "capture");

impl CaptureOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CaptureOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaptureOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("capture expects one argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        interpret(self.space.clone(), &atom).map_err(|e| ExecError::from(e))
    }
}

#[derive(Clone, Debug)]
pub struct CaseOp {
    space: DynSpace,
}

grounded_op!(CaseOp, "case");

impl CaseOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for CaseOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CaseOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("case expects two arguments: atom and expression of cases");
        let cases = args.get(1).ok_or_else(arg_error)?;
        let atom = args.get(0).ok_or_else(arg_error)?;
        log::debug!("CaseOp::execute: atom: {}, cases: {:?}", atom, cases);

        let switch = |interpreted: Atom| -> Atom {
            Atom::expr([sym!("switch"), interpreted, cases.clone()])
        };

        // Interpreting argument inside CaseOp is required because otherwise `Empty` result
        // calculated inside interpreter cuts the whole branch of the interpretation. Also we
        // cannot use `unify` in a unit test because after interpreting `(chain... (chain
        // (metta (unify ...) Atom <space>)) ...)` `chain` executes `unify` and also gets
        // `Empty` even if we have `Atom` as a resulting type. It can be solved by different ways.
        // One way is to invent new type `EmptyType` (type of the `Empty` atom) and use this type
        // in a function to allow `Empty` atoms as an input. `EmptyType` type should not be
        // casted to the `%Undefined%` thus one cannot pass `Empty` to the function which accepts
        // `%Undefined%`. Another way is to introduce "call" level. Thus if function called
        // returned the result to the `chain` it should stop reducing it and insert it into the
        // last argument.
        let results = interpret(self.space.clone(), atom);
        log::debug!("CaseOp::execute: atom results: {:?}", results);
        let results = match results {
            Ok(results) if results.is_empty() =>
                vec![switch(EMPTY_SYMBOL)],
            Ok(results) =>
                results.into_iter().map(|atom| switch(atom)).collect(),
            Err(err) => vec![Atom::expr([ERROR_SYMBOL, atom.clone(), Atom::sym(err)])],
        };
        Ok(results)
    }
}

#[derive(Clone, Debug)]
pub struct PragmaOp {
    settings: Shared<HashMap<String, Atom>>,
}

grounded_op!(PragmaOp, "pragma!");

impl PragmaOp {
    pub fn new(settings: Shared<HashMap<String, Atom>>) -> Self {
        Self{ settings }
    }
}

impl Grounded for PragmaOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PragmaOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "pragma! expects symbol atom as a key")?.name();
        let value = args.get(1).ok_or_else(arg_error)?;
        self.settings.borrow_mut().insert(key.into(), value.clone());
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct ImportOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static>>>>>>,
}

grounded_op!(ImportOp, "import!");

impl ImportOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Grounded for ImportOp {
    fn type_(&self) -> Atom {
        //TODO: Ideally the "import as" / "import into" part would be optional
        //A deeper discussion on arg semantics as it relates to import! is here:
        // https://github.com/trueagi-io/hyperon-experimental/pull/580#discussion_r1491332304
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for ImportOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        //QUESTION: "Import" can mean several (3) different things.  In Python parlance, it can mean
        //1. "import module" opt. ("as bar")
        //2. "from module import foo" opt. ("as bar")
        //3. "from module import *"
        //
        //Do we want one MeTTa operation with multiple ways of invoking it?  Or do we want different
        // implementations for different versions of the import operation?  (since we don't have key-words)
        // like "from" and "as" (unless we want to add them)
        //
        //The old version of this operation supported 1. or 3., depending on whether a "space" argument
        // mapped to an atom that already existed or not.  If the space atom existed and was a Space, then
        // the operation would perform behavior 3 (by importing only the space atom and no token).
        // Otherwise it would perform behavior 1, by adding a token, but not adding the child space atom to
        // the parent space.
        //
        //For now, in order to not lose functionality, I have kept this behavior.
        //
        // ** TO SUMMARIZE **
        // If the destination argument is the &self Space atom, the behavior is (3) ie "from module import *",
        // and if the destination argument is a Symbol atom, the behavior is (1) ie "import module as foo"
        //
        //The Underlying functionality for behavior 2 exists in MettaMod::import_item_from_dependency_as,
        //  but it isn't called yet because I wanted to discuss the way to expose it as a MeTTa op.
        //For behavior 3, there are deeper questions about desired behavior around tokenizer entries,
        //  transitive imports, etc.  I have summarized those concerns in the discussion comments above
        //  MettaMod::import_all_from_dependency
        //

        let arg_error = || ExecError::from("import! expects a destination &space and a module name argument");
        let dest_arg = args.get(0).ok_or_else(arg_error)?;
        let mod_name_atom = args.get(1).ok_or_else(arg_error)?;

        // TODO: replace Symbol by grounded String?
        let mod_name = match mod_name_atom {
            Atom::Symbol(mod_name) => mod_name.name(),
            _ => return Err("import! expects a module name as the first argument".into())
        };
        let mod_name = strip_quotes(mod_name);

        // Load the module into the runner, or get the ModId if it's already loaded
        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let mod_id = context.load_module(mod_name)?;

        // Import the module, as per the behavior described above
        match dest_arg {
            Atom::Symbol(dest_sym) => {
                context.import_dependency_as(mod_id, Some(dest_sym.name().to_string()))?;
            }
            other_atom => {
                match &other_atom {
                    Atom::Grounded(_) if Atom::as_gnd::<DynSpace>(other_atom) == Some(context.module().space()) => {
                        context.import_all_from_dependency(mod_id)?;
                    },
                    _ => {
                        return Err(format!("import! destination argument must be a symbol atom naming a new space, or &self.  Found: {other_atom:?}").into());
                    }
                }
            }
            // None => {
            //     //TODO: Currently this pattern is unreachable on account of arity-checking in the MeTTa
            //     // interpreter, but I have the code path in here for when it is possible
            //     context.module().import_dependency_as(&context.metta, mod_id, None)?;
            // },
        }

        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct IncludeOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static>>>>>>,
}

grounded_op!(IncludeOp, "include");

impl IncludeOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Grounded for IncludeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IncludeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("include expects a module name argument");
        let mod_name_atom = args.get(0).ok_or_else(arg_error)?;

        // TODO: replace Symbol by grounded String?
        let mod_name = match mod_name_atom {
            Atom::Symbol(mod_name) => mod_name.name(),
            _ => return Err(arg_error())
        };
        let mod_name = strip_quotes(mod_name);

        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let program_buf = context.load_resource_from_module(mod_name, ResourceKey::MainMettaSrc)?;

        // Interpret the loaded MeTTa S-Expression text
        let program_text = String::from_utf8(program_buf)
            .map_err(|e| e.to_string())?;
        let parser = crate::metta::text::OwnedSExprParser::new(program_text);
        let eval_result = context.run_inline(|context| {
            context.push_parser(Box::new(parser));
            Ok(())
        })?;

        //NOTE: Current behavior returns the result of the last sub-eval to match the old
        // `import!` before before module isolation.  However that means the results prior to
        // the last are dropped.  I don't know how to fix this or if it's even wrong, but it's
        // different from the way "eval-type" APIs work when called from host code, e.g. Rust
        Ok(eval_result.into_iter().last().unwrap_or_else(|| vec![]))
    }
}

#[derive(Clone, Debug)]
pub struct BindOp {
    tokenizer: Shared<Tokenizer>,
}

grounded_op!(BindOp, "bind!");

impl BindOp {
    pub fn new(tokenizer: Shared<Tokenizer>) -> Self {
        Self{ tokenizer }
    }
}

impl Grounded for BindOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for BindOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("bind! expects two arguments: token and atom");
        let token = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "bind! expects symbol atom as a token")?.name();
        let atom = args.get(1).ok_or_else(arg_error)?.clone();

        let token_regex = Regex::new(token).map_err(|err| format!("Could convert token {} into regex: {}", token, err))?;
        self.tokenizer.borrow_mut().register_token(token_regex, move |_| { atom.clone() });
        unit_result()
    }
}

/// Implement trace! built-in.
///
/// It is equivalent to Idris or Haskell Trace, that is, it prints a
/// message to stderr and pass a value along.
///
/// For instance
/// ```metta
/// !(trace! "Here?" 42)
/// ```
/// prints to stderr
/// ```stderr
/// Here?
/// ```
/// and returns
/// ```metta
/// [42]
/// ```
///
/// Note that the first argument does not need to be a string, which
/// makes `trace!` actually quite capable on its own.  For instance
/// ```metta
/// !(trace! ("Hello world!" (if True A B) 1 2 3) 42)
/// ```
/// prints to stderr
/// ```stderr
/// (Hello world! A 1 2 3)
/// ```
/// and returns
/// ```metta
/// [42]
/// ```

#[derive(Clone, Debug)]
pub struct TraceOp {}

grounded_op!(TraceOp, "trace!");

impl Grounded for TraceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, Atom::var("a"), Atom::var("a")])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for TraceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("trace! expects two atoms as arguments");
        let val = args.get(1).ok_or_else(arg_error)?;
        let msg = args.get(0).ok_or_else(arg_error)?;
        eprintln!("{}", msg);
        Ok(vec![val.clone()])
    }
}

#[derive(Clone, Debug)]
pub struct PrintlnOp {}

grounded_op!(PrintlnOp, "println!");

impl Grounded for PrintlnOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PrintlnOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("println! expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        println!("{}", atom_to_string(atom));
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct FormatArgsOp {}

grounded_op!(FormatArgsOp, "format-args");

use dyn_fmt::AsStrFormatExt;

impl Grounded for FormatArgsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FormatArgsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("format-args expects format string as a first argument and expression as a second argument");
        let format = atom_to_string(args.get(0).ok_or_else(arg_error)?);
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        let res = format.format(args.as_slice());
        Ok(vec![Atom::gnd(Str::from_string(res))])
    }
}

#[derive(Clone, Debug)]
pub struct PrintAlternativesOp {}

grounded_op!(PrintAlternativesOp, "print-alternatives!");

impl Grounded for PrintAlternativesOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PrintAlternativesOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("print-alternatives! expects format string as a first argument and expression as a second argument");
        let atom = atom_to_string(args.get(0).ok_or_else(arg_error)?);
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        println!("{} {}:", args.len(), atom);
        args.iter().for_each(|arg| println!("    {}", arg));
        Ok(vec![UNIT_ATOM])
    }
}

#[derive(Clone, Debug)]
pub struct SealedOp {}

grounded_op!(SealedOp, "sealed");

impl Grounded for SealedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SealedOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("sealed expects two arguments: var_list and expression");

        let mut term_to_seal = args.get(1).ok_or_else(arg_error)?.clone();
        let var_list = args.get(0).ok_or_else(arg_error)?.clone();

        let mut local_var_mapper = CachingMapper::new(|var: &VariableAtom| var.clone().make_unique());

        var_list.iter().filter_type::<&VariableAtom>()
            .for_each(|var| { let _ = local_var_mapper.replace(var); });

        term_to_seal.iter_mut().filter_type::<&mut VariableAtom>()
            .for_each(|var| match local_var_mapper.mapping().get(var) {
                Some(v) => *var = v.clone(),
                None => {},
            });

        let result = vec![term_to_seal.clone()];
        log::debug!("sealed::execute: var_list: {}, term_to_seal: {}, result: {:?}", var_list, term_to_seal, result);

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::common::test_utils::metta_space;
    use crate::metta::runner::stdlib_minimal::tests::run_program;
    use std::convert::TryFrom;
    use regex::Regex;

    fn assert_runtime_error(actual: Result<Vec<Atom>, ExecError>, expected: Regex) {
        match actual {
            Err(ExecError::Runtime(msg)) => assert!(expected.is_match(msg.as_str()),
                                                    "Incorrect error message:\nexpected: {:?}\n  actual: {:?}", expected.to_string(), msg),
            _ => assert!(false, "Error is expected as result, {:?} returned", actual),
        }
    }

    #[test]
    fn metta_assert_equal_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertEqualOp::new(metta.space().clone());
        let program = "
            (= (foo $x) $x)
            (= (bar $x) $x)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
        ]));
    }

    #[test]
    fn metta_assert_equal_to_result_op() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let assert = AssertEqualToResultOp::new(metta.space().clone());
        let program = "
            (= (foo) A)
            (= (foo) B)
            (= (bar) C)
            (= (baz) D)
            (= (baz) D)
        ";
        assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (foo) (A B))")), Ok(vec![
            vec![UNIT_ATOM],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed result: A")],
        ]));
        assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
            vec![expr!("Error" ({assert.clone()} ("baz") ("D")) "\nExpected: [D]\nGot: [D, D]\nExcessive result: D")]
        ]));
    }

    #[test]
    fn bind_new_space_op() {
        let tokenizer = Shared::new(Tokenizer::new());

        let bind_op = BindOp::new(tokenizer.clone());

        assert_eq!(bind_op.execute(&mut vec![sym!("&my"), sym!("definition")]), unit_result());
        let borrowed = tokenizer.borrow();
        let constr = borrowed.find_token("&my");
        assert!(constr.is_some());
        assert_eq!(constr.unwrap()("&my"), Ok(sym!("definition")));
    }

    #[test]
    fn trace_op() {
        assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
                   Ok(vec![sym!("42")]));
    }

    #[test]
    fn println_op() {
        assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), unit_result());
    }

    #[test]
    fn sealed_op_execute() {
        let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
        assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
    }

    #[test]
    fn assert_equal_op() {
        let space = DynSpace::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
            (= (bar) (B C))
            (= (bar) (A B))
            (= (err) (A B))
        "));

        let assert_equal_op = AssertEqualOp::new(space);

        assert_eq!(assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]), unit_result());

        let actual = assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("err"))]);
        let expected = Regex::new("\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)").unwrap();
        assert_runtime_error(actual, expected);

        let actual = assert_equal_op.execute(&mut vec![expr!(("err")), expr!(("foo"))]);
        let expected = Regex::new("\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)").unwrap();
        assert_runtime_error(actual, expected);
    }

    #[test]
    fn assert_equal_to_result_op() {
        let space = DynSpace::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
        "));
        let assert_equal_to_result_op = AssertEqualToResultOp::new(space);

        assert_eq!(assert_equal_to_result_op.execute(&mut vec![
            expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
                   unit_result());
    }

    #[test]
    fn metta_superpose() {
        assert_eq_metta_results!(run_program("!(superpose (red yellow green))"),
            Ok(vec![vec![expr!("red"), expr!("yellow"), expr!("green")]]));
        let program = "
            (= (foo) FOO)
            (= (bar) BAR)
            !(superpose ((foo) (bar) BAZ))
        ";
        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("FOO"), expr!("BAR"), expr!("BAZ")]]));
    }

    #[test]
    fn metta_collapse() {
        let program = "
            (= (color) red)
            (= (color) green)
            (= (color) blue)
            !(collapse (color))
        ";
        let result = run_program(program).expect("Successful result is expected");
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        assert_eq!(result.len(), 1);
        let result = result.get(0).unwrap();
        let actual = <&ExpressionAtom>::try_from(result)
            .expect("Expression atom is expected").children();
        assert_eq_no_order!(actual, vec![expr!("red"), expr!("green"), expr!("blue")]);
    }

    #[test]
    fn metta_case_empty() {
        let result = run_program("!(case Empty ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (C B) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("ok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok nok) ( (ok ok) (nok nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
        let result = run_program("!(case (unify (B C) (C B) ok Empty) ( (ok ok) (Empty nok) ))");
        assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
    }

    #[test]
    fn test_pragma_interpreter_bare_minimal() {
        let program = "
            (= (bar) baz)
            (= (foo) (bar))
            !(foo)
            !(pragma! interpreter bare-minimal)
            !(foo)
            !(eval (foo))
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![
                vec![expr!("baz")],
                vec![UNIT_ATOM],
                vec![expr!(("foo"))],
                vec![expr!(("bar"))],
            ]));
    }

    #[test]
    fn sealed_op_runner() {
        let nested = run_program("!(sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b)))))");
        let simple_replace = run_program("!(sealed ($x $y) (quote (= ($y $z))))");

        assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("quote" ("=" (a b c) (z)))));
        assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("quote" ("=" (y z)))));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
        assert_eq!(run_program("!(let $x (input $y) (output $x))"), Ok(vec![vec![expr!("output" ("input" y))]]));
        assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
    }
}