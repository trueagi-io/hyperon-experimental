use crate::*;
use crate::matcher::MatchResultIter;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, RunContext, ModuleLoader};
use crate::metta::types::{get_atom_types, get_meta_type};
use crate::common::shared::Shared;
use crate::common::CachingMapper;

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Display;
use std::collections::HashMap;
use regex::Regex;

use super::arithmetics::*;

pub const VOID_SYMBOL : Atom = sym!("%void%");

fn unit_result() -> Result<Vec<Atom>, ExecError> {
    Ok(vec![UNIT_ATOM()])
}

#[derive(Clone, Debug)]
pub struct ImportOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
}

impl PartialEq for ImportOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl ImportOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Display for ImportOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import!")
    }
}

impl Grounded for ImportOp {
    fn type_(&self) -> Atom {
        //TODO: Ideally the "import as" / "import into" part would be optional
        //A deeper discussion on arg semantics as it relates to import! is here:
        // https://github.com/trueagi-io/hyperon-experimental/pull/580#discussion_r1491332304
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

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
                context.module().import_dependency_as(&context.metta, mod_id, Some(dest_sym.name().to_string()))?;
            }
            other_atom => {
                match &other_atom {
                    Atom::Grounded(_) if Atom::as_gnd::<DynSpace>(other_atom) == Some(context.module().space()) => {
                        context.module().import_all_from_dependency(&context.metta, mod_id)?;
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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// A utility function to return the part of a string in between starting and ending quotes
// TODO: Roll this into a stdlib grounded string module, maybe as a test case for
//   https://github.com/trueagi-io/hyperon-experimental/issues/351
fn strip_quotes(src: &str) -> &str {
    if let Some(first) = src.chars().next() {
        if first == '"' {
            if let Some(last) = src.chars().last() {
                if last == '"' {
                    if src.len() > 1 {
                        return &src[1..src.len()-1]
                    }
                }
            }
        }
    }
    src
}

#[derive(Clone, Debug)]
pub struct IncludeOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
}

impl PartialEq for IncludeOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl IncludeOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Display for IncludeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "include")
    }
}

impl Grounded for IncludeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

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
        let program_buf = context.load_resource_from_module(mod_name, "module.metta")?;

        // Interpret the loaded MeTTa S-Expression text
        let program_text = String::from_utf8(program_buf)
            .map_err(|e| e.to_string())?;
        let parser = crate::metta::text::OwnedSExprParser::new(program_text);
        context.push_parser(Box::new(parser));

        unit_result()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// mod-space! returns the space of a specified module, loading the module if it's not loaded already
//NOTE: The "impure" '!' denoted in the op atom name is due to the side effect of loading the module.  If
// we want a side-effect-free version, it could be implemented by calling `RunContext::get_module_by_name`
// instead of `RunContext::load_module`, but then the user would need to use `register-module!`, `import!`,
// or some other mechanism to make sure the module is loaded in advance.
#[derive(Clone, Debug)]
pub struct ModSpaceOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
}

impl PartialEq for ModSpaceOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl ModSpaceOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Display for ModSpaceOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod-space!")
    }
}

impl Grounded for ModSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "mod-space! expects a module name argument";
        let mod_name_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;

        // TODO: replace Symbol by grounded String?
        let mod_name = match mod_name_atom {
            Atom::Symbol(mod_name) => mod_name.name(),
            _ => {return Err(ExecError::from(arg_error))}
        };
        let mod_name = strip_quotes(mod_name);

        // Load the module into the runner, or get the ModId if it's already loaded
        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let mod_id = context.load_module(mod_name)?;

        let space = Atom::gnd(context.metta().module_space(mod_id));
        Ok(vec![space])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// Provides a way to access [Metta::load_module_at_path] from within MeTTa code
#[derive(Clone, Debug)]
pub struct RegisterModuleOp {
    metta: Metta
}

impl PartialEq for RegisterModuleOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl RegisterModuleOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Display for RegisterModuleOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "register-module!")
    }
}

impl Grounded for RegisterModuleOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "register-module! expects a file system path; use quotes if needed";
        let path_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;

        // TODO: replace Symbol by grounded String?
        let path = match path_arg_atom {
            Atom::Symbol(path_arg) => path_arg.name(),
            _ => return Err(arg_error.into())
        };
        let path = strip_quotes(path);
        let path = std::path::PathBuf::from(path);

        // Load the module from the path
        // QUESTION: Do we want to expose the ability to give the module a different name and/ or
        // load it into a different part of the namespace hierarchy?  For now I was just thinking
        // it is better to keep the args simple.  IMO this is a place for optional var-args when we
        // decide on the best way to handle them language-wide.
        self.metta.load_module_at_path(path, None).map_err(|e| ExecError::from(e))?;

        unit_result()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

/// This operation prints the modules loaded from the top of the runner
///
/// NOTE: This is a temporary stop-gap to help MeTTa users inspect which modules they have loaded and
/// debug module import issues.  Ultimately it probably makes sense to make this information accessible
/// as a special kind of Space, so that it would be possible to work with it programmatically.
#[derive(Clone, Debug)]
pub struct PrintModsOp {
    metta: Metta
}

impl PartialEq for PrintModsOp {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl PrintModsOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Display for PrintModsOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "print-mods!")
    }
}

impl Grounded for PrintModsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE()])
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        self.metta.display_loaded_modules();
        unit_result()
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

impl Grounded for BindOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("bind! expects two arguments: token and atom");
        let token = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "bind! expects symbol atom as a token")?.name();
        let atom = args.get(1).ok_or_else(arg_error)?.clone();

        let token_regex = Regex::new(token).map_err(|err| format!("Could convert token {} into regex: {}", token, err))?;
        self.tokenizer.borrow_mut().register_token(token_regex, move |_| { atom.clone() });
        unit_result()
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
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::gnd(DynSpace::new(GroundingSpace::new()));
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
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("add-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("add-atom expects a space as the first argument")?;
        space.borrow_mut().add(atom.clone());
        unit_result()
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
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("remove-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("remove-atom expects a space as the first argument")?;
        space.borrow_mut().remove(atom);
        // TODO? Is it necessary to distinguish whether the atom was removed or not?
        unit_result()
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
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-atoms expects one argument: space");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-atoms expects a space as its argument")?;
        space.borrow().as_space().atom_iter()
            .map(|iter| iter.cloned().map(|a| make_variables_unique(a)).collect())
            .ok_or(ExecError::Runtime("Unsupported Operation. Can't traverse atoms in this space".to_string()))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PragmaOp {
    settings: Shared<HashMap<String, Atom>>,
}

impl PragmaOp {
    pub fn new(settings: Shared<HashMap<String, Atom>>) -> Self {
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

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("pragma! expects key and value as arguments");
        let key = <&SymbolAtom>::try_from(args.get(0).ok_or_else(arg_error)?).map_err(|_| "pragma! expects symbol atom as a key")?.name();
        let value = args.get(1).ok_or_else(arg_error)?;
        self.settings.borrow_mut().insert(key.into(), value.clone());
        unit_result()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct GetTypeOp {
    space: DynSpace,
}

impl GetTypeOp {
    pub fn new(space: DynSpace) -> Self {
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

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        Ok(get_atom_types(self.space.borrow().as_space(), atom))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct GetMetaTypeOp { }

impl Display for GetMetaTypeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-metatype")
    }
}

impl Grounded for GetMetaTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-metatype expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        Ok(vec![get_meta_type(&atom)])
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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE()])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("println! expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        println!("{}", atom);
        unit_result()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
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

#[derive(Clone, PartialEq, Debug)]
pub struct TraceOp {}

impl Display for TraceOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "trace!")
    }
}

impl Grounded for TraceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, Atom::var("a"), Atom::var("a")])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("trace! expects two atoms as arguments");
        let val = args.get(1).ok_or_else(arg_error)?;
        let msg = args.get(0).ok_or_else(arg_error)?;
        eprintln!("{}", msg);
        Ok(vec![val.clone()])
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

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        unit_result()
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StateAtom {
    state: Rc<RefCell<Atom>>
}

impl StateAtom {
    pub fn new(atom: Atom) -> Self {
        Self{ state: Rc::new(RefCell::new(atom)) }
    }
}

impl Display for StateAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(State {})", self.state.borrow())
    }
}

impl Grounded for StateAtom {
    fn type_(&self) -> Atom {
        // TODO? Wrap metatypes for non-grounded atoms
        // rust_type_atom::<StateAtom>() instead of StateMonad symbol might be used
        let atom = &*self.state.borrow();
        let typ = match atom {
            Atom::Symbol(_) => ATOM_TYPE_SYMBOL,
            Atom::Expression(_) => ATOM_TYPE_EXPRESSION,
            Atom::Variable(_) => ATOM_TYPE_VARIABLE,
            Atom::Grounded(a) => a.type_(),
        };
        Atom::expr([expr!("StateMonad"), typ])
    }

    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        // Different state atoms with equal states are equal
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NewStateOp { }

impl Display for NewStateOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "new-state")
    }
}

impl Grounded for NewStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(tnso), expr!("StateMonad" tnso)])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "new-state expects single atom as an argument";
        let atom = args.get(0).ok_or(arg_error)?;
        Ok(vec![Atom::gnd(StateAtom::new(atom.clone()))])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct GetStateOp { }

impl Grounded for GetStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tgso), expr!(tgso)])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "get-state expects single state atom as an argument";
        let state = args.get(0).ok_or(arg_error)?;
        let atom = Atom::as_gnd::<StateAtom>(state).ok_or(arg_error)?;
        Ok(vec![atom.state.borrow().clone()])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

impl Display for GetStateOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "get-state")
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ChangeStateOp { }

impl Display for ChangeStateOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "change-state!")
    }
}

impl Grounded for ChangeStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tcso), expr!(tcso), expr!("StateMonad" tcso)])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "change-state! expects a state atom and its new value as arguments";
        let atom = args.get(0).ok_or(arg_error)?;
        let state = Atom::as_gnd::<StateAtom>(atom).ok_or("change-state! expects a state as the first argument")?;
        let new_value = args.get(1).ok_or(arg_error)?;
        *state.state.borrow_mut() = new_value.clone();
        Ok(vec![atom.clone()])
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct SealedOp {}

impl Display for SealedOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sealed")
    }
}

impl Grounded for SealedOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

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

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct EqualOp {}

impl Display for EqualOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "==")
    }
}

impl Grounded for EqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(t), expr!(t), ATOM_TYPE_BOOL])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from(concat!(stringify!($op), " expects two arguments"));
        let a = args.get(0).ok_or_else(arg_error)?;
        let b = args.get(1).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(a == b))])
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
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::debug!("MatchOp::execute: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("match expects a space as the first argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }

    fn match_(&self, other: &Atom) -> MatchResultIter {
        match_by_equality(self, other)
    }
}


/// The internal `non_minimal_only_stdlib` module contains code that is never used by the minimal stdlib
#[cfg(not(feature = "minimal"))]
mod non_minimal_only_stdlib {
    use super::*;
    use crate::metta::interpreter::interpret;
    use crate::common::assert::vec_eq_no_order;

    // TODO: move it into hyperon::atom module?
    pub(crate) fn atom_as_expr(atom: &Atom) -> Option<&ExpressionAtom> {
        match atom {
            Atom::Expression(expr) => Some(expr),
            _ => None,
        }
    }

    // TODO: remove hiding errors completely after making it possible passing
    // them to the user
    fn interpret_no_error(space: DynSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
        let result = interpret(space, expr);
        log::debug!("interpret_no_error: interpretation expr: {}, result {:?}", expr, result);
        match result {
            Ok(result) => Ok(result),
            Err(_) => Ok(vec![]),
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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = || ExecError::from("cdr-atom expects one argument: expression");
            let expr = args.get(0).ok_or_else(arg_error)?;
            let chld = atom_as_expr(expr).ok_or_else(arg_error)?.children();
            if chld.len() == 0 {
                Err(ExecError::Runtime("cdr-atom expects non-empty expression".into()))
            } else {
                let cdr = Vec::from_iter(chld[1..].iter().cloned());
                Ok(vec![Atom::expr(cdr)])
            }
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

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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

    use std::collections::HashSet;

    #[derive(Clone, PartialEq, Debug)]
    pub struct CaseOp {
        space: DynSpace,
    }

    impl CaseOp {
        pub fn new(space: DynSpace) -> Self {
            Self{ space }
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = || ExecError::from("case expects two arguments: atom and expression of cases");
            let cases = args.get(1).ok_or_else(arg_error)?;
            let atom = args.get(0).ok_or_else(arg_error)?;
            let cases = CaseOp::parse_cases(atom, cases.clone())?;
            log::debug!("CaseOp::execute: atom: {}, cases: {:?}", atom, cases);

            let result = interpret_no_error(self.space.clone(), &atom);
            log::debug!("CaseOp::execute: interpretation result {:?}", result);

            match result {
                Ok(result) if result.is_empty() => {
                    cases.into_iter()
                        .find_map(|(pattern, template, _external_vars)| {
                            if pattern == VOID_SYMBOL {
                                Some(template)
                            } else {
                                None
                            }
                        })
                        .map_or(Ok(vec![]), |result| Ok(vec![result]))
                },
                Ok(result) => {
                    let triggered = result.into_iter()
                        .flat_map(|atom| CaseOp::return_first_matched(&atom, &cases))
                        .collect();
                    Ok(triggered)
                },
                Err(message) => Err(format!("Error: {}", message).into()),
            }
        }

        fn parse_cases(atom: &Atom, cases: Atom) -> Result<Vec<(Atom, Atom, HashSet<VariableAtom>)>, ExecError> {
            let cases = match cases {
                Atom::Expression(expr) => Ok(expr),
                _ => Err("case expects expression of cases as a second argument"),
            }?;

            let mut atom_vars = HashSet::new();
            collect_vars(&atom, &mut atom_vars);

            let mut result = Vec::new();
            for next_case in cases.into_children() {
                let mut next_case = match next_case {
                    Atom::Expression(next_case) if next_case.children().len() == 2 => Ok(next_case.into_children()),
                    _ => Err("case expects expression of pairs as a second argument"),
                }?;
                let mut template = next_case.pop().unwrap();
                let mut pattern = next_case.pop().unwrap();

                let mut external_vars = atom_vars.clone();
                collect_vars(&template, &mut external_vars);
                make_conflicting_vars_unique(&mut pattern, &mut template, &external_vars);

                result.push((pattern, template, external_vars));
            }

            Ok(result)
        }

        fn return_first_matched(atom: &Atom, cases: &Vec<(Atom, Atom, HashSet<VariableAtom>)>) -> Vec<Atom> {
            for (pattern, template, external_vars) in cases {
                let bindings = matcher::match_atoms(atom, &pattern)
                    .map(|b| b.convert_var_equalities_to_bindings(&external_vars));
                let result: Vec<Atom> = bindings.map(|b| matcher::apply_bindings_to_atom(&template, &b)).collect();
                if !result.is_empty() {
                    return result
                }
            }
            return vec![]
        }
    }

    impl Display for CaseOp {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "case")
        }
    }

    impl Grounded for CaseOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            CaseOp::execute(self, args)
        }

        fn match_(&self, other: &Atom) -> MatchResultIter {
            match_by_equality(self, other)
        }
    }

    fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
        log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
        let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
        match vec_eq_no_order(actual.iter(), expected.iter()) {
            Ok(()) => unit_result(),
            Err(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
        }
    }

    #[derive(Clone, PartialEq, Debug)]
    pub struct AssertEqualOp {
        space: DynSpace,
    }

    impl AssertEqualOp {
        pub fn new(space: DynSpace) -> Self {
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

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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
        space: DynSpace,
    }

    impl AssertEqualToResultOp {
        pub fn new(space: DynSpace) -> Self {
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

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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
        space: DynSpace,
    }

    impl CollapseOp {
        pub fn new(space: DynSpace) -> Self {
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

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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
    pub struct SuperposeOp {
        pub(crate) space: DynSpace,
    }

    impl SuperposeOp {
        pub fn new(space: DynSpace) -> Self {
            Self{ space }
        }
    }

    impl Display for SuperposeOp {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "superpose")
        }
    }

    impl Grounded for SuperposeOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = || ExecError::from("superpose expects single expression as an argument");
            let atom = args.get(0).ok_or_else(arg_error)?;
            let expr = atom_as_expr(&atom).ok_or(arg_error())?;

            let mut superposed = Vec::new();
            for atom in expr.children() {
                match interpret_no_error(self.space.clone(), atom) {
                    Ok(results) => { superposed.extend(results); },
                    Err(message) => { return Err(format!("Error: {}", message).into()) },
                }
            }
            Ok(superposed)
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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = || ExecError::from("let expects three arguments: pattern, atom and template");
            let mut template = args.get(2).ok_or_else(arg_error)?.clone();
            let atom = args.get(1).ok_or_else(arg_error)?;
            let mut pattern = args.get(0).ok_or_else(arg_error)?.clone();

            let external_vars = resolve_var_conflicts(&atom, &mut pattern, &mut template);

            let bindings = matcher::match_atoms(&pattern, &atom)
                .map(|b| b.convert_var_equalities_to_bindings(&external_vars));
            let result = bindings.map(|b| { matcher::apply_bindings_to_atom(&template, &b) }).collect();
            log::debug!("LetOp::execute: pattern: {}, atom: {}, template: {}, result: {:?}", pattern, atom, template, result);
            Ok(result)
        }

        fn match_(&self, other: &Atom) -> MatchResultIter {
            match_by_equality(self, other)
        }
    }

    fn resolve_var_conflicts(atom: &Atom, pattern: &mut Atom, template: &mut Atom) -> HashSet<VariableAtom> {
        let mut external_vars = HashSet::new();
        collect_vars(&atom, &mut external_vars);
        collect_vars(&template, &mut external_vars);
        make_conflicting_vars_unique(pattern, template, &external_vars);
        external_vars
    }

    fn collect_vars(atom: &Atom, vars: &mut HashSet<VariableAtom>) {
        atom.iter().filter_type::<&VariableAtom>().cloned().for_each(|var| { vars.insert(var); });
    }

    fn make_conflicting_vars_unique(pattern: &mut Atom, template: &mut Atom, external_vars: &HashSet<VariableAtom>) {
        let mut local_var_mapper = CachingMapper::new(VariableAtom::make_unique);

        pattern.iter_mut().filter_type::<&mut VariableAtom>()
            .filter(|var| external_vars.contains(var))
            .for_each(|var| *var = local_var_mapper.replace(var.clone()));

        template.iter_mut().filter_type::<&mut VariableAtom>()
            .for_each(|var| match local_var_mapper.mapping_mut().get(var) {
                Some(v) => *var = v.clone(),
                None => {},
            });
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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
        }

        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
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

    //TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
    // to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
    #[cfg(not(feature = "minimal"))]
    pub fn register_common_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, _space: &DynSpace, metta: &Metta) {

        let match_op = Atom::gnd(MatchOp{});
        tref.register_token(regex(r"match"), move |_| { match_op.clone() });
        let bind_op = Atom::gnd(BindOp::new(tokenizer));
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
        let println_op = Atom::gnd(PrintlnOp{});
        tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
        let trace_op = Atom::gnd(TraceOp{});
        tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
        let nop_op = Atom::gnd(NopOp{});
        tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
        let let_op = Atom::gnd(LetOp{});
        tref.register_token(regex(r"let"), move |_| { let_op.clone() });
        let let_var_op = Atom::gnd(LetVarOp{});
        tref.register_token(regex(r"let\*"), move |_| { let_var_op.clone() });
        let new_state_op = Atom::gnd(NewStateOp{});
        tref.register_token(regex(r"new-state"), move |_| { new_state_op.clone() });
        let change_state_op = Atom::gnd(ChangeStateOp{});
        tref.register_token(regex(r"change-state!"), move |_| { change_state_op.clone() });
        let get_state_op = Atom::gnd(GetStateOp{});
        tref.register_token(regex(r"get-state"), move |_| { get_state_op.clone() });
        let get_meta_type_op = Atom::gnd(GetMetaTypeOp{});
        tref.register_token(regex(r"get-metatype"), move |_| { get_meta_type_op.clone() });
        let register_module_op = Atom::gnd(RegisterModuleOp::new(metta.clone()));
        tref.register_token(regex(r"register-module!"), move |_| { register_module_op.clone() });
        let mod_space_op = Atom::gnd(ModSpaceOp::new(metta.clone()));
        tref.register_token(regex(r"mod-space!"), move |_| { mod_space_op.clone() });
        let print_mods_op = Atom::gnd(PrintModsOp::new(metta.clone()));
        tref.register_token(regex(r"print-mods!"), move |_| { print_mods_op.clone() });
        let sealed_op = Atom::gnd(SealedOp{});
        tref.register_token(regex(r"sealed"), move |_| { sealed_op.clone() });
    }

    //TODO: The metta argument is a temporary hack on account of the way the operation atoms store references
    // to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
    #[cfg(not(feature = "minimal"))]
    pub fn register_runner_tokens(tref: &mut Tokenizer, _tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {

        let case_op = Atom::gnd(CaseOp::new(space.clone()));
        tref.register_token(regex(r"case"), move |_| { case_op.clone() });
        let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
        tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
        let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
        tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
        let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
        tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
        let superpose_op = Atom::gnd(SuperposeOp::new(space.clone()));
        tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
        let get_type_op = Atom::gnd(GetTypeOp::new(space.clone()));
        tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
        let import_op = Atom::gnd(ImportOp::new(metta.clone()));
        tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
        let include_op = Atom::gnd(IncludeOp::new(metta.clone()));
        tref.register_token(regex(r"include"), move |_| { include_op.clone() });
        let pragma_op = Atom::gnd(PragmaOp::new(metta.settings().clone()));
        tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });

        // &self should be updated
        // TODO: adding &self might be done not by stdlib, but by MeTTa itself.
        // TODO: adding &self introduces self referencing and thus prevents space
        // from being freed. There are two options to eliminate this. (1) use weak
        // pointer and somehow use the same type to represent weak and strong
        // pointers to the atomspace. (2) resolve &self in GroundingSpace::query
        // method without adding it into container.
        let self_atom = Atom::gnd(space.clone());
        tref.register_token(regex(r"&self"), move |_| { self_atom.clone() });
    }

    #[cfg(not(feature = "minimal"))]
    pub fn register_rust_stdlib_tokens(target: &mut Tokenizer) {
        let mut rust_tokens = Tokenizer::new();
        let tref = &mut rust_tokens;

        tref.register_fallible_token(regex(r"[\-\+]?\d+"),
            |token| { Ok(Atom::gnd(Number::from_int_str(token)?)) });
        tref.register_fallible_token(regex(r"[\-\+]?\d+\.\d+"),
            |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
        tref.register_fallible_token(regex(r"[\-\+]?\d+(\.\d+)?[eE][\-\+]?\d+"),
            |token| { Ok(Atom::gnd(Number::from_float_str(token)?)) });
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
        let lt_op = Atom::gnd(LessOp{});
        tref.register_token(regex(r"<"), move |_| { lt_op.clone() });
        let gt_op = Atom::gnd(GreaterOp{});
        tref.register_token(regex(r">"), move |_| { gt_op.clone() });
        let le_op = Atom::gnd(LessEqOp{});
        tref.register_token(regex(r"<="), move |_| { le_op.clone() });
        let ge_op = Atom::gnd(GreaterEqOp{});
        tref.register_token(regex(r">="), move |_| { ge_op.clone() });
        let eq_op = Atom::gnd(EqualOp{});
        tref.register_token(regex(r"=="), move |_| { eq_op.clone() });
        let and_op = Atom::gnd(AndOp{});
        tref.register_token(regex(r"and"), move |_| { and_op.clone() });
        let or_op = Atom::gnd(OrOp{});
        tref.register_token(regex(r"or"), move |_| { or_op.clone() });
        let not_op = Atom::gnd(NotOp{});
        tref.register_token(regex(r"not"), move |_| { not_op.clone() });
        // NOTE: xor and flip are absent in Python intentionally for conversion testing
        let xor_op = Atom::gnd(XorOp{});
        tref.register_token(regex(r"xor"), move |_| { xor_op.clone() });
        let flip_op = Atom::gnd(FlipOp{});
        tref.register_token(regex(r"flip"), move |_| { flip_op.clone() });

        target.move_front(&mut rust_tokens);
    }

    pub static METTA_CODE: &'static str = "
        ; `$then`, `$else` should be of `Atom` type to avoid evaluation
        ; and infinite cycle in inference
        (: if (-> Bool Atom Atom $t))
        (= (if True $then $else) $then)
        (= (if False $then $else) $else)
        (: Error (-> Atom Atom ErrorType))

        (: add-reduct (-> Grounded %Undefined% (->)))
        (= (add-reduct $dst $atom)  (add-atom $dst $atom))

        ; quote prevents atom from being reduced
        (: quote (-> Atom Atom))

        ; unify matches two atoms and returns $then if they are matched
        ; and $else otherwise.
        (: unify (-> Atom Atom Atom Atom %Undefined%))
        (= (unify $a $a $then $else) $then)
        (= (unify $a $b $then $else)
        (case (unify-or-empty $a $b) ((%void%  $else))) )
        (: unify-or-empty (-> Atom Atom Atom))
        (= (unify-or-empty $a $a) unified)
        (= (unify-or-empty $a $b) (empty))


        ; empty removes current result from a non-deterministic result
        (: empty (-> %Undefined%))
        (= (empty) (let a b never-happens))
    ";
}

#[cfg(not(feature = "minimal"))]
pub use non_minimal_only_stdlib::*;

#[cfg(feature = "minimal")]
use super::stdlib_minimal::*;

#[cfg(feature = "minimal")]
use crate::metta::runner::METTA_CODE;

/// Loader to Initialize the corelib module
///
/// NOTE: the corelib will be loaded automatically if the runner is initialized with one of the high-level
/// init functions such as [Metta::new] and [Metta::new_with_stdlib_loader]
#[derive(Debug)]
pub(crate) struct CoreLibLoader;

impl Default for CoreLibLoader {
    fn default() -> Self {
        CoreLibLoader
    }
}

impl ModuleLoader for CoreLibLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        register_rust_stdlib_tokens(&mut *context.module().tokenizer().borrow_mut());

        let parser = SExprParser::new(METTA_CODE);
        context.push_parser(Box::new(parser));

        Ok(())
    }
}


#[test]
fn mod_space_op() {
    let program = r#"
        !(bind! &new_space (new-space))
        !(add-atom &new_space (mod-space! stdlib))
        !(get-atoms &new_space)
    "#;
    let runner = Metta::new(Some(runner::environment::EnvBuilder::test_env()));
    let result = runner.run(SExprParser::new(program)).unwrap();

    let stdlib_space = runner.module_space(runner.get_module_by_name("stdlib").unwrap());
    assert_eq!(result[2], vec![Atom::gnd(stdlib_space)]);
}

#[cfg(all(test, not(feature = "minimal")))]
mod tests {
    use super::*;
    use crate::atom::matcher::atoms_are_equivalent;
    use crate::metta::text::*;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::types::validate_atom;
    use crate::common::test_utils::*;


    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run(SExprParser::new(program))
    }

    #[test]
    fn match_op() {
        let space = DynSpace::new(metta_space("(A B)"));
        let match_op = MatchOp{};
        assert_eq!(match_op.execute(&mut vec![expr!({space}), expr!("A" "B"), expr!("B" "A")]),
            Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn match_op_issue_530() {
        let space = DynSpace::new(metta_space("(A $a $a)"));
        let match_op = MatchOp{};
        let result = match_op.execute(&mut vec![expr!({space}), expr!("A" x y), expr!("A" x y)]).unwrap();
        assert_eq!(result.len(), 1);
        assert!(atoms_are_equivalent(&result[0], &expr!("A" x x)),
            "atoms are not equivalent: expected: {}, actual: {}", expr!("A" x x), result[0]);
    }


    #[test]
    fn new_space_op() {
        let res = NewSpaceOp{}.execute(&mut vec![]).expect("No result returned");
        let space = res.get(0).expect("Result is empty");
        let space = space.as_gnd::<DynSpace>().expect("Result is not space");
        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
        assert_eq_no_order!(space_atoms, Vec::<Atom>::new());
    }

    #[test]
    fn add_atom_op() {
        let space = DynSpace::new(GroundingSpace::new());
        let satom = Atom::gnd(space.clone());
        let res = AddAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        assert_eq!(res, vec![UNIT_ATOM()]);
        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
        assert_eq_no_order!(space_atoms, vec![expr!(("foo" "bar"))]);
    }

    #[test]
    fn remove_atom_op() {
        let space = DynSpace::new(metta_space("
            (foo bar)
            (bar foo)
        "));
        let satom = Atom::gnd(space.clone());
        let res = RemoveAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
        // REM: can return Bool in future
        assert_eq!(res, vec![UNIT_ATOM()]);
        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
        assert_eq_no_order!(space_atoms, vec![expr!(("bar" "foo"))]);
    }

    #[test]
    fn get_atoms_op() {
        let space = DynSpace::new(metta_space("
            (foo bar)
            (bar foo)
        "));
        let satom = Atom::gnd(space.clone());
        let res = GetAtomsOp{}.execute(&mut vec![satom]).expect("No result returned");
        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
        assert_eq_no_order!(res, space_atoms);
        assert_eq_no_order!(res, vec![expr!(("foo" "bar")), expr!(("bar" "foo"))]);
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
        let res = CdrAtomOp{}.execute(&mut vec![expr!()]);
        assert_eq!(res, Err(ExecError::Runtime("cdr-atom expects non-empty expression".into())));
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

        assert_eq!(bind_op.execute(&mut vec![sym!("&my"), sym!("definition")]), unit_result());
        let borrowed = tokenizer.borrow();
        let constr = borrowed.find_token("&my");
        assert!(constr.is_some());
        assert_eq!(constr.unwrap()("&my"), Ok(sym!("definition")));
    }

    #[test]
    fn case_op() {
        let space = DynSpace::new(metta_space("
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

    #[test]
    fn case_op_external_vars_at_right_are_kept_untouched() {
        let space = DynSpace::new(GroundingSpace::new());
        let case_op = CaseOp::new(space.clone());

        assert_eq!(case_op.execute(&mut vec![expr!(ext), expr!(((t t)))]),
            Ok(vec![expr!(ext)]));
        assert_eq!(case_op.execute(&mut vec![expr!(ext "A"), expr!(((t t)))]),
            Ok(vec![expr!(ext "A")]));
    }

    #[test]
    fn case_op_internal_variables_has_priority_in_template() {
        let space = DynSpace::new(GroundingSpace::new());
        let case_op = CaseOp::new(space.clone());

        assert_eq!(case_op.execute(&mut vec![expr!(x "A"), expr!(((x x)))]),
            Ok(vec![expr!(x "A")]));
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
    fn collapse_op() {
        let space = DynSpace::new(metta_space("
            (= (foo) (A B))
            (= (foo) (B C))
        "));
        let collapse_op = CollapseOp::new(space);

        let actual = collapse_op.execute(&mut vec![expr!(("foo"))]).unwrap();
        assert_eq!(actual.len(), 1);
        assert_eq_no_order!(
            *atom_as_expr(&actual[0]).unwrap().children(),
            vec![expr!("B" "C"), expr!("A" "B")]);
    }

    #[test]
    fn superpose_op() {
        let space = DynSpace::new(GroundingSpace::new());
        let superpose_op = SuperposeOp::new(space);
        assert_eq!(superpose_op.execute(&mut vec![expr!("A" ("B" "C"))]),
            Ok(vec![sym!("A"), expr!("B" "C")]));
    }

    #[test]
    fn superpose_op_type() {
        let space = DynSpace::new(GroundingSpace::new());
        assert!(validate_atom(space.borrow().as_space(), &expr!({SumOp{}}
            ({SuperposeOp::new(space.clone())} ({Number::Integer(1)} {Number::Integer(2)} {Number::Integer(3)}))
            {Number::Integer(1)})));
    }

    #[test]
    fn superpose_op_multiple_interpretations() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            (= (f) A)
            (= (f) B)
            (= (g) C)
            (= (g) D)

            !(superpose ((f) (g)))
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![vec![expr!("A"), expr!("B"), expr!("C"), expr!("D")]]));
    }

    #[test]
    fn superpose_op_superposed_with_collapse() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            (= (f) A)
            (= (f) B)

            !(let $x (collapse (f)) (superpose $x))
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![vec![expr!("A"), expr!("B")]]));
    }

    #[test]
    fn superpose_op_consumes_interpreter_errors() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            (: f (-> A B))
            (= (f $x) $x)

            (: a A)
            (: b B)

            !(superpose ((f (superpose ())) (f a) (f b)))
        ");

        assert_eq!(metta.run(parser), Ok(vec![vec![
                expr!("Error" ("f" ({SuperposeOp{space:metta.space().clone()}} ())) "NoValidAlternatives"),
                expr!("a"), expr!("Error" "b" "BadType")]]));
    }

    #[test]
    fn get_type_op() {
        let space = DynSpace::new(metta_space("
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
    fn get_type_op_non_valid_atom() {
        let space = DynSpace::new(metta_space("
            (: f (-> Number String))
            (: 42 Number)
            (: \"test\" String)
        "));

        let get_type_op = GetTypeOp::new(space);
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42")]).unwrap(),
            vec![sym!("String")]);
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\"")]).unwrap(),
            Vec::<Atom>::new());
    }

    #[test]
    fn println_op() {
        assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), unit_result());
    }

    #[test]
    fn trace_op() {
        assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
                   Ok(vec![sym!("42")]));
    }

    #[test]
    fn nop_op() {
        assert_eq!(NopOp{}.execute(&mut vec![]), unit_result());
    }

    #[test]
    fn let_op() {
        assert_eq!(LetOp{}.execute(&mut vec![expr!(a b), expr!("A" "B"), expr!(b a)]),
            Ok(vec![expr!("B" "A")]));
    }

    #[test]
    fn let_op_external_vars_at_right_are_kept_untouched() {
        assert_eq!(LetOp{}.execute(&mut vec![expr!(t), expr!(ext), expr!(t)]),
            Ok(vec![expr!(ext)]));
        assert_eq!(LetOp{}.execute(&mut vec![expr!(t), expr!(ext "A"), expr!(t)]),
            Ok(vec![expr!(ext "A")]));
    }

    #[test]
    fn let_op_internal_variables_has_priority_in_template() {
        assert_eq!(LetOp{}.execute(&mut vec![expr!(x), expr!(x "A"), expr!(x)]),
            Ok(vec![expr!(x "A")]));
    }

    #[test]
    fn let_op_keep_variables_equalities_issue290() {
        assert_eq_metta_results!(run_program("!(let* (($f f) ($f $x)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let* (($f $x) ($f f)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let ($x $x) ($z $y) (let $y A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
        assert_eq_metta_results!(run_program("!(let ($x $x) ($z $y) (let $z A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
    }

    #[test]
    fn let_op_variables_visibility_pr262() {
        let program = "
            ;; Knowledge
            (→ P Q)
            (→ Q R)

            ;; Rule
            (= (rule (→ $p $q) (→ $q $r)) (→ $p $r))

            ;; Query (does not work as expected)
            (= (query $kb)
               (let* (($pq (→ $p $q))
                      ($qr (→ $q $r)))
                 (match $kb
                   ;; Premises
                   (, $pq $qr)
                   ;; Conclusion
                   (rule $pq $qr))))

            ;; Call
            !(query &self)
            ;; [(→ P R)]
        ";
        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("→" "P" "R")]]));
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

    #[test]
    fn state_ops() {
        let result = NewStateOp{}.execute(&mut vec![expr!("A" "B")]).unwrap();
        let old_state = result.get(0).ok_or("error").unwrap();
        assert_eq!(old_state, &Atom::gnd(StateAtom::new(expr!("A" "B"))));
        let result = ChangeStateOp{}.execute(&mut vec!(old_state.clone(), expr!("C" "D"))).unwrap();
        let new_state = result.get(0).ok_or("error").unwrap();
        assert_eq!(old_state, new_state);
        assert_eq!(new_state, &Atom::gnd(StateAtom::new(expr!("C" "D"))));
        let result = GetStateOp{}.execute(&mut vec![new_state.clone()]);
        assert_eq!(result, Ok(vec![expr!("C" "D")]))
    }

    #[test]
    fn test_stdlib_uses_rust_grounded_tokens() {
        assert_eq!(run_program("!(if True ok nok)"), Ok(vec![vec![Atom::sym("ok")]]));
    }

    #[test]
    fn test_let_op_inside_other_operation() {
        assert_eq!(run_program("!(and True (let $x False $x))"), Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn test_quote() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            (= (foo) a)
            (= (foo) b)
            !(foo)
            !(quote (foo))
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![
                vec![expr!("a"), expr!("b")],
                vec![expr!("quote" ("foo"))],
            ]));
    }

    #[test]
    fn test_unify() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            !(unify (a $b 1 (d)) (a $a 1 (d)) ok nok)
            !(unify (a $b c) (a b $c) (ok $b $c) nok)
            !(unify $a (a b c) (ok $a) nok)
            !(unify (a b c) $a (ok $a) nok)
            !(unify (a b c) (a b d) ok nok)
            !(unify ($x a) (b $x) ok nok)
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![
                vec![expr!("ok")],
                vec![expr!("ok" "b" "c")],
                vec![expr!("ok" ("a" "b" "c"))],
                vec![expr!("ok" ("a" "b" "c"))],
                vec![expr!("nok")],
                vec![expr!("nok")]
            ]));
    }

    #[test]
    fn test_empty() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new("
            !(empty)
        ");

        assert_eq_metta_results!(metta.run(parser),
            Ok(vec![vec![]]));
    }

    #[test]
    fn sealed_op_runner() {
        let nested = run_program("!(sealed ($x) (sealed ($a $b) (=($a $x $c) ($b))))");
        let simple_replace = run_program("!(sealed ($x $y) (=($y $z)))");

        assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("="(a b c) (z))));
        assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("="(y z))));
    }

    #[test]
    fn sealed_op_execute() {
        let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
        assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![expr!("output" ("input" x))]]));
        assert_eq!(run_program("!(let ($sv $st) (sealed ($x) ($x (output $x)))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
    }
}
