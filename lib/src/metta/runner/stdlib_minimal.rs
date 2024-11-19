use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::types::{get_atom_types, get_meta_type};
use crate::common::assert::vec_eq_no_order;
use crate::common::shared::Shared;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, RunContext, ModuleLoader, ResourceKey};
use crate::metta::runner::string::Str;
use crate::common::CachingMapper;
use crate::common::multitrie::MultiTrie;
use crate::space::grounding::atom_to_trie_key;
#[cfg(feature = "pkg_mgmt")]
use crate::metta::runner::{git_catalog::ModuleGitLocation, mod_name_from_url, pkg_mgmt::UpdateMode};
use crate::metta::runner::stdlib_math;

use std::convert::TryInto;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Display;
use std::collections::HashMap;
use regex::Regex;
use rand::Rng;

use super::arithmetics::*;
use super::string::*;

pub(crate) fn unit_result() -> Result<Vec<Atom>, ExecError> {
    Ok(vec![UNIT_ATOM])
}

pub(crate) fn regex(regex: &str) -> Regex {
    Regex::new(regex).unwrap()
}

macro_rules! grounded_op {
    ($name:ident, $disp:literal) => {
        impl PartialEq for $name {
            fn eq(&self, _other: &Self) -> bool {
                true
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, $disp)
            }
        }
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

/// mod-space! returns the space of a specified module, loading the module if it's not loaded already
//NOTE: The "impure" '!' denoted in the op atom name is due to the side effect of loading the module.  If
// we want a side-effect-free version, it could be implemented by calling `RunContext::get_module_by_name`
// instead of `RunContext::load_module`, but then the user would need to use `register-module!`, `import!`,
// or some other mechanism to make sure the module is loaded in advance.
#[derive(Clone, Debug)]
pub struct ModSpaceOp {
    //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static>>>>>>,
}

grounded_op!(ModSpaceOp, "mod-space!");

impl ModSpaceOp {
    pub fn new(metta: Metta) -> Self {
        Self{ context: metta.0.context.clone() }
    }
}

impl Grounded for ModSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, rust_type_atom::<DynSpace>()])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for ModSpaceOp {
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

grounded_op!(PrintModsOp, "print-mods!");

impl PrintModsOp {
    pub fn new(metta: Metta) -> Self {
        Self{ metta }
    }
}

impl Grounded for PrintModsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PrintModsOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        self.metta.display_loaded_modules();
        unit_result()
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

#[derive(Clone, Debug)]
pub struct NewSpaceOp {}

grounded_op!(NewSpaceOp, "new-space");

impl Grounded for NewSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>()])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewSpaceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 0 {
            let space = Atom::gnd(DynSpace::new(GroundingSpace::new()));
            Ok(vec![space])
        } else {
            Err("new-space doesn't expect arguments".into())
        }
    }
}

#[derive(Clone, Debug)]
pub struct AddAtomOp {}

grounded_op!(AddAtomOp, "add-atom");

impl Grounded for AddAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for AddAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("add-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("add-atom expects a space as the first argument")?;
        space.borrow_mut().add(atom.clone());
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct RemoveAtomOp {}

grounded_op!(RemoveAtomOp, "remove-atom");

impl Grounded for RemoveAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RemoveAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("remove-atom expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("remove-atom expects a space as the first argument")?;
        space.borrow_mut().remove(atom);
        // TODO? Is it necessary to distinguish whether the atom was removed or not?
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct GetAtomsOp {}

grounded_op!(GetAtomsOp, "get-atoms");

impl Grounded for GetAtomsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(),
            ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetAtomsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-atoms expects one argument: space");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-atoms expects a space as its argument")?;
        space.borrow().as_space().atom_iter()
            .map(|iter| iter.cloned().map(|a| make_variables_unique(a)).collect())
            .ok_or(ExecError::Runtime("Unsupported Operation. Can't traverse atoms in this space".to_string()))
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
pub struct GetTypeSpaceOp {}

grounded_op!(GetTypeSpaceOp, "get-type-space");

impl Grounded for GetTypeSpaceOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetTypeSpaceOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type-space expects two arguments: space and atom");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-type-space expects a space as the first argument")?;
        let atom = args.get(1).ok_or_else(arg_error)?;
        log::debug!("GetTypeSpaceOp::execute: space: {}, atom: {}", space, atom);

        Ok(get_atom_types(space, atom))
    }
}

#[derive(Clone, Debug)]
pub struct GetMetaTypeOp { }

grounded_op!(GetMetaTypeOp, "get-metatype");

impl Grounded for GetMetaTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetMetaTypeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-metatype expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;

        Ok(vec![get_meta_type(&atom)])
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
pub struct NopOp {}

grounded_op!(NopOp, "nop");

impl Grounded for NopOp {
    fn type_(&self) -> Atom {
        ATOM_TYPE_UNDEFINED
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NopOp {
    fn execute(&self, _args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        unit_result()
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
}

#[derive(Clone, Debug)]
pub struct NewStateOp { }

grounded_op!(NewStateOp, "new-state");

impl Grounded for NewStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(tnso), expr!("StateMonad" tnso)])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewStateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "new-state expects single atom as an argument";
        let atom = args.get(0).ok_or(arg_error)?;
        Ok(vec![Atom::gnd(StateAtom::new(atom.clone()))])
    }
}

#[derive(Clone, Debug)]
pub struct GetStateOp { }

grounded_op!(GetStateOp, "get-state");

impl Grounded for GetStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tgso), expr!(tgso)])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetStateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "get-state expects single state atom as an argument";
        let state = args.get(0).ok_or(arg_error)?;
        let atom = Atom::as_gnd::<StateAtom>(state).ok_or(arg_error)?;
        Ok(vec![atom.state.borrow().clone()])
    }
}

#[derive(Clone, Debug)]
pub struct ChangeStateOp { }

grounded_op!(ChangeStateOp, "change-state!");

impl Grounded for ChangeStateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!("StateMonad" tcso), expr!(tcso), expr!("StateMonad" tcso)])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for ChangeStateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = "change-state! expects a state atom and its new value as arguments";
        let atom = args.get(0).ok_or(arg_error)?;
        let state = Atom::as_gnd::<StateAtom>(atom).ok_or("change-state! expects a state as the first argument")?;
        let new_value = args.get(1).ok_or(arg_error)?;
        *state.state.borrow_mut() = new_value.clone();
        Ok(vec![atom.clone()])
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

#[derive(Clone, Debug)]
pub struct EqualOp {}

grounded_op!(EqualOp, "==");

impl Grounded for EqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, expr!(t), expr!(t), ATOM_TYPE_BOOL])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for EqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from(concat!(stringify!($op), " expects two arguments"));
        let a = args.get(0).ok_or_else(arg_error)?;
        let b = args.get(1).ok_or_else(arg_error)?;

        Ok(vec![Atom::gnd(Bool(a == b))])
    }
}

#[derive(Clone, Debug)]
pub struct MatchOp {}

grounded_op!(MatchOp, "match");

impl Grounded for MatchOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for MatchOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("match expects three arguments: space, pattern and template");
        let space = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let template = args.get(2).ok_or_else(arg_error)?;
        log::debug!("MatchOp::execute: space: {:?}, pattern: {:?}, template: {:?}", space, pattern, template);
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("match expects a space as the first argument")?;
        Ok(space.borrow().subst(&pattern, &template))
    }
}

/// The op atoms that depend on the pkg_mgmt feature
#[cfg(feature = "pkg_mgmt")]
pub(crate) mod pkg_mgmt_ops {
    use super::*;

    /// Provides a way to access [Metta::load_module_at_path] from within MeTTa code
    #[derive(Clone, Debug)]
    pub struct RegisterModuleOp {
        metta: Metta
    }

    grounded_op!(RegisterModuleOp, "register-module!");

    impl RegisterModuleOp {
        pub fn new(metta: Metta) -> Self {
            Self{ metta }
        }
    }

    impl Grounded for RegisterModuleOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE])
        }

        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for RegisterModuleOp {
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = "register-module! expects a file system path; use quotes if needed";
            let path_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;

            let path = match path_arg_atom {
                Atom::Symbol(path_arg) => path_arg.name(),
                Atom::Grounded(g) => g.downcast_ref::<Str>().ok_or_else(|| ExecError::from(arg_error))?.as_str(),
                _ => return Err(arg_error.into()),
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
    }

    /// Provides access to module in a remote git repo, from within MeTTa code
    /// Similar to `register-module!`, this op will bypass the catalog search
    ///
    /// NOTE: Even if Hyperon is build without git support, this operation may still be used to
    /// load existing modules from a git cache.  That situation may occur if modules were fetched
    /// earlier or by another tool that manages the module cache.  However this operation requres
    /// git support to actually clone or pull from a git repository.
    #[derive(Clone, Debug)]
    pub struct GitModuleOp {
        //TODO-HACK: This is a terrible horrible ugly hack that should be fixed ASAP
        context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static>>>>>>,
    }

    grounded_op!(GitModuleOp, "git-module!");

    impl GitModuleOp {
        pub fn new(metta: Metta) -> Self {
            Self{ context: metta.0.context.clone() }
        }
    }

    impl Grounded for GitModuleOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE])
        }

        fn as_execute(&self) -> Option<&dyn CustomExecute> {
            Some(self)
        }
    }

    impl CustomExecute for GitModuleOp {
        fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
            let arg_error = "git-module! expects a URL; use quotes if needed";
            let url_arg_atom = args.get(0).ok_or_else(|| ExecError::from(arg_error))?;
            // TODO: When we figure out how to address varargs, it will be nice to take an optional branch name

            let url = match url_arg_atom {
                Atom::Symbol(url_arg) => url_arg.name(),
                Atom::Grounded(g) => g.downcast_ref::<Str>().ok_or_else(|| ExecError::from(arg_error))?.as_str(),
                _ => return Err(arg_error.into()),
            };
            let url = strip_quotes(url);

            // TODO: Depending on what we do with `register-module!`, we might want to let the
            // caller provide an optional mod_name here too, rather than extracting it from the url
            let mod_name = match mod_name_from_url(url) {
                Some(mod_name) => mod_name,
                None => return Err(ExecError::from("git-module! error extracting module name from URL"))
            };

            let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
            let mut context = ctx_ref.lock().unwrap();

            let git_mod_location = ModuleGitLocation::new(url.to_string());

            match context.metta.environment().specified_mods.as_ref() {
                Some(specified_mods) => if let Some((loader, descriptor)) = specified_mods.loader_for_explicit_git_module(&mod_name, UpdateMode::TryFetchLatest, &git_mod_location)? {
                    context.get_or_init_module_with_descriptor(&mod_name, descriptor, loader).map_err(|e| ExecError::from(e))?;
                },
                None => return Err(ExecError::from(format!("Unable to pull module \"{mod_name}\" from git; no local \"caches\" directory available")))
            }

            unit_result()
        }
    }

    pub fn register_pkg_mgmt_tokens(tref: &mut Tokenizer, metta: &Metta) {
        let register_module_op = Atom::gnd(RegisterModuleOp::new(metta.clone()));
        tref.register_token(regex(r"register-module!"), move |_| { register_module_op.clone() });
        let git_module_op = Atom::gnd(GitModuleOp::new(metta.clone()));
        tref.register_token(regex(r"git-module!"), move |_| { git_module_op.clone() });
    }
}

#[derive(Clone, Debug)]
pub struct UniqueAtomOp {}

grounded_op!(UniqueAtomOp, "unique-atom");

impl Grounded for UniqueAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for UniqueAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("unique expects single expression atom as an argument");
        let expr = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?;

        let mut atoms: Vec<Atom> = expr.children().into();
        let mut set = GroundingSpace::new();
        atoms.retain(|x| {
            let not_contained = set.query(x).is_empty();
            if not_contained { set.add(x.clone()) };
            not_contained
        });
        Ok(vec![Atom::expr(atoms)])
    }
}

#[derive(Clone, Debug)]
pub struct UnionAtomOp {}

grounded_op!(UnionAtomOp, "union-atom");

impl Grounded for UnionAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for UnionAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("union expects and executable LHS and RHS atom");
        let mut lhs: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().into();
        let rhs: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().into();

        lhs.extend(rhs);

        Ok(vec![Atom::expr(lhs)])
    }
}

#[derive(Clone, Debug)]
pub struct IntersectionAtomOp {}

grounded_op!(IntersectionAtomOp, "intersection-atom");

impl Grounded for IntersectionAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IntersectionAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("intersection expects and executable LHS and RHS atom");
        let mut lhs: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().into();
        let rhs = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children();

        let mut rhs_index: MultiTrie<SymbolAtom, Vec<usize>> = MultiTrie::new();
        for (index, rhs_item) in rhs.iter().enumerate() {
            let k = atom_to_trie_key(&rhs_item);
            // FIXME this should
            // a) use a mutable value endpoint which the MultiTrie does not support atm
            // b) use a linked list, which Rust barely supports atm
            let r = rhs_index.get(&k).next();
            match r.cloned() {
                Some(bucket) => {
                    rhs_index.remove(&k, &bucket);
                    let mut nbucket = bucket;
                    nbucket.push(index);
                    let nbucket = nbucket;
                    rhs_index.insert(k, nbucket);
                }
                None => { rhs_index.insert(k, vec![index]) }
            }
        }

        lhs.retain(|candidate| {
            let k = atom_to_trie_key(candidate);
            let r = rhs_index.get(&k).next();
            match r.cloned() {
                None => { false }
                Some(bucket) => {
                    match bucket.iter().position(|item| &rhs[*item] == candidate) {
                        None => { false }
                        Some(i) => {
                            rhs_index.remove(&k, &bucket);
                            if bucket.len() > 1 {
                                let mut nbucket = bucket;
                                nbucket.remove(i);
                                rhs_index.insert(k, nbucket);
                            }
                            true
                        }
                    }
                }
            }
        });

        Ok(vec![Atom::expr(lhs)])
    }
}

#[derive(Clone, Debug)]
pub struct MaxAtomOp {}

grounded_op!(MaxAtomOp, "max-atom");

impl Grounded for MaxAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for MaxAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("max-atom expects one argument: expression");
        let children = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children();
        if children.is_empty() {
            Err(ExecError::from("Empty expression"))
        } else {
            children.into_iter().fold(Ok(f64::NEG_INFINITY), |res, x| {
                match (res, Number::from_atom(x)) {
                    (res @ Err(_), _) => res,
                    (_, None) => Err(ExecError::from("Only numbers are allowed in expression")),
                    (Ok(max), Some(x)) => Ok(f64::max(max, x.into())),
                }
            })
        }.map(|max| vec![Atom::gnd(Number::Float(max))])
    }
}

#[derive(Clone, Debug)]
pub struct MinAtomOp {}

grounded_op!(MinAtomOp, "min-atom");

impl Grounded for MinAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for MinAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("min-atom expects one argument: expression");
        let children = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children();
        if children.is_empty() {
            Err(ExecError::from("Empty expression"))
        } else {
            children.into_iter().fold(Ok(f64::INFINITY), |res, x| {
                match (res, Number::from_atom(x)) {
                    (res @ Err(_), _) => res,
                    (_, None) => Err(ExecError::from("Only numbers are allowed in expression")),
                    (Ok(min), Some(x)) => Ok(f64::min(min, x.into())),
                }
            })
        }.map(|min| vec![Atom::gnd(Number::Float(min))])
    }
}

#[derive(Clone, Debug)]
pub struct SizeAtomOp {}

grounded_op!(SizeAtomOp, "size-atom");

impl Grounded for SizeAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SizeAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("size-atom expects one argument: expression");
        let children = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children();
        let size = children.len();
        Ok(vec![Atom::gnd(Number::Integer(size as i64))])
    }
}

#[derive(Clone, Debug)]
pub struct IndexAtomOp {}

grounded_op!(IndexAtomOp, "index-atom");

impl Grounded for IndexAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_NUMBER, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IndexAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("index-atom expects two arguments: expression and atom");
        let children = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children();
        let index = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?;
        match children.get(Into::<i64>::into(index) as usize) {
            Some(atom) => Ok(vec![atom.clone()]),
            None => Err(ExecError::from("Index is out of bounds")),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SubtractionAtomOp {}

grounded_op!(SubtractionAtomOp, "subtraction-atom");

impl Grounded for SubtractionAtomOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION, ATOM_TYPE_EXPRESSION])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SubtractionAtomOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("subtraction expects and executable LHS and RHS atom");
        let mut lhs: Vec<Atom> = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().into();
        let rhs = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children();

        let mut rhs_index: MultiTrie<SymbolAtom, Vec<usize>> = MultiTrie::new();
        for (index, rhs_item) in rhs.iter().enumerate() {
            let k = atom_to_trie_key(&rhs_item);
            // FIXME this should
            // a) use a mutable value endpoint which the MultiTrie does not support atm
            // b) use a linked list, which Rust barely supports atm
            let r = rhs_index.get(&k).next();
            match r.cloned() {
                Some(bucket) => {
                    rhs_index.remove(&k, &bucket);
                    let mut nbucket = bucket;
                    nbucket.push(index);
                    let nbucket = nbucket;
                    rhs_index.insert(k, nbucket);
                }
                None => { rhs_index.insert(k, vec![index]) }
            }
        }

        lhs.retain(|candidate| {
            let k = atom_to_trie_key(candidate);
            let r = rhs_index.get(&k).next();
            match r.cloned() {
                None => { true }
                Some(bucket) => {
                    match bucket.iter().position(|item| &rhs[*item] == candidate) {
                        None => { true }
                        Some(i) => {
                            rhs_index.remove(&k, &bucket);
                            if bucket.len() > 1 {
                                let mut nbucket = bucket;
                                nbucket.remove(i);
                                rhs_index.insert(k, nbucket);
                            }
                            false
                        }
                    }
                }
            }
        });

        Ok(vec![Atom::expr(lhs)])
    }
}

//TODO: In the current version of rand it is possible for rust to hang if range end's value is too
// big. In future releases (0.9+) of rand signature of sample_single will be changed and it will be
// possible to use match construction to cover overflow and other errors. So after library will be
// upgraded RandomInt and RandomFloat codes should be altered.
// see comment https://github.com/trueagi-io/hyperon-experimental/pull/791#discussion_r1824355414
#[derive(Clone, Debug)]
pub struct RandomIntOp {}

grounded_op!(RandomIntOp, "random-int");

impl Grounded for RandomIntOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomIntOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-int expects two arguments: number (start) and number (end)");
        let start: i64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("Range is empty"));
        }
        let mut rng = rand::thread_rng();
        Ok(vec![Atom::gnd(Number::Integer(rng.gen_range(range)))])
    }
}

#[derive(Clone, Debug)]
pub struct RandomFloatOp {}

grounded_op!(RandomFloatOp, "random-float");

impl Grounded for RandomFloatOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER, ATOM_TYPE_NUMBER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for RandomFloatOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("random-float expects two arguments: number (start) and number (end)");
        let start: f64 = args.get(0).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let end: f64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("Range is empty"));
        }
        let mut rng = rand::thread_rng();
        Ok(vec![Atom::gnd(Number::Float(rng.gen_range(range)))])
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

fn atom_to_string(atom: &Atom) -> String {
    match atom {
        Atom::Grounded(gnd) if gnd.type_() == ATOM_TYPE_STRING => {
            let mut s = gnd.to_string();
            s.remove(0);
            s.pop();
            s
        },
        _ => atom.to_string(),
    }
}
#[derive(Clone, Debug)]
pub struct GetTypeOp {
    space: DynSpace,
}

grounded_op!(GetTypeOp, "get-type");

impl GetTypeOp {
    pub fn new(space: DynSpace) -> Self {
        Self{ space }
    }
}

impl Grounded for GetTypeOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetTypeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-type expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let space = match args.get(1) {
            Some(space) => Atom::as_gnd::<DynSpace>(space)
                .ok_or("match expects a space as the first argument"),
            None => Ok(&self.space),
        }?;
        let types = get_atom_types(space, atom);
        if types.is_empty() {
            Ok(vec![EMPTY_SYMBOL])
        } else {
            Ok(types)
        }
    }
}

#[derive(Clone, Debug)]
pub struct IfEqualOp { }

grounded_op!(IfEqualOp, "if-equal");

impl Grounded for IfEqualOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for IfEqualOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("if-equal expects <atom> <pattern> <then> <else> as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        let pattern = args.get(1).ok_or_else(arg_error)?;
        let then = args.get(2).ok_or_else(arg_error)?;
        let else_ = args.get(3).ok_or_else(arg_error)?;

        if crate::matcher::atoms_are_equivalent(atom, pattern) {
            Ok(vec![then.clone()])
        } else {
            Ok(vec![else_.clone()])
        }
    }
}

// TODO: remove hiding errors completely after making it possible passing
// them to the user
fn interpret_no_error(space: DynSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
    let result = interpret(space, &expr);
    log::debug!("interpret_no_error: interpretation expr: {}, result {:?}", expr, result);
    match result {
        Ok(result) => Ok(result),
        Err(_) => Ok(vec![]),
    }
}

fn interpret(space: DynSpace, expr: &Atom) -> Result<Vec<Atom>, String> {
    let expr = Atom::expr([METTA_SYMBOL, expr.clone(), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())]);
    let result = crate::metta::interpreter_minimal::interpret(space, &expr);
    result
}

fn assert_results_equal(actual: &Vec<Atom>, expected: &Vec<Atom>, atom: &Atom) -> Result<Vec<Atom>, ExecError> {
    log::debug!("assert_results_equal: actual: {:?}, expected: {:?}, actual atom: {:?}", actual, expected, atom);
    let report = format!("\nExpected: {:?}\nGot: {:?}", expected, actual);
    match vec_eq_no_order(actual.iter(), expected.iter()) {
        Ok(()) => unit_result(),
        Err(diff) => Err(ExecError::Runtime(format!("{}\n{}", report, diff)))
    }
}

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
    fn new(space: DynSpace) -> Self {
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

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
pub fn register_common_tokens(tref: &mut Tokenizer, _tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {

    let get_type_op = Atom::gnd(GetTypeOp::new(space.clone()));
    tref.register_token(regex(r"get-type"), move |_| { get_type_op.clone() });
    let get_type_space_op = Atom::gnd(GetTypeSpaceOp{});
    tref.register_token(regex(r"get-type-space"), move |_| { get_type_space_op.clone() });
    let get_meta_type_op = Atom::gnd(GetMetaTypeOp{});
    tref.register_token(regex(r"get-metatype"), move |_| { get_meta_type_op.clone() });
    let is_equivalent = Atom::gnd(IfEqualOp{});
    tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
    let new_space_op = Atom::gnd(NewSpaceOp{});
    tref.register_token(regex(r"new-space"), move |_| { new_space_op.clone() });
    let add_atom_op = Atom::gnd(AddAtomOp{});
    tref.register_token(regex(r"add-atom"), move |_| { add_atom_op.clone() });
    let remove_atom_op = Atom::gnd(RemoveAtomOp{});
    tref.register_token(regex(r"remove-atom"), move |_| { remove_atom_op.clone() });
    let get_atoms_op = Atom::gnd(GetAtomsOp{});
    tref.register_token(regex(r"get-atoms"), move |_| { get_atoms_op.clone() });
    let new_state_op = Atom::gnd(NewStateOp{});
    tref.register_token(regex(r"new-state"), move |_| { new_state_op.clone() });
    let change_state_op = Atom::gnd(ChangeStateOp{});
    tref.register_token(regex(r"change-state!"), move |_| { change_state_op.clone() });
    let get_state_op = Atom::gnd(GetStateOp{});
    tref.register_token(regex(r"get-state"), move |_| { get_state_op.clone() });
    let nop_op = Atom::gnd(NopOp{});
    tref.register_token(regex(r"nop"), move |_| { nop_op.clone() });
    let match_op = Atom::gnd(MatchOp{});
    tref.register_token(regex(r"match"), move |_| { match_op.clone() });
    let min_atom_op = Atom::gnd(MinAtomOp{});
    tref.register_token(regex(r"min-atom"), move |_| { min_atom_op.clone() });
    let max_atom_op = Atom::gnd(MaxAtomOp{});
    tref.register_token(regex(r"max-atom"), move |_| { max_atom_op.clone() });
    let size_atom_op = Atom::gnd(SizeAtomOp{});
    tref.register_token(regex(r"size-atom"), move |_| { size_atom_op.clone() });
    let index_atom_op = Atom::gnd(IndexAtomOp{});
    tref.register_token(regex(r"index-atom"), move |_| { index_atom_op.clone() });
    let random_int_op = Atom::gnd(RandomIntOp{});
    tref.register_token(regex(r"random-int"), move |_| { random_int_op.clone() });
    let random_float_op = Atom::gnd(RandomFloatOp{});
    tref.register_token(regex(r"random-float"), move |_| { random_float_op.clone() });
    let mod_space_op = Atom::gnd(ModSpaceOp::new(metta.clone()));
    tref.register_token(regex(r"mod-space!"), move |_| { mod_space_op.clone() });
    let print_mods_op = Atom::gnd(PrintModsOp::new(metta.clone()));
    tref.register_token(regex(r"print-mods!"), move |_| { print_mods_op.clone() });
    let unique_op = Atom::gnd(UniqueAtomOp{});
    tref.register_token(regex(r"unique-atom"), move |_| { unique_op.clone() });
    let subtraction_op = Atom::gnd(SubtractionAtomOp{});
    tref.register_token(regex(r"subtraction-atom"), move |_| { subtraction_op.clone() });
    let intersection_op = Atom::gnd(IntersectionAtomOp{});
    tref.register_token(regex(r"intersection-atom"), move |_| { intersection_op.clone() });
    let union_op = Atom::gnd(UnionAtomOp{});
    tref.register_token(regex(r"union-atom"), move |_| { union_op.clone() });
    let powi_math_op = Atom::gnd(stdlib_math::PowiMathOp {});
    tref.register_token(regex(r"powi-math"), move |_| { powi_math_op.clone() });
    let powf_math_op = Atom::gnd(stdlib_math::PowfMathOp {});
    tref.register_token(regex(r"powf-math"), move |_| { powf_math_op.clone() });
    let sqrt_math_op = Atom::gnd(stdlib_math::SqrtMathOp {});
    tref.register_token(regex(r"sqrt-math"), move |_| { sqrt_math_op.clone() });
    let abs_math_op = Atom::gnd(stdlib_math::AbsMathOp {});
    tref.register_token(regex(r"abs-math"), move |_| { abs_math_op.clone() });
    let log_math_op = Atom::gnd(stdlib_math::LogMathOp {});
    tref.register_token(regex(r"log-math"), move |_| { log_math_op.clone() });
    let trunc_math_op = Atom::gnd(stdlib_math::TruncMathOp {});
    tref.register_token(regex(r"trunc-math"), move |_| { trunc_math_op.clone() });
    let ceil_math_op = Atom::gnd(stdlib_math::CeilMathOp {});
    tref.register_token(regex(r"ceil-math"), move |_| { ceil_math_op.clone() });
    let floor_math_op = Atom::gnd(stdlib_math::FloorMathOp{});
    tref.register_token(regex(r"floor-math"), move |_| { floor_math_op.clone() });
    let round_math_op = Atom::gnd(stdlib_math::RoundMathOp{});
    tref.register_token(regex(r"round-math"), move |_| { round_math_op.clone() });
    let sin_math_op = Atom::gnd(stdlib_math::SinMathOp{});
    tref.register_token(regex(r"sin-math"), move |_| { sin_math_op.clone() });
    let asin_math_op = Atom::gnd(stdlib_math::AsinMathOp{});
    tref.register_token(regex(r"asin-math"), move |_| { asin_math_op.clone() });
    let cos_math_op = Atom::gnd(stdlib_math::CosMathOp{});
    tref.register_token(regex(r"cos-math"), move |_| { cos_math_op.clone() });
    let acos_math_op = Atom::gnd(stdlib_math::AcosMathOp{});
    tref.register_token(regex(r"acos-math"), move |_| { acos_math_op.clone() });
    let tan_math_op = Atom::gnd(stdlib_math::TanMathOp{});
    tref.register_token(regex(r"tan-math"), move |_| { tan_math_op.clone() });
    let atan_math_op = Atom::gnd(stdlib_math::AtanMathOp{});
    tref.register_token(regex(r"atan-math"), move |_| { atan_math_op.clone() });
    let isnan_math_op = Atom::gnd(stdlib_math::IsNanMathOp{});
    tref.register_token(regex(r"isnan-math"), move |_| { isnan_math_op.clone() });
    let isinf_math_op = Atom::gnd(stdlib_math::IsInfMathOp{});
    tref.register_token(regex(r"isinf-math"), move |_| { isinf_math_op.clone() });
    tref.register_token(regex(r"const_pi"),
                        |_| { Atom::gnd(Number::Float(std::f64::consts::PI)) });
    tref.register_token(regex(r"const_e"),
                        |_| { Atom::gnd(Number::Float(std::f64::consts::E)) });

    #[cfg(feature = "pkg_mgmt")]
    pkg_mgmt_ops::register_pkg_mgmt_tokens(tref, metta);
}

//TODO: The additional arguments are a temporary hack on account of the way the operation atoms store references
// to the runner & module state.  https://github.com/trueagi-io/hyperon-experimental/issues/410
pub fn register_runner_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, space: &DynSpace, metta: &Metta) {

    let assert_equal_op = Atom::gnd(AssertEqualOp::new(space.clone()));
    tref.register_token(regex(r"assertEqual"), move |_| { assert_equal_op.clone() });
    let assert_equal_to_result_op = Atom::gnd(AssertEqualToResultOp::new(space.clone()));
    tref.register_token(regex(r"assertEqualToResult"), move |_| { assert_equal_to_result_op.clone() });
    let superpose_op = Atom::gnd(SuperposeOp::new(space.clone()));
    tref.register_token(regex(r"superpose"), move |_| { superpose_op.clone() });
    let collapse_op = Atom::gnd(CollapseOp::new(space.clone()));
    tref.register_token(regex(r"collapse"), move |_| { collapse_op.clone() });
    let case_op = Atom::gnd(CaseOp::new(space.clone()));
    tref.register_token(regex(r"case"), move |_| { case_op.clone() });
    let capture_op = Atom::gnd(CaptureOp::new(space.clone()));
    tref.register_token(regex(r"capture"), move |_| { capture_op.clone() });
    let pragma_op = Atom::gnd(PragmaOp::new(metta.settings().clone()));
    tref.register_token(regex(r"pragma!"), move |_| { pragma_op.clone() });
    let import_op = Atom::gnd(ImportOp::new(metta.clone()));
    tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
    let include_op = Atom::gnd(IncludeOp::new(metta.clone()));
    tref.register_token(regex(r"include"), move |_| { include_op.clone() });
    let bind_op = Atom::gnd(BindOp::new(tokenizer.clone()));
    tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
    let trace_op = Atom::gnd(TraceOp{});
    tref.register_token(regex(r"trace!"), move |_| { trace_op.clone() });
    let println_op = Atom::gnd(PrintlnOp{});
    tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
    let format_args_op = Atom::gnd(FormatArgsOp{});
    tref.register_token(regex(r"format-args"), move |_| { format_args_op.clone() });
    let print_alternatives_op = Atom::gnd(PrintAlternativesOp{});
    tref.register_token(regex(r"print-alternatives!"), move |_| { print_alternatives_op.clone() });
    let sealed_op = Atom::gnd(SealedOp{});
    tref.register_token(regex(r"sealed"), move |_| { sealed_op.clone() });
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
    tref.register_token(regex(r#"(?s)^".*"$"#),
        |token| { let mut s = String::from(token); s.remove(0); s.pop(); Atom::gnd(Str::from_string(s)) });
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

pub static METTA_CODE: &'static str = include_str!("stdlib_minimal.metta");

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::text::SExprParser;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::runner::string::Str;
    use crate::matcher::atoms_are_equivalent;
    use crate::common::Operation;
    use crate::common::test_utils::metta_space;

    use std::convert::TryFrom;
    use std::fmt::Display;
    use regex::Regex;

    fn run_program(program: &str) -> Result<Vec<Vec<Atom>>, String> {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.run(SExprParser::new(program))
    }

    #[test]
    fn get_type_op() {
        let space = DynSpace::new(metta_space("
            (: B Type)
            (: C Type)
            (: A B)
            (: A C)
        "));

        let get_type_op = GetTypeOp::new(space.clone());
        assert_eq_no_order!(get_type_op.execute(&mut vec![sym!("A"), expr!({space.clone()})]).unwrap(),
            vec![sym!("B"), sym!("C")]);
    }

    #[test]
    fn get_type_op_non_valid_atom() {
        let space = DynSpace::new(metta_space("
            (: f (-> Number String))
            (: 42 Number)
            (: \"test\" String)
        "));

        let get_type_op = GetTypeOp::new(space.clone());
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42"), expr!({space.clone()})]).unwrap(),
            vec![sym!("String")]);
        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\""), expr!({space.clone()})]).unwrap(),
            vec![EMPTY_SYMBOL]);
    }


    #[test]
    fn metta_car_atom() {
        let result = run_program("!(eval (car-atom (A $b)))");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
        let result = run_program("!(eval (car-atom ($a B)))");
        assert_eq!(result, Ok(vec![vec![expr!(a)]]));
        let result = run_program("!(eval (car-atom ()))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" ()) {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
        let result = run_program("!(eval (car-atom A))");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" "A") {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
    }

    #[test]
    fn metta_cdr_atom() {
        assert_eq!(run_program(&format!("!(cdr-atom (a b c))")), Ok(vec![vec![expr!("b" "c")]]));
        assert_eq!(run_program(&format!("!(cdr-atom ($a b $c))")), Ok(vec![vec![expr!("b" c)]]));
        assert_eq!(run_program(&format!("!(cdr-atom ())")), Ok(vec![vec![expr!("Error" ("cdr-atom" ()) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
        assert_eq!(run_program(&format!("!(cdr-atom a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" "a") {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
        assert_eq!(run_program(&format!("!(cdr-atom $a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" a) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
    }

    #[test]
    fn metta_size_atom() {
        assert_eq!(run_program(&format!("!(size-atom (5 4 3 2 1))")), Ok(vec![vec![expr!({Number::Integer(5)})]]));
        assert_eq!(run_program(&format!("!(size-atom ())")), Ok(vec![vec![expr!({Number::Integer(0)})]]));
    }

    #[test]
    fn metta_min_atom() {
        assert_eq!(run_program(&format!("!(min-atom (5 4 5.5))")), Ok(vec![vec![expr!({Number::Integer(4)})]]));
        assert_eq!(run_program(&format!("!(min-atom ())")), Ok(vec![vec![expr!("Error" ({ MinAtomOp{} } ()) "Empty expression")]]));
        assert_eq!(run_program(&format!("!(min-atom (3 A B 5))")), Ok(vec![vec![expr!("Error" ({ MinAtomOp{} } ({Number::Integer(3)} "A" "B" {Number::Integer(5)})) "Only numbers are allowed in expression")]]));
    }

    #[test]
    fn metta_max_atom() {
        assert_eq!(run_program(&format!("!(max-atom (5 4 5.5))")), Ok(vec![vec![expr!({Number::Float(5.5)})]]));
        assert_eq!(run_program(&format!("!(max-atom ())")), Ok(vec![vec![expr!("Error" ({ MaxAtomOp{} } ()) "Empty expression")]]));
        assert_eq!(run_program(&format!("!(max-atom (3 A B 5))")), Ok(vec![vec![expr!("Error" ({ MaxAtomOp{} } ({Number::Integer(3)} "A" "B" {Number::Integer(5)})) "Only numbers are allowed in expression")]]));
    }

    #[test]
    fn metta_index_atom() {
        assert_eq!(run_program(&format!("!(index-atom (5 4 3 2 1) 2)")), Ok(vec![vec![expr!({Number::Integer(3)})]]));
        assert_eq!(run_program(&format!("!(index-atom (A B C D E) 5)")), Ok(vec![vec![expr!("Error" ({ IndexAtomOp{} } ("A" "B" "C" "D" "E") {Number::Integer(5)}) "Index is out of bounds")]]));
    }

    #[test]
    fn metta_random() {
        assert_eq!(run_program(&format!("!(chain (eval (random-int 0 5)) $rint (and (>= $rint 0) (< $rint 5)))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(random-int 0 0)")), Ok(vec![vec![expr!("Error" ({ RandomIntOp{} } {Number::Integer(0)} {Number::Integer(0)}) "Range is empty")]]));
        assert_eq!(run_program(&format!("!(chain (eval (random-float 0.0 5.0)) $rfloat (and (>= $rfloat 0.0) (< $rfloat 5.0)))")), Ok(vec![vec![expr!({Bool(true)})]]));
        assert_eq!(run_program(&format!("!(random-float 0 -5)")), Ok(vec![vec![expr!("Error" ({ RandomFloatOp{} } {Number::Integer(0)} {Number::Integer(-5)}) "Range is empty")]]));
    }

    #[test]
    fn metta_switch() {
        let result = run_program("!(eval (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) )))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) (($a B) ($b $a)) )))");
        assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
        let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) ((D E) (E B)) )))");
        assert_eq!(result, Ok(vec![vec![]]));
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
    fn metta_is_function() {
        let result = run_program("!(eval (is-function (-> $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(true)})]]));
        let result = run_program("!(eval (is-function (A $t)))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
        let result = run_program("!(eval (is-function %Undefined%))");
        assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
    }

    #[test]
    fn metta_type_cast() {
        assert_eq!(run_program("(: a A) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a B &self))"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast a B &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast 42 Number &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(eval (type-cast 42 %Undefined% &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a Atom &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(eval (type-cast a Symbol &self))"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(eval (type-cast 42 Grounded &self))"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
        assert_eq!(run_program("!(eval (type-cast () Expression &self))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (type-cast (a b) Expression &self))"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("!(eval (type-cast $v Variable &self))"), Ok(vec![vec![expr!(v)]]));
        assert_eq!(run_program("(: a A) (: b B) !(eval (type-cast (a b) (A B) &self))"), Ok(vec![vec![expr!("a" "b")]]));
        assert_eq!(run_program("(: a A) (: a B) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
    }

    #[test]
    fn metta_filter_atom() {
        assert_eq!(run_program("!(eval (filter-atom () $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (filter-atom (a (b) $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" ("b") c)]]));
        assert_eq!(run_program("!(eval (filter-atom (a (Error (b) \"Test error\") $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" c)]]));
    }

    #[test]
    fn metta_map_atom() {
        assert_eq!(run_program("!(eval (map-atom () $x ($x mapped)))"), Ok(vec![vec![expr!()]]));
        assert_eq!(run_program("!(eval (map-atom (a (b) $c) $x (mapped $x)))"), Ok(vec![vec![expr!(("mapped" "a") ("mapped" ("b")) ("mapped" c))]]));
    }

    #[test]
    fn metta_foldl_atom() {
        assert_eq!(run_program("!(eval (foldl-atom () 1 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({Number::Integer(1)})]]));
        assert_eq!(run_program("!(eval (foldl-atom (1 2 3) 0 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({Number::Integer(6)})]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_atom() {
        let result = run_program("!(metta A Atom &self)");
        assert_eq!(result, Ok(vec![vec![expr!("A")]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_meta_type() {
        assert_eq!(run_program("!(metta A Symbol &self)"), Ok(vec![vec![expr!("A")]]));
        assert_eq!(run_program("!(metta $x Variable &self)"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(metta (A B) Expression &self)"), Ok(vec![vec![expr!("A" "B")]]));
        assert_eq!(run_program("!(metta 42 Grounded &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_symbol_or_grounded_value_as_type() {
        assert_eq!(run_program("(: a A) !(metta a A &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("(: a A) !(metta a B &self)"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
        assert_eq!(run_program("!(metta 42 Number &self)"), Ok(vec![vec![expr!({Number::Integer(42)})]]));
    }

    #[test]
    fn metta_interpret_variable_as_type() {
        assert_eq!(run_program("!(metta $x %Undefined% &self)"), Ok(vec![vec![expr!(x)]]));
        assert_eq!(run_program("!(metta $x SomeType &self)"), Ok(vec![vec![expr!(x)]]));
    }

    #[test]
    fn metta_interpret_empty_expression_as_type() {
        assert_eq!(run_program("!(metta () %Undefined% &self)"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(metta () SomeType &self)"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_as_variable_type() {
        let result = run_program("
            (: S Int)
            !(chain (metta S $t &self) $res (: $res $t))
        ");
        assert_eq!(result, Ok(vec![vec![expr!(":" "S" "Int")]]));
    }

    #[test]
    fn metta_interpret_func() {
        let result = run_program("
            (: a T)
            (: foo (-> T T))
            (= (foo $x) $x)
            (= (bar $x) $x)
            !(metta (foo (bar a)) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
        let result = run_program("
            (: b B)
            (: foo (-> T T))
            (= (foo $x) $x)
            !(metta (foo b) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" "b" "BadType")]]));
        let result = run_program("
            (: Nil (List $t))
            (: Z Nat)
            (: S (-> Nat Nat))
            (: Cons (-> $t (List $t) (List $t)))
            !(metta (Cons S (Cons Z Nil)) %Undefined% &self)
        ");
        assert_eq!(result, Ok(vec![vec![expr!("Error" ("Cons" "Z" "Nil") "BadType")]]));
    }

    #[test]
    fn metta_interpret_tuple() {
        assert_eq!(run_program("!(metta () %Undefined% &self)"), Ok(vec![vec![expr!(())]]));
        assert_eq!(run_program("!(metta (a) %Undefined% &self)"), Ok(vec![vec![expr!(("a"))]]));
        assert_eq!(run_program("!(metta (a b) %Undefined% &self)"), Ok(vec![vec![expr!(("a" "b"))]]));
        assert_eq!(run_program("
            (= (foo $x) (bar $x))
            (= (bar $x) (baz $x))
            (= (baz $x) $x)
            !(metta ((foo A) (foo B)) %Undefined% &self)
        "), Ok(vec![vec![expr!("A" "B")]]));
    }

    #[test]
    fn metta_interpret_expression_as_type() {
        assert_eq!(run_program("(= (foo $x) $x) !(metta (foo a) %Undefined% &self)"), Ok(vec![vec![expr!("a")]]));
        assert_eq!(run_program("!(metta (foo a) %Undefined% &self)"), Ok(vec![vec![expr!("foo" "a")]]));
        assert_eq!(run_program("!(metta () SomeType &self)"), Ok(vec![vec![expr!(())]]));
    }

    #[test]
    fn metta_interpret_single_atom_with_two_types() {
        let result = run_program("(: a A) (: a B) !(metta a %Undefined% &self)");
        assert_eq!(result, Ok(vec![vec![expr!("a")]]));
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
    fn metta_let_novar() {
        let result = run_program("!(let (P A $b) (P $a B) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("
            (= (foo) (P A B))
            !(let (P A $b) (foo) (P $b A))
            ");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("
            (= (foo) (P A B))
            !(let (foo) (P A $b) (P $b A))
            ");
        assert_eq!(result, Ok(vec![vec![]]));
        let result = run_program("!(let (P A $b) (P B C) (P C B))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_let_var() {
        let result = run_program("!(let* () result)");
        assert_eq!(result, Ok(vec![vec![expr!("result")]]));
        let result = run_program("!(let* ( ((P A $b) (P $a B)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("!(let* ( ((P $a) (P A)) ((P B) (P $b)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![expr!("P" "B" "A")]]));
        let result = run_program("!(let* ( ((P $a) (P A)) ((P B) (P C)) ) (P $b $a))");
        assert_eq!(result, Ok(vec![vec![]]));
    }

    #[test]
    fn metta_quote_unquote() {
        let header = "
            (= (foo) A)
            (= (bar $x) $x)
        ";
        assert_eq!(run_program(&format!("{header} !(bar (foo))")), Ok(vec![vec![sym!("A")]]), "sanity check");
        assert_eq!(run_program(&format!("{header} !(bar (quote (foo)))")), Ok(vec![vec![expr!("quote" ("foo"))]]), "quote");
        assert_eq!(run_program(&format!("{header} !(bar (unquote (quote (foo))))")), Ok(vec![vec![expr!("A")]]), "unquote before call");
        assert_eq!(run_program(&format!("{header} !(unquote (bar (quote (foo))))")), Ok(vec![vec![expr!("A")]]), "unquote after call");
    }


    #[test]
    fn test_frog_reasoning() {
        let program = "
            (= (is Fritz croaks) True)
            (= (is Fritz eats-flies) True)

            (= (is Tweety chirps) True)
            (= (is Tweety yellow) True)
            (= (is Tweety eats-flies) True)

            !(metta (if (and (is $x croaks) (is $x eats-flies)) (= (is $x frog) True) Empty) %Undefined% &self)
        ";

        assert_eq!(run_program(program),
            Ok(vec![vec![expr!("=" ("is" "Fritz" "frog") {Bool(true)})]]));
    }

    #[test]
    fn test_match_all() {
        let program = "
            (= (color) blue)
            (= (color) red)
            (= (color) green)

            !(metta (color) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("blue"), expr!("red"), expr!("green")]]));
    }

    #[test]
    fn test_variable_keeps_value_in_different_sub_expressions() {
        let program = "
            (= (eq $x $x) True)
            (= (plus Z $y) $y)
            (= (plus (S $k) $y) (S (plus $k $y)))

            !(metta (eq (plus Z $n) $n) %Undefined% &self)
            !(metta (eq (plus (S Z) $n) $n) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!({Bool(true)})], vec![expr!("eq" ("S" n) n)]]));
    }

    #[test]
    fn test_variable_defined_via_variable() {
        let program = "
            (= (myif T $y) $y)
            (= (mynot F) T)
            (= (a $z) (mynot (b $z)))
            (= (b d) F)

            !(metta (myif (a $x) $x) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!("d")]]));
    }

    #[test]
    fn test_variable_name_conflict() {
        let program = "
            (= (a ($W)) True)

            !(metta (a $W) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program),
            Ok(vec![vec![expr!({Bool(true)})]]));
    }

    #[test]
    fn test_variable_name_conflict_renaming() {
        let program = "
            (= (b ($x $y)) (c $x $y))

            !(metta (a (b $a) $x $y) %Undefined% &self)
        ";

        let result = run_program(program);
        assert!(result.is_ok_and(|res| res.len() == 1 && res[0].len() == 1 &&
            atoms_are_equivalent(&res[0][0], &expr!("a" ("c" a b) c d))));
    }

    #[test]
    fn test_operation_is_expression() {
        let program = "
            (: foo (-> (-> A A)))
            (: a A)
            (= (foo) bar)
            (= (bar $x) $x)

            !(metta ((foo) a) %Undefined% &self)
        ";

        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("a")]]));
    }

    static ID_NUM: &Operation = &Operation{
        name: "id_num",
        execute: |_, args| {
            let arg_error = || ExecError::from("id_num expects one argument: number");
            let num = args.get(0).ok_or_else(arg_error)?;
            Ok(vec![num.clone()])
        },
        typ: "(-> Number Number)",
    };

    #[test]
    fn test_return_bad_type_error() {
        let program1 = "
            (: myAtom myType)
            (: id_a (-> A A))
            (= (id_a $a) $a)

            !(metta (id_a myAtom) %Undefined% &self)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));

        let program2 = "
            !(metta (id_num myAtom) %Undefined% &self)
        ";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));
    }

    #[test]
    fn test_return_incorrect_number_of_args_error() {
        let program1 = "
            (: a A)
            (: b B)
            (: c C)
            (: foo (-> A B C))
            (= (foo $a $b) c)

            !(metta (foo a b) %Undefined% &self)
        ";

        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
            |_| Atom::gnd(ID_NUM));

        assert_eq!(metta.run(SExprParser::new(program1)),
            Ok(vec![vec![expr!("c")]]));

        let program2 = "!(metta (foo a) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program2)),
            Ok(vec![vec![expr!("Error" ("foo" "a") "IncorrectNumberOfArguments")]]));

        let program3 = "!(metta (foo a b c) %Undefined% &self)";

        assert_eq!(metta.run(SExprParser::new(program3)),
            Ok(vec![vec![expr!("Error" ("foo" "a" "b" "c") "IncorrectNumberOfArguments")]]));
    }

    #[test]
    fn sealed_op_runner() {
        let nested = run_program("!(sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b)))))");
        let simple_replace = run_program("!(sealed ($x $y) (quote (= ($y $z))))");

        assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("quote" ("=" (a b c) (z)))));
        assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("quote" ("=" (y z)))));
    }

    #[test]
    fn sealed_op_execute() {
        let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
        assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
    }

    #[test]
    fn use_sealed_to_make_scoped_variable() {
        assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
        assert_eq!(run_program("!(let $x (input $y) (output $x))"), Ok(vec![vec![expr!("output" ("input" y))]]));
        assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
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

    #[derive(Clone, PartialEq, Debug)]
    pub struct SomeGndAtom { }

    impl Display for SomeGndAtom {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "some-gnd-atom")
        }
    }

    impl Grounded for SomeGndAtom {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, sym!("Arg1Type"), sym!("Arg2Type"), sym!("RetType")])
        }
    }

    #[test]
    fn test_get_doc_func() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: Arg1Type Type)
            (: Arg2Type Type)
            (: RetType Type)
            (: some-func (-> Arg1Type Arg2Type RetType))
            (@doc some-func
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )
            
            !(get-doc some-func)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "some-func")
                ("@kind" "function")
                ("@type" ("->" "Arg1Type" "Arg2Type" "RetType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "RetType") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_get_doc_atom() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: SomeAtom SomeType)
            (@doc SomeAtom (@desc "Test symbol atom having specific type"))

            !(get-doc SomeAtom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "SomeAtom")
                ("@kind" "atom")
                ("@type" "SomeType")
                ("@desc" {Str::from_str("Test symbol atom having specific type")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_gnd_func() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        metta.tokenizer().borrow_mut()
            .register_token(regex::Regex::new(r"some-gnd-atom").unwrap(), |_| Atom::gnd(SomeGndAtom{}));
        let parser = SExprParser::new(r#"
            (@doc some-gnd-atom
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )
            !(get-doc some-gnd-atom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" {SomeGndAtom{}})
                ("@kind" "function")
                ("@type" ("->" "Arg1Type" "Arg2Type" "RetType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "RetType") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_get_doc_no_doc() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            !(get-doc NoSuchAtom)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "NoSuchAtom")
                ("@kind" "atom")
                ("@type" "%Undefined%")
                ("@desc" {Str::from_str("No documentation")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_function_call() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: Arg1Type Type)
            (: Arg2Type Type)
            (: RetType Type)
            (: some-func (-> Arg1Type Arg2Type RetType))
            (@doc some-func
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )

            !(get-doc (some-func arg1 arg2))
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" ("some-func" "arg1" "arg2"))
                ("@kind" "atom")
                ("@type" "RetType")
                ("@desc" {Str::from_str("No documentation")}) )],
        ]));
    }

    #[test]
    fn test_get_doc_no_type() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (@doc some-func-no-type
              (@desc "Test function")
              (@params (
                (@param "First argument")
                (@param "Second argument")
              ))
              (@return "Return value")
            )

            !(get-doc some-func-no-type)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("@doc-formal"
                ("@item" "some-func-no-type")
                ("@kind" "function")
                ("@type" "%Undefined%")
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "%Undefined%") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "%Undefined%") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "%Undefined%") ("@desc" {Str::from_str("Return value")})) )],
        ]));
    }

    #[test]
    fn test_error_is_used_as_an_argument() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            !(get-type Error)
            !(get-metatype Error)
            !(get-type (Error Foo Boo))
            !(Error (+ 1 2) (+ 1 +))
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!("->" "Atom" "Atom" "ErrorType")],
            vec![expr!("Symbol")],
            vec![expr!("ErrorType")],
            vec![expr!("Error" ({SumOp{}} {Number::Integer(1)} {Number::Integer(2)}) ({SumOp{}} {Number::Integer(1)} {SumOp{}}))],
        ]));
    }

    #[test]
    fn test_string_parsing() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            !(id "test")
            !(id "te st")
            !(id "te\"st")
            !(id "")
            !(id "te\nst")
            !("te\nst"test)
        "#);

        assert_eq_metta_results!(metta.run(parser), Ok(vec![
            vec![expr!({Str::from_str("test")})],
            vec![expr!({Str::from_str("te st")})],
            vec![expr!({Str::from_str("te\"st")})],
            vec![expr!({Str::from_str("")})],
            vec![expr!({Str::from_str("te\nst")})],
            vec![expr!({Str::from_str("te\nst")} "test")],
        ]));
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
        assert_eq!(res, vec![UNIT_ATOM]);
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
        assert_eq!(res, vec![UNIT_ATOM]);
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
    fn bind_new_space_op() {
        let tokenizer = Shared::new(Tokenizer::new());

        let bind_op = BindOp::new(tokenizer.clone());

        assert_eq!(bind_op.execute(&mut vec![sym!("&my"), sym!("definition")]), unit_result());
        let borrowed = tokenizer.borrow();
        let constr = borrowed.find_token("&my");
        assert!(constr.is_some());
        assert_eq!(constr.unwrap()("&my"), Ok(sym!("definition")));
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
    fn unique_op() {
        let unique_op = UniqueAtomOp{};
        let actual = unique_op.execute(&mut vec![expr!(
                ("A" ("B" "C"))
                ("A" ("B" "C"))
                ("f" "g")
                ("f" "g")
                ("f" "g")
                "Z"
        )]).unwrap();
        assert_eq_no_order!(actual,
                   vec![expr!(("A" ("B" "C")) ("f" "g") "Z")]);
    }

    #[test]
    fn union_op() {
        let union_op = UnionAtomOp{};
        let actual = union_op.execute(&mut vec![expr!(
                ("A" ("B" "C"))
                ("A" ("B" "C"))
                ("f" "g")
                ("f" "g")
                ("f" "g")
                "Z"
            ), expr!(
                ("A" ("B" "C"))
                "p"
                "p"
                ("Q" "a")
            )]).unwrap();
        assert_eq_no_order!(actual,
                   vec![expr!(("A" ("B" "C")) ("A" ("B" "C"))
                        ("f" "g") ("f" "g") ("f" "g") "Z"
                        ("A" ("B" "C")) "p" "p" ("Q" "a"))]);
    }

    #[test]
    fn intersection_op() {
        let intersection_op = IntersectionAtomOp{};
        let actual = intersection_op.execute(&mut vec![expr!(
                "Z"
                ("A" ("B" "C"))
                ("A" ("B" "C"))
                ("f" "g")
                ("f" "g")
                ("f" "g")
                ("P" "b")
            ), expr!(
                ("f" "g")
                ("f" "g")
                ("A" ("B" "C"))
                "p"
                "p"
                ("Q" "a")
                "Z"
            )]).unwrap();
        assert_eq_no_order!(actual, vec![expr!("Z" ("A" ("B" "C")) ("f" "g") ("f" "g"))]);

        assert_eq_no_order!(intersection_op.execute(&mut vec![expr!(
                { Number::Integer(5) }
                { Number::Integer(4) }
                { Number::Integer(3) }
                { Number::Integer(2) }
            ), expr!(
                { Number::Integer(5) }
                { Number::Integer(3) }
            )]).unwrap(), vec![expr!({Number::Integer(5)} {Number::Integer(3)})]);
    }

    #[test]
    fn subtraction_op() {
        let subtraction_op = SubtractionAtomOp{};
        let actual = subtraction_op.execute(&mut vec![expr!(
                "Z"
                "S"
                "S"
                ("A" ("B" "C"))
                ("A" ("B" "C"))
                ("f" "g")
                ("f" "g")
                ("f" "g")
                ("P" "b")
            ), expr!(
                ("f" "g")
                ("A" ("B" "C"))
                "p"
                "P"
                ("Q" "a")
                "Z"
                "S"
                "S"
                "S"
            )]).unwrap();
        assert_eq_no_order!(actual,
                   vec![expr!(("A" ("B" "C")) ("f" "g") ("f" "g") ("P" "b"))]);
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
    fn let_op_keep_variables_equalities_issue290() {
        assert_eq_metta_results!(run_program("!(let* (($f f) ($f $x)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let* (($f $x) ($f f)) $x)"), Ok(vec![vec![expr!("f")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $y A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
        assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $z A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
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
    fn random_op() {
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0..5;
        let res_i64: i64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_i64));
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(2)}), expr!({Number::Integer(-2)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));

        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0.0..5.0;
        let res_f64: f64 = res.unwrap().get(0).and_then(Number::from_atom).unwrap().into();
        assert!(range.contains(&res_f64));
        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));
    }

    #[test]
    fn size_atom_op() {
        let res = SizeAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} {Number::Integer(3)} {Number::Integer(2)} {Number::Integer(1)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(5)})]);
        let res = SizeAtomOp{}.execute(&mut vec![expr!()]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(0)})]);
    }

    #[test]
    fn min_atom_op() {
        let res = MinAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} {Number::Float(5.5)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(4)})]);
        let res = MinAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} "A")]);
        assert_eq!(res, Err(ExecError::from("Only numbers are allowed in expression")));
        let res = MinAtomOp{}.execute(&mut vec![expr!()]);
        assert_eq!(res, Err(ExecError::from("Empty expression")));
    }

    #[test]
    fn max_atom_op() {
        let res = MaxAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} {Number::Float(5.5)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Float(5.5)})]);
        let res = MaxAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} "A")]);
        assert_eq!(res, Err(ExecError::from("Only numbers are allowed in expression")));
        let res = MaxAtomOp{}.execute(&mut vec![expr!()]);
        assert_eq!(res, Err(ExecError::from("Empty expression")));
    }

    #[test]
    fn index_atom_op() {
        let res = IndexAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} {Number::Integer(3)} {Number::Integer(2)} {Number::Integer(1)}), expr!({Number::Integer(2)})]).expect("No result returned");
        assert_eq!(res, vec![expr!({Number::Integer(3)})]);
        let res = IndexAtomOp{}.execute(&mut vec![expr!({Number::Integer(5)} {Number::Integer(4)} {Number::Integer(3)} {Number::Integer(2)} {Number::Integer(1)}), expr!({Number::Integer(5)})]);
        assert_eq!(res, Err(ExecError::from("Index is out of bounds")));
    }
}