use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::{Metta, RunContext, ResourceKey};
use crate::metta::runner::string::Str;
use crate::metta::types::{get_atom_types, get_meta_type};
use crate::common::shared::Shared;
use crate::common::CachingMapper;
use crate::common::multitrie::MultiTrie;
use crate::space::grounding::atom_to_trie_key;

#[cfg(feature = "pkg_mgmt")]
use crate::metta::runner::{git_catalog::ModuleGitLocation, mod_name_from_url, pkg_mgmt::UpdateMode};

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Display;
use std::collections::HashMap;
use regex::Regex;
use rand::Rng;

use super::arithmetics::*;
use super::string::*;
use super::stdlib_minimal::*;

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
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE()])
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
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
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
    context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
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
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE()])
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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE()])
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
            ATOM_TYPE_ATOM, UNIT_TYPE()])
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
            ATOM_TYPE_ATOM, UNIT_TYPE()])
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

        Ok(get_atom_types(self.space.borrow().as_space(), atom))
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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE()])
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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE()])
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
        context: std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<&'static mut RunContext<'static, 'static, 'static>>>>>>,
    }

    grounded_op!(GitModuleOp, "git-module!");

    impl GitModuleOp {
        pub fn new(metta: Metta) -> Self {
            Self{ context: metta.0.context.clone() }
        }
    }

    impl Grounded for GitModuleOp {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE()])
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

        let mut atoms = expr.children().clone();
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
        let mut lhs = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().clone();
        let rhs = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().clone();

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
        let mut lhs = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().clone();
        let rhs = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().clone();

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
                match (res, AsPrimitive::from_atom(x).as_number()) {
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
                match (res, AsPrimitive::from_atom(x).as_number()) {
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
        let index = AsPrimitive::from_atom(args.get(1).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?;
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
        let mut lhs = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?.children().clone();
        let rhs = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?.children().clone();

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
        let start: i64 = AsPrimitive::from_atom(args.get(0).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?.into();
        let end: i64 = AsPrimitive::from_atom(args.get(1).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?.into();
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
        let start: f64 = AsPrimitive::from_atom(args.get(0).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?.into();
        let end: f64 = AsPrimitive::from_atom(args.get(1).ok_or_else(arg_error)?).as_number().ok_or_else(arg_error)?.into();
        let range = start..end;
        if range.is_empty() {
            return Err(ExecError::from("Range is empty"));
        }
        let mut rng = rand::thread_rng();
        Ok(vec![Atom::gnd(Number::Float(rng.gen_range(range)))])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::atom::matcher::atoms_are_equivalent;
    use crate::metta::text::*;
    use crate::metta::runner::EnvBuilder;
    use crate::metta::runner::string::Str;
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

    #[test]
    fn random_op() {
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0..5;
        let res_i64: i64 = AsPrimitive::from_atom(res.unwrap().get(0).unwrap()).as_number().unwrap().into();
        assert!(range.contains(&res_i64));
        let res = RandomIntOp{}.execute(&mut vec![expr!({Number::Integer(2)}), expr!({Number::Integer(-2)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));

        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(5)})]);
        let range = 0.0..5.0;
        let res_f64: f64 = AsPrimitive::from_atom(res.unwrap().get(0).unwrap()).as_number().unwrap().into();
        assert!(range.contains(&res_f64));
        let res = RandomFloatOp{}.execute(&mut vec![expr!({Number::Integer(0)}), expr!({Number::Integer(0)})]);
        assert_eq!(res, Err(ExecError::from("Range is empty")));
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

    #[derive(Clone, PartialEq, Debug)]
    pub struct SomeGndAtom { }

    impl Display for SomeGndAtom {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "some-gnd-atom")
        }
    }

    impl Grounded for SomeGndAtom {
        fn type_(&self) -> Atom {
            Atom::expr([ARROW_SYMBOL, sym!("Arg1Type"), sym!("Arg2Type"), sym!("ReturnType")])
        }
    }

    #[test]
    fn test_get_doc_func() {
        let metta = Metta::new(Some(EnvBuilder::test_env()));
        let parser = SExprParser::new(r#"
            (: Arg1Type Type)
            (: Arg2Type Type)
            (: ReturnType Type)
            (: some-func (-> Arg1Type Arg2Type ReturnType))
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
                ("@type" ("->" "Arg1Type" "Arg2Type" "ReturnType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "ReturnType") ("@desc" {Str::from_str("Return value")})) )],
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
                ("@type" ("->" "Arg1Type" "Arg2Type" "ReturnType"))
                ("@desc" {Str::from_str("Test function")})
                ("@params" (
                    ("@param" ("@type" "Arg1Type") ("@desc" {Str::from_str("First argument")}))
                    ("@param" ("@type" "Arg2Type") ("@desc" {Str::from_str("Second argument")})) ))
                ("@return" ("@type" "ReturnType") ("@desc" {Str::from_str("Return value")})) )],
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
            (: ReturnType Type)
            (: some-func (-> Arg1Type Arg2Type ReturnType))
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
                ("@type" "ReturnType")
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
}
