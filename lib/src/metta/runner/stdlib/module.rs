use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::common::shared::Shared;
use crate::metta::runner::{Metta, RunContext, ResourceKey};
use super::{grounded_op, regex, unit_result};
use crate::metta::runner::str::expect_string_like_atom;

use regex::Regex;

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
        let mod_name = args.get(1).and_then(expect_string_like_atom).ok_or_else(arg_error)?;

        // Load the module into the runner, or get the ModId if it's already loaded
        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let mod_id = context.load_module(&mod_name)?;

        // Import the module, as per the behavior described above
        match dest_arg {
            Atom::Symbol(dest_sym) => {
                context.import_dependency_as(mod_id, Some(dest_sym.name().to_string()))?;
            }
            other_atom => {
                match &other_atom {
                    Atom::Grounded(_) if Atom::as_gnd::<DynSpace>(other_atom) == Some(&context.module().space()) => {
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
        let mod_name = args.get(0).and_then(expect_string_like_atom).ok_or_else(arg_error)?;

        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let resource = context.load_resource_from_module(&mod_name, ResourceKey::MainMettaSrc)?;
        let parser = crate::metta::text::SExprParser::new(resource);
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
        let mod_name = args.get(0).and_then(expect_string_like_atom).ok_or_else(|| ExecError::from(arg_error))?;

        // Load the module into the runner, or get the ModId if it's already loaded
        //TODO: Remove this hack to access the RunContext, when it's part of the arguments to `execute`
        let ctx_ref = self.context.lock().unwrap().last().unwrap().clone();
        let mut context = ctx_ref.lock().unwrap();
        let mod_id = context.load_module(&mod_name)?;

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

pub(super) fn register_context_dependent_tokens(tref: &mut Tokenizer, tokenizer: Shared<Tokenizer>, metta: &Metta) {
    let import_op = Atom::gnd(ImportOp::new(metta.clone()));
    tref.register_token(regex(r"import!"), move |_| { import_op.clone() });
    let include_op = Atom::gnd(IncludeOp::new(metta.clone()));
    tref.register_token(regex(r"include"), move |_| { include_op.clone() });
    let bind_op = Atom::gnd(BindOp::new(tokenizer.clone()));
    tref.register_token(regex(r"bind!"), move |_| { bind_op.clone() });
    let mod_space_op = Atom::gnd(ModSpaceOp::new(metta.clone()));
    tref.register_token(regex(r"mod-space!"), move |_| { mod_space_op.clone() });
    let print_mods_op = Atom::gnd(PrintModsOp::new(metta.clone()));
    tref.register_token(regex(r"print-mods!"), move |_| { print_mods_op.clone() });
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
