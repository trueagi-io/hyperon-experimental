use super::{grounded_op, regex, unit_result};
use hyperon_atom::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::{Metta, RunContext,
                           git_catalog::ModuleGitLocation,
                           mod_name_from_url,
                           pkg_mgmt::UpdateMode};
use hyperon_atom::str::expect_string_like_atom;

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
        let path = args.get(0).and_then(expect_string_like_atom).ok_or_else(|| ExecError::from(arg_error))?;

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
        let url = args.get(0).and_then(expect_string_like_atom).ok_or_else(|| ExecError::from(arg_error))?;
        // TODO: When we figure out how to address varargs, it will be nice to take an optional branch name

        // TODO: Depending on what we do with `register-module!`, we might want to let the
        // caller provide an optional mod_name here too, rather than extracting it from the url
        let mod_name = match mod_name_from_url(&url) {
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

pub(super) fn register_context_dependent_tokens(tref: &mut Tokenizer, metta: &Metta) {
    let register_module_op = Atom::gnd(RegisterModuleOp::new(metta.clone()));
    tref.register_token(regex(r"register-module!"), move |_| { register_module_op.clone() });
    let git_module_op = Atom::gnd(GitModuleOp::new(metta.clone()));
    tref.register_token(regex(r"git-module!"), move |_| { git_module_op.clone() });
}
