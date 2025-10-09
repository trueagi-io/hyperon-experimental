use std::{fmt::Debug, sync::Arc};

use hyperon_atom::{Atom, CustomExecute, ExecError, Grounded};
use hyperon_space::{DynSpace, ATOM_TYPE_SPACE};
use metta_bus_client::space::DistributedAtomSpace;
use metta_bus_client::host_id_from_atom;

use crate::metta::runner::modules::{MettaMod, ModuleLoader};
use crate::metta::runner::stdlib::{grounded_op, regex};
use crate::metta::runner::{Metta, RunContext, SExprParser};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_SYMBOL};
use crate::space::grounding::GroundingSpace;

/// This module expose DAS directly in metta-repl:
/// 
/// Run the MeTTa REPL binary:
/// ./target/release/metta-repl
/// Visit https://metta-lang.dev/ for tutorials.
/// Execute !(help!) to get list of the standard library functions.
/// > !(import! &self das)
/// [()]
/// > !(bind! &das (new-das! localhost:42000-42999 localhost:35700))
/// [()]
/// > !(match &das (Similarity "human" $S) ($S))
/// [(a408f6dd446cdd4fa56f82e77fe6c870), (3225ea795289574ceee32e091ad54ef4), (181a19436acef495c8039a610be59603)]
/// > 

/// Loader to Initialize the "das" module
#[derive(Debug)]
pub(crate) struct DasModLoader;

impl ModuleLoader for DasModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        let space = GroundingSpace::new();
        context.init_self_module(space.into(), None);
        self.load_tokens(context.module(), context.metta.clone())
    }

    fn load_tokens(&self, target: &MettaMod, metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();
        let metta_runner = Arc::new(move |program| metta.run(SExprParser::new(program)));
        let new_das_op = Atom::gnd(NewDasOp { metta_runner });
        tref.register_token(regex(r"new-das!"), move |_| new_das_op.clone());
        Ok(())
    }
}

#[derive(Clone)]
pub struct NewDasOp {
    metta_runner: Arc<dyn Fn(String) -> Result<Vec<Vec<Atom>>, String>>,
}

impl Debug for NewDasOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NewDasOp")
    }
}

grounded_op!(NewDasOp, "new-das!");

impl Grounded for NewDasOp {
    fn type_(&self) -> Atom {
        Atom::expr([
            ARROW_SYMBOL,
            ATOM_TYPE_SYMBOL,
            ATOM_TYPE_SYMBOL,
            ATOM_TYPE_SPACE,
        ])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for NewDasOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() == 2 {
            let server = args.get(0).ok_or(ExecError::from(
                "new-das! first argument must be a valid endpoint with port range (eg. 0.0.0.0:42000-42999)",
            ))?;
            let client = args.get(1).ok_or(ExecError::from(
                "new-das! second argument must be a valid endpoint (eg. 0.0.0.0:35700)",
            ))?;
            let (host_id, port_lower, port_upper) = host_id_from_atom(server)?;
            let (known_peer, _, _) = host_id_from_atom(client)?;
            let space = Atom::gnd(DynSpace::new(DistributedAtomSpace::new(
                Some("context".to_string()),
                host_id,
                known_peer,
                port_lower,
                port_upper,
                self.metta_runner.clone(),
            )));
            log::debug!(target: "das", "new-das! initialized.");
            Ok(vec![space])
        } else {
            Err("new-das! expects 2 arguments, eg. !(bind! &das (new-das! 0.0.0.0:42000-42999 0.0.0.0:35700))".into())
        }
    }
}
