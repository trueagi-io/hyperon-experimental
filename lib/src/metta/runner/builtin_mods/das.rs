use std::sync::{Arc, Mutex};

use hyperon_atom::{Atom, CustomExecute, ExecError, Grounded};
use hyperon_space::{DynSpace, ATOM_TYPE_SPACE};
use metta_bus_client::space::DistributedAtomSpace;
use metta_bus_client::{host_id_from_atom, init_service_bus};

use crate::metta::runner::modules::{MettaMod, ModuleLoader};
use crate::metta::runner::stdlib::{grounded_op, regex};
use crate::metta::runner::{Metta, RunContext};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_SYMBOL};
use crate::space::grounding::GroundingSpace;

/// This module expose DAS directly in metta-repl:
/// 
/// In order to use it we must compile metta-repl using the "das" feature:
/// cargo build --release --features=das
/// 
/// Then run the MeTTa REPL binary:
/// ./target/release/metta-repl
/// Visit https://metta-lang.dev/ for tutorials.
/// Execute !(help!) to get list of the standard library functions.
/// > !(import! &self das)
/// [()]
/// > !(bind! &das (new-das! (localhost:8080) (localhost:35700)))
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

    fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();
        let new_das_op = Atom::gnd(NewDasOp {});
        tref.register_token(regex(r"new-das!"), move |_| new_das_op.clone());
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct NewDasOp {}

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
                "new-das! first argument must be a valid endpoint (eg. 0.0.0.0:8080)",
            ))?;
            let client = args.get(1).ok_or(ExecError::from(
                "new-das! second argument must be a valid endpoint (eg. 0.0.0.0:35700)",
            ))?;
            let host_id = host_id_from_atom(server)?;
            let known_peer = host_id_from_atom(client)?;
            let service_bus = Arc::new(Mutex::new(init_service_bus(host_id, known_peer, 42000, 42999).unwrap()));
            let space = Atom::gnd(DynSpace::new(DistributedAtomSpace::new(
                service_bus,
                Some("context".to_string()),
            )));
            log::debug!(target: "das", "new-das! initialized.");
            Ok(vec![space])
        } else {
            Err("new-das! expects 2 arguments, eg. !(bind! &das (new-das! 0.0.0.0:8080 0.0.0.0:35700))".into())
        }
    }
}
