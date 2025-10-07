use std::{fmt::Debug, sync::{Arc, Mutex}};

use hyperon_atom::{Atom, CustomExecute, ExecError, Grounded};
use hyperon_space::{DynSpace, ATOM_TYPE_SPACE};
use metta_bus_client::{properties::PropertyValue, space::DistributedAtomSpace};
use metta_bus_client::host_id_from_atom;
use metta_bus_client::properties::Properties;

use crate::metta::{runner::{modules::{MettaMod, ModuleLoader}, stdlib::unit_result}, ATOM_TYPE_ATOM, UNIT_TYPE};
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
/// > !(bind! &das (new-das! localhost:42000-42999 localhost:40002))
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

        let params = Arc::new(Mutex::new(Properties::default()));

        let new_das_op = Atom::gnd(NewDasOp { metta_runner, params: params.clone() });
        tref.register_token(regex(r"new-das!"), move |_| new_das_op.clone());

        let set_params_op = Atom::gnd(SetDasParamOp { params: params.clone() });
        tref.register_token(regex(r"das-set-param!"), move |_| { set_params_op.clone() });

        let get_params_op = Atom::gnd(GetDasParamsOp { params: params.clone() });
        tref.register_token(regex(r"das-get-params!"), move |_| { get_params_op.clone() });

        Ok(())
    }
}

#[derive(Clone)]
pub struct NewDasOp {
    params: Arc<Mutex<Properties>>,
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
                self.params.clone(),
                self.metta_runner.clone(),
            )));
            log::debug!(target: "das", "new-das! initialized.");
            Ok(vec![space])
        } else {
            Err("new-das! expects 2 arguments, eg. !(bind! &das (new-das! 0.0.0.0:42000-42999 0.0.0.0:35700))".into())
        }
    }
}

#[derive(Clone)]
pub struct SetDasParamOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for SetDasParamOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SetDasParamOp")
    }
}

grounded_op!(SetDasParamOp, "das-set-param!");

impl Grounded for SetDasParamOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SetDasParamOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("das-set-param! expects only one argument: atom");
        let atom = args.get(0).ok_or_else(arg_error)?;
        match atom {
			Atom::Expression(exp_atom) => {
				let mut params = self.params.lock().unwrap();
				let children = exp_atom.children();
                if children.len() != 2 {
                    return Err(ExecError::from("das-set-param! expects an expression with two arguments: key and value, eg. !(das-set-param! (max_answers 100))"));
                }
                let key = children[0].to_string();
                if params.try_get(&key).is_none() {
                    return Err(ExecError::from(format!("DAS Param '{}' does not exist", key)));
                }
                let value = PropertyValue::parse_str(&children[1].to_string()).unwrap();
                params.insert(key.clone(), value.clone());
                println!("DAS Param Updated: '{}': {:?}", key, value);
			},
			_ => return Err(ExecError::from("das-set-param! expects an atom as the argument")),
		}

        unit_result()
    }
}

#[derive(Clone)]
pub struct GetDasParamsOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for GetDasParamsOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GetDasParamsOp")
    }
}

grounded_op!(GetDasParamsOp, "das-get-params!");

impl Grounded for GetDasParamsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetDasParamsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        if args.len() != 0 {
            return Err(ExecError::from("das-get-params! expects no arguments"));
        }
		let params = self.params.lock().unwrap();
        println!("DAS Params:");
        for key in params.keys() {
            println!("'{}': {:?}", key, params.try_get(&key).unwrap().clone());
        }
        unit_result()
    }
}
