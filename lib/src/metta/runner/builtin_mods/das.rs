use std::{
    fmt::Debug,
    sync::{Arc, Mutex},
    thread::sleep,
    time::Duration,
};

use hyperon_atom::{matcher::Bindings, Atom, BoxedIter, CustomExecute, ExecError, Grounded};
use hyperon_space::{DynSpace, ATOM_TYPE_SPACE};
use metta_bus_client::properties::{self as das_properties, Properties};
use metta_bus_client::{host_id_from_atom, service_bus_singleton::ServiceBusSingleton};
use metta_bus_client::{properties::PropertyValue, space::DistributedAtomSpace};

use crate::metta::gnd::str::ATOM_TYPE_STRING;
use crate::metta::runner::stdlib::{grounded_op, regex};
use crate::metta::runner::{Metta, RunContext, SExprParser};
use crate::metta::{
    runner::{
        modules::{MettaMod, ModuleLoader},
        stdlib::unit_result,
    },
    ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED, UNIT_TYPE,
};
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

        let new_das_op = Atom::gnd(NewDasOp {
            maybe_metta_runner: Some(metta_runner.clone()),
            params: params.clone(),
        });
        tref.register_token(regex(r"new-das!"), move |_| new_das_op.clone());

        let das_join_network_op = Atom::gnd(DasJoinNetworkOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-join-network!"), move |_| {
            das_join_network_op.clone()
        });

        let das_services_op = Atom::gnd(DasServicesOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-services!"), move |_| das_services_op.clone());

        let das_service_status_op = Atom::gnd(DasServiceStatusOp {});
        tref.register_token(regex(r"das-service-status!"), move |_| {
            das_service_status_op.clone()
        });

        let set_params_op = Atom::gnd(SetDasParamOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-set-param!"), move |_| set_params_op.clone());

        let get_params_op = Atom::gnd(GetDasParamsOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-get-params!"), move |_| get_params_op.clone());

        let das_create_context_op = Atom::gnd(CreateContextOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-create-context!"), move |_| {
            das_create_context_op.clone()
        });

        let das_evolution_op = Atom::gnd(EvolutionDasOp {
            maybe_metta_runner: Some(metta_runner),
            params: params.clone(),
        });
        tref.register_token(regex(r"das-evolution!"), move |_| das_evolution_op.clone());

        let das_link_creation_op = Atom::gnd(LinkCreationDasOp {
            params: params.clone(),
        });
        tref.register_token(regex(r"das-link-creation!"), move |_| {
            das_link_creation_op.clone()
        });

        let das_helpers_op = Atom::gnd(DasHelpersOp {});
        tref.register_token(regex(r"das-helpers!"), move |_| das_helpers_op.clone());

        Ok(())
    }
}

#[derive(Clone)]
pub struct NewDasOp {
    params: Arc<Mutex<Properties>>,
    maybe_metta_runner: Option<Arc<dyn Fn(String) -> Result<Vec<Vec<Atom>>, String>>>,
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
                "new-das! second argument must be a valid endpoint (eg. 0.0.0.0:40002)",
            ))?;
            let (hostname, port_lower, port_upper) = host_id_from_atom(server)?;
            let (known_peer, _, _) = host_id_from_atom(client)?;

            let mut params = self.params.lock().unwrap();
            params.insert(
                das_properties::HOSTNAME.to_string(),
                PropertyValue::String(hostname.clone()),
            );
            params.insert(
                das_properties::PORT_LOWER.to_string(),
                PropertyValue::UnsignedInt(port_lower.clone() as u64),
            );
            params.insert(
                das_properties::PORT_UPPER.to_string(),
                PropertyValue::UnsignedInt(port_upper.clone() as u64),
            );
            params.insert(
                das_properties::KNOWN_PEER_ID.to_string(),
                PropertyValue::String(known_peer.clone()),
            );
            drop(params);

            let das_space = match DistributedAtomSpace::new(
                self.params.clone(),
                self.maybe_metta_runner.clone(),
            ) {
                Ok(space) => space,
                Err(e) => return Err(ExecError::from(format!("Error at new-das: {e}"))),
            };
            let space = Atom::gnd(DynSpace::new(das_space));

            log::debug!(target: "das", "new-das! initialized.");
            Ok(vec![space])
        } else {
            Err("new-das! expects 2 arguments, eg. !(bind! &das (new-das! 0.0.0.0:42000-42999 0.0.0.0:40002))".into())
        }
    }
}

#[derive(Clone)]
pub struct DasJoinNetworkOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for DasJoinNetworkOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DasJoinNetworkOp")
    }
}

grounded_op!(DasJoinNetworkOp, "das-join-network!");

impl Grounded for DasJoinNetworkOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for DasJoinNetworkOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("das-join-network! expects no arguments");
        if args.len() != 0 {
            return Err(arg_error());
        }
        let mut das_space = DistributedAtomSpace::new(self.params.clone(), None)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        das_space
            .join_network()
            .map_err(|e| ExecError::from(format!("{e}")))?;
        unit_result()
    }
}

#[derive(Clone)]
pub struct DasServicesOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for DasServicesOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DasServicesOp")
    }
}

grounded_op!(DasServicesOp, "das-services!");

impl Grounded for DasServicesOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for DasServicesOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("das-services! expects no arguments");
        if args.len() != 0 {
            return Err(arg_error());
        }
        let das_space = DistributedAtomSpace::new(self.params.clone(), None)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        das_space
            .print_services()
            .map_err(|e| ExecError::from(format!("{e}")))?;
        unit_result()
    }
}

#[derive(Clone)]
pub struct DasServiceStatusOp {}

impl Debug for DasServiceStatusOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DasServiceStatusOp")
    }
}

grounded_op!(DasServiceStatusOp, "das-service-status!");

impl Grounded for DasServiceStatusOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for DasServiceStatusOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error =
            || ExecError::from("das-service-status! expects one argument: service name");
        if args.len() != 1 {
            return Err(arg_error());
        }
        let service_name = args.get(0).ok_or_else(arg_error)?.to_string();
        let service_bus =
            ServiceBusSingleton::get_instance().map_err(|e| ExecError::from(format!("{e}")))?;
        let mut locked_bus = service_bus.bus_node.lock().unwrap().bus.clone();
        // First check if the service is already available
        if locked_bus.contains(service_name.to_string()) {
            if locked_bus
                .get_ownership(service_name.to_string())
                .is_empty()
            {
                log::debug!(target: "das", "Dropping locked bus for service: {}", service_name);
                drop(locked_bus);
                sleep(Duration::from_secs(5));
            }
            locked_bus = service_bus.bus_node.lock().unwrap().bus.clone();
            // If the service is not available, wait for 5 seconds and check again
            if locked_bus
                .get_ownership(service_name.to_string())
                .is_empty()
            {
                return Err(ExecError::from(format!(
                    "ServiceNotAvailable: {}",
                    service_name
                )));
            }
        } else {
            return Err(ExecError::from(format!(
                "ServiceNotRegistered: {}",
                service_name
            )));
        }
        Ok(vec![Atom::sym(format!(
            "ServiceAvailable: {}",
            service_name
        ))])
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
                    return Err(ExecError::from(format!(
                        "DAS Param '{}' does not exist",
                        key
                    )));
                }
                let value = PropertyValue::parse_str(&children[1].to_string()).unwrap();
                params.insert(key.clone(), value.clone());
                println!("DAS Param Updated: '{}': {:?}", key, value);
            }
            _ => {
                return Err(ExecError::from(
                    "das-set-param! expects an atom as the argument",
                ))
            }
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
        params.section_print();
        unit_result()
    }
}

// !(das-create-context ( (query) (determiner_schema) (stimulus_schema) ) )
#[derive(Clone)]
pub struct CreateContextOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for CreateContextOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CreateContextOp")
    }
}

grounded_op!(CreateContextOp, "das-create-context!");

impl Grounded for CreateContextOp {
    fn type_(&self) -> Atom {
        Atom::expr([
            ARROW_SYMBOL,
            ATOM_TYPE_STRING, // context
            ATOM_TYPE_ATOM,   // ( (query) (determiner_schema) (stimulus_schema) )
            ATOM_TYPE_UNDEFINED,
        ])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for CreateContextOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || {
            ExecError::from("
            das-create-context expects 2 arguments: context and ( (query) (determiner_schema) (stimulus_schema) )")
        };
        if args.len() != 2 {
            return Err(arg_error());
        }
        let context = args.get(0).ok_or_else(arg_error)?.clone();
        let atom = args.get(1).ok_or_else(arg_error)?.clone();
        let das_space = DistributedAtomSpace::new(self.params.clone(), None)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        let result = das_space
            .create_context(context.to_string(), &atom)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        Ok(vec![result])
    }
}

// !(das-evolution ( (query) (fitness-function) (correlation-queries) (correlation-replacements) (correlation-mappings) ) template)
#[derive(Clone)]
pub struct EvolutionDasOp {
    params: Arc<Mutex<Properties>>,
    maybe_metta_runner: Option<Arc<dyn Fn(String) -> Result<Vec<Vec<Atom>>, String>>>,
}

impl Debug for EvolutionDasOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EvolutionDasOp")
    }
}

grounded_op!(EvolutionDasOp, "das-evolution!");

impl Grounded for EvolutionDasOp {
    fn type_(&self) -> Atom {
        Atom::expr([
            ARROW_SYMBOL,
            ATOM_TYPE_ATOM, // ( (query) (fitness-function) (correlation-queries) (correlation-replacements) (correlation-mappings) )
            ATOM_TYPE_ATOM, // template
            ATOM_TYPE_UNDEFINED,
        ])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for EvolutionDasOp {
    fn execute_bindings(
        &self,
        args: &[Atom],
    ) -> Result<BoxedIter<'static, (Atom, Option<Bindings>)>, ExecError> {
        let arg_error = || {
            ExecError::from("
            das-evolution expects 2 arguments: ( (query) (fitness-function) (correlation-queries) (correlation-replacements) (correlation-mappings) ) and template")
        };
        if args.len() != 2 {
            return Err(arg_error());
        }
        let atom = args.get(0).ok_or_else(arg_error)?.clone();
        let t = args.get(1).ok_or_else(arg_error)?.clone();
        let das_space =
            DistributedAtomSpace::new(self.params.clone(), self.maybe_metta_runner.clone())
                .map_err(|e| ExecError::from(format!("{e}")))?;
        let r = das_space
            .evolution(&atom)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        Ok(Box::new(r.into_iter().map(move |b| (t.clone(), Some(b)))))
    }
}

#[derive(Clone)]
pub struct LinkCreationDasOp {
    params: Arc<Mutex<Properties>>,
}

impl Debug for LinkCreationDasOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LinkCreationDasOp")
    }
}

grounded_op!(LinkCreationDasOp, "das-link-creation!");

impl Grounded for LinkCreationDasOp {
    fn type_(&self) -> Atom {
        Atom::sym("LinkCreationDas")
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for LinkCreationDasOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || {
            ExecError::from(
                "das-link-creation expects the arguments: query and at least one template",
            )
        };
        if args.len() < 2 {
            return Err(arg_error());
        }
        let query = args.get(0).ok_or_else(arg_error)?.clone();
        let templates = args.get(1..).ok_or_else(arg_error)?.to_vec();
        let das_space = DistributedAtomSpace::new(self.params.clone(), None)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        let result = das_space
            .link_creation(&query, templates)
            .map_err(|e| ExecError::from(format!("{e}")))?;
        Ok(vec![result])
    }
}

#[derive(Clone)]
pub struct DasHelpersOp {}

impl Debug for DasHelpersOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DasHelpersOp")
    }
}

grounded_op!(DasHelpersOp, "das-helpers!");

impl Grounded for DasHelpersOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_ATOM, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for DasHelpersOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || {
            ExecError::from(
                "das-helpers! expects 2 arguments: helper function name and its argument",
            )
        };
        if args.len() != 2 {
            return Err(arg_error());
        }
        let helper_name = args.get(0).ok_or_else(arg_error)?.to_string();
        let argument = args.get(1).ok_or_else(arg_error)?.to_string();
        match helper_name.as_str() {
            "sleep" => {
                let duration = match argument.parse::<u64>() {
                    Ok(duration) => duration,
                    Err(_) => {
                        return Err(ExecError::from(format!("Invalid duration: {}", argument)))
                    }
                };
                sleep(Duration::from_secs(duration));
                unit_result()
            }
            _ => {
                return Err(ExecError::from(format!(
                    "Helper function {} not found",
                    helper_name
                )));
            }
        }
    }
}
