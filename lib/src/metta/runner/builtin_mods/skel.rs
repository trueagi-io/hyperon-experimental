use std::fmt::{Display, Formatter};
use crate::metta::*;
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{ModuleLoader, RunContext, DynSpace};
use crate::metta::runner::stdlib::regex;

pub static SKEL_METTA: &'static str = include_str!("skel.metta");

#[derive(Debug)]
pub(crate) struct SkelModLoader;

impl ModuleLoader for SkelModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        // Initialize module's space
        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        // Load module's tokens
        let mut tref = context.module().tokenizer().borrow_mut();
        let skel_swap_pair_native = Atom::gnd(SkelSwapPairNativeOp{});
        tref.register_token(regex(r"skel-swap-pair-native"), move |_| { skel_swap_pair_native.clone() });
        drop(tref);

        // Parse MeTTa code of the module
        let parser = SExprParser::new(SKEL_METTA);
        context.push_parser(Box::new(parser));

        Ok(())
    }

}

#[derive(Clone, PartialEq, Debug)]
pub struct SkelSwapPairNativeOp{}

impl Display for SkelSwapPairNativeOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "skel-swap-pair-native")
    }
}

impl Grounded for SkelSwapPairNativeOp {
    fn type_(&self) -> Atom {
        expr!("->" (ta tb) (tb ta))
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for SkelSwapPairNativeOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("skel-swap-pair-native expects single pair as argument");
        let pair = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?;
        let pair = Atom::expr([pair.children()[1].clone(), pair.children()[0].clone()]) ;
        Ok(vec![pair])
    }
}
