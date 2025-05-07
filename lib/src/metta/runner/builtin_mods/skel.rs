use crate::metta::*;
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{ModuleLoader, RunContext, Metta, MettaMod};
use crate::atom::gnd::*;

pub static SKEL_METTA: &'static str = include_str!("skel.metta");

#[derive(Debug)]
pub(crate) struct SkelModLoader;

impl ModuleLoader for SkelModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        // Initialize module's space
        let space = GroundingSpace::new();
        context.init_self_module(space.into(), None);

        // Load module's tokens
        let _ = self.load_tokens(context.module(), context.metta.clone())?;

        // Parse MeTTa code of the module
        let parser = SExprParser::new(SKEL_METTA);
        context.push_parser(Box::new(parser));

        Ok(())
    }

    fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();

        tref.register_function(GroundedFunctionAtom::new(
                r"skel-swap-pair-native".into(),
                expr!("->" ("PairType" ta tb) ("PairType" tb ta)),
                skel_swap_pair_native));

        Ok(())
    }
}

fn skel_swap_pair_native(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("single pair is expected as an argument");
    let pair = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?;
    let pair = Atom::expr([pair.children()[0].clone(), pair.children()[2].clone(), pair.children()[1].clone()]) ;
    Ok(vec![pair])
}

#[cfg(test)]
mod tests {
    use crate::metta::*;
    use crate::metta::runner::run_program;

    #[test]
    fn test_import_skel() {
        let program = "
            !(import! &self skel)
            !(skel-swap-pair (Pair a b))
            !(skel-swap-pair-native (Pair a b))
        ";
        assert_eq!(run_program(program), Ok(vec![
                vec![UNIT_ATOM],
                vec![expr!("Pair" "b" "a")],
                vec![expr!("Pair" "b" "a")],
        ]));
    }
}
