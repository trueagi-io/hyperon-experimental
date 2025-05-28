use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{ModuleLoader, RunContext};

pub static JSON_METTA: &'static str = include_str!("json.metta");

#[derive(Debug)]
pub(crate) struct JsonModLoader;

impl ModuleLoader for JsonModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {
        // Initialize module's space
        let space = GroundingSpace::new();
        context.init_self_module(space.into(), None);

        // Load module's tokens
        let _ = self.load_tokens(context.module(), context.metta.clone())?;

        // Parse MeTTa code of the module
        let parser = SExprParser::new(JSON_METTA);
        context.push_parser(Box::new(parser));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::metta::*;
    use crate::metta::runner::run_program;

    #[test]
    fn test_json() {
        let program = "
            !(import! &self json)
        ";
        assert_eq!(run_program(program), Ok(vec![
            vec![UNIT_ATOM],
        ]));
    }
}
