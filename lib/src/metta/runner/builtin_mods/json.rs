use crate::{Atom, ExecError};
use crate::gnd::GroundedFunctionAtom;
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_SPACE};
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, ModuleLoader, RunContext};
use crate::metta::runner::modules::MettaMod;
use crate::metta::runner::str::{atom_to_string, ATOM_TYPE_STRING};
use crate::metta::runner::str::Str;
use serde_json::Value;
use crate::space::DynSpace;
use crate::metta::runner::bool::*;
use crate::metta::runner::number::Number;

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

    fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();

        tref.register_function(GroundedFunctionAtom::new(
            r"json-encode".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_STRING]),
            json_encode));

        tref.register_function(GroundedFunctionAtom::new(
            r"json-decode".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_SPACE]),
            json_decode));

        Ok(())
    }
}

fn json_encode(_args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    // let arg_error = || ExecError::from("json-encode expects atom to encode as input");
    // let input = args.get(0).ok_or_else(arg_error)?.into();

    Ok(vec![Atom::gnd(Str::from_str(""))])
}

fn decode_value(v: &Value) -> Atom {
    match v {
        Value::Array(_) => decode_array(v.as_array().unwrap()),
        Value::Null => Atom::gnd(Str::from_str("null")),
        Value::Bool(_) => Atom::gnd(Bool(v.as_bool().unwrap())),
        Value::String(_) => Atom::gnd(Str::from_string(v.to_string())),
        Value::Object(_) => decode_object(&v.clone()),
        Value::Number(_) => {
            if v.is_i64() {
                Atom::gnd(Number::Integer(v.as_i64().unwrap()))
            }
            else if v.is_f64() {
                Atom::gnd(Number::Float(v.as_f64().unwrap()))
            }
            else {
                Atom::gnd(Number::Integer(v.as_u64().unwrap() as i64))
            }
        },
    }
}

fn decode_object(obj: &Value) -> Atom {
    let dict = obj.as_object().unwrap();
    let keys = dict.keys();
    let space = DynSpace::new(GroundingSpace::new());
    for key in keys {
        let value = dict.get(key).unwrap();
        let decoded_value = decode_value(value);
        space.borrow_mut().add(Atom::expr([Atom::gnd(Str::from_string(key.clone())), decoded_value]));
    }
    Atom::gnd(space)
}

fn decode_array(arr: &Vec<Value>) -> Atom {
    let mut res: Vec<Atom> = vec![];
    for val in arr {
        let decoded_val = decode_value(val);
        res.push(decoded_val);
    }
    Atom::expr(res)
}


fn json_decode(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("json-decode expects string to decode as input");
    let input = atom_to_string(args.get(0).ok_or_else(arg_error)?);

    match &serde_json::from_str(&input) {
        Ok(decoded) => Ok(vec![decode_value(decoded)]),
        Err(e) => Err(ExecError::from(format!("Failed to decode string. Reason: {}", e))),
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
