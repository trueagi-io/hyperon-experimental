use std::borrow::Cow;
use hyperon_atom::{Atom, ExecError, gnd::GroundedFunctionAtom, ExpressionAtom};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_ATOM};
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, ModuleLoader, RunContext};
use crate::metta::runner::modules::MettaMod;
use crate::metta::runner::str::{atom_to_string, ATOM_TYPE_STRING};
use crate::metta::runner::str::Str;
use serde_json::Value;
use hyperon_space::ATOM_TYPE_SPACE;
use crate::metta::runner::DynSpace;
use crate::metta::runner::bool::*;
use crate::metta::runner::number::{Number, ATOM_TYPE_NUMBER};

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
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_ATOM]),
            json_decode));

        Ok(())
    }
}

fn encode_list(input: &[Atom]) -> Result<String, ExecError> {
    let mut res_str = "[".to_string();
    if !input.is_empty() {
        let first_atom = input.first().unwrap();
        res_str = res_str + &encode_atom(first_atom)?;
        for atom in &input[1..] {
            res_str = res_str + ", " + &encode_atom(atom)?;
        }
    }
    res_str.push_str("]");
    Ok(res_str)
}

fn extract_key_value_pair(atom: &Atom) -> Result<String, ExecError> {
    let res_expr = TryInto::<&ExpressionAtom>::try_into(atom)?;
    let key = res_expr.children().get(0)
        .and_then(|k| Str::from_atom(k))
        .ok_or(ExecError::from("Key/value pair is expected, key must be a string"))?
        .to_string();
    let value = res_expr.children().get(1)
        .ok_or(ExecError::from("Key/value pair is expected"))?;
    let value_str = encode_atom(value)?;
    Ok(key + ": " + &value_str)
}

fn encode_dictspace(input: &Atom) -> Result<String, ExecError> {
    let space = Atom::as_gnd::<DynSpace>(input).unwrap();
    let mut err = None;
    let mut first_atom = true;
    let mut res_str = "{".to_string();
    let _ = space.borrow().visit(&mut |atom: Cow<Atom>| {
        match TryInto::<&ExpressionAtom>::try_into(atom.as_ref()) {
            Ok(expr) => if err.is_none() && expr.children().len() == 2 {
                match extract_key_value_pair(&atom) {
                    Ok(encoded_key_value) => {
                        if !first_atom {
                            res_str.push_str(", ");
                        } else {
                            first_atom = false;
                        }
                        res_str.push_str(&encoded_key_value)
                    },
                    Err(e) => err = Some(e),
                }
            }
            _ => {
                err = Some(ExecError::from("Input space must be a dictionary space consists of key-value pairs only or empty space. Use dict-space function"));
            },
        }
    });
    match err {
        Some(err) => Err(err),
        None => {
            res_str.push_str("}");
            Ok(res_str)
        }
    }
}

fn encode_atom(input: &Atom) -> Result<String, ExecError> {
    fn encode_other(input: &Atom) -> Result<String, ExecError> {
        // String should be encoded using serde_json since it will encode "a" with additional escape
        // characters.
        let atom_string = atom_to_string(&input);
        serde_json::to_string(&atom_string)
            .map_err(|err| ExecError::from(format!("Encode string failed: {}", err)))
    }
    match input {
        Atom::Grounded(gnd) => {
            let typ = gnd.type_();
            if typ == ATOM_TYPE_NUMBER {
                Ok(atom_to_string(input))
            } else if typ == ATOM_TYPE_SPACE {
                encode_dictspace(&input)
            } else if typ == ATOM_TYPE_BOOL {
                let val = gnd.downcast_ref::<Bool>().unwrap().0;
                serde_json::to_string(&val)
                    .map_err(|err| ExecError::from(format!("Encode bool failed: {}", err)))
            } else {
                encode_other(input)
            }
        },
        Atom::Expression(exp) => encode_list(exp.children()),
        // Symbols will use additional sym!: prefix to separate them from regular strings. null is a
        // symbol too, but it should remain "null" after encoding so decoder will see it like Value::Null instance.
        Atom::Symbol(sym) if sym.name() == "null" => Ok("null".into()),
        Atom::Symbol(sym) => {
            let sym_name = "sym!:".to_string() + sym.name();
            serde_json::to_string(&sym_name)
                .map_err(|err| ExecError::from(format!("Encode symbol failed: {}", err)))
        },
        Atom::Variable(_) => encode_other(input),
    }
}

fn json_encode(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("json-encode expects atom to encode as input");
    let input = args.get(0).ok_or_else(arg_error)?;
    encode_atom(input).map(|s| vec![Atom::gnd(Str::from_string(s))])
}

fn decode_value(v: &Value) -> Result<Atom, ExecError> {
    match v {
        Value::Array(_) => {match decode_array(v.as_array().unwrap()) {
            Ok(atom) => Ok(atom),
            Err(err) => Err(err),
        }},
        Value::Null => Ok(Atom::sym("null")),
        Value::Bool(_) => Ok(Atom::gnd(Bool(v.as_bool().unwrap()))),
        Value::String(_) => {
            let decoded_string = v.to_string();
            if decoded_string.starts_with("\"sym!:") {
                let slice = &decoded_string[6..decoded_string.len() - 1];
                Ok(Atom::sym(slice))
            }
            else {
                Ok(Atom::gnd(Str::from_string(v.to_string())))
            }
        },
        Value::Object(_) => {match decode_object(&v.clone()) {
            Ok(atom) => Ok(atom),
            Err(err) => Err(err),
        }},
        Value::Number(_) => {
            if v.is_i64() {
                Ok(Atom::gnd(Number::Integer(v.as_i64().unwrap())))
            }
            else if v.is_f64() {
                Ok(Atom::gnd(Number::Float(v.as_f64().unwrap())))
            }
            else {
                Err(ExecError::from("Currently u64 type is not supported"))
            }
        },
    }
}

fn decode_object(obj: &Value) -> Result<Atom, ExecError> {
    let dict = obj.as_object().unwrap();
    let keys = dict.keys();
    let mut space = GroundingSpace::new();
    for key in keys {
        let value = dict.get(key).unwrap();
        match decode_value(value) {
            Ok(decoded) => space.add(Atom::expr([Atom::gnd(Str::from_string(key.clone())), decoded])),
            Err(err) => return Err(err)
        }
    }
    Ok(Atom::gnd(DynSpace::new(space)))
}

fn decode_array(arr: &Vec<Value>) -> Result<Atom, ExecError> {
    let mut res: Vec<Atom> = vec![];
    for val in arr {
        match decode_value(val) {
            Ok(atom) => res.push(atom),
            Err(err) => return Err(err)
        }
    }
    Ok(Atom::expr(res))
}


fn json_decode(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("json-decode expects string to decode as input");
    let input = atom_to_string(args.get(0).ok_or_else(arg_error)?);
    match &serde_json::from_str(&input) {
        Ok(decoded) => Ok(vec![decode_value(decoded)?]),
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
            !(assertEqual (json-encode 5) \"5\")
            !(assertEqual (json-encode (5 4 3)) \"[5, 4, 3]\")
            !(assertEqual (let $encoded (json-encode (5 4 3)) (json-decode $encoded)) (5 4 3))
            !(assertEqual (json-decode 5) (Error 5 BadType))
            !(assertEqual (json-decode \"[5, 4, 3]\") (5 4 3))
            !(bind! &dictspace (dict-space ((\"k1\" v1) (\"k2\" v2) (\"k3\" (4 \"a\" 5)))))
            !(assertEqual (let $decoded (let $encoded (json-encode &dictspace) (json-decode $encoded)) (get-keys $decoded)) (superpose (\"k1\" \"k2\" \"k3\")))
            !(assertEqual (json-encode True) \"true\")
            !(assertEqual (let $encoded (json-encode False) (json-decode $encoded)) False)
            !(assertEqual (json-decode \"null\") null)
            !(assertEqual (let $encoded (json-encode null) (json-decode $encoded)) null)
            !(assertEqual (json-encode symbol) \"\\\"sym!:symbol\\\"\")
            !(assertEqual (let $encoded (json-encode symbol) (json-decode $encoded)) symbol)
            !(assertEqual (let $emptyspace (new-space) (json-encode $emptyspace)) \"{}\")
        ";
        assert_eq!(run_program(program), Ok(vec![
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
        ]));
    }
}
