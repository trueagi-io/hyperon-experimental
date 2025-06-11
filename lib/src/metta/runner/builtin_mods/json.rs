use hyperon_atom::{Atom, ExecError, gnd::GroundedFunctionAtom, ExpressionAtom};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_SPACE};
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, ModuleLoader, RunContext};
use crate::metta::runner::modules::MettaMod;
use crate::metta::runner::str::{atom_to_string, ATOM_TYPE_STRING};
use crate::metta::runner::str::Str;
use serde_json::Value;
use crate::metta::runner::DynSpace;
use crate::metta::runner::bool::*;
use crate::metta::runner::number::Number;
use crate::metta::types::{get_atom_types, get_meta_type};

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

fn encode_list(input: Vec<Atom>) -> Result<String, ExecError> {
    let mut res_str = "".to_string();
    for atom in input {
        match encode_atom(&atom) {
            Ok(encoded) => {res_str = res_str + ", " + &encoded;},
            Err(err) => return Err(err),
        }
    }
    res_str.remove(0);
    res_str.remove(0);
    Ok(format!("[{}]", res_str))
}

fn encode_dictspace(input: &Atom) -> Result<String, ExecError> {
    let space = Atom::as_gnd::<DynSpace>(input).unwrap();
    let result = space.borrow().subst(&Atom::expr([Atom::var("k"), Atom::var("v")]),
                                      &Atom::expr([Atom::var("k"), Atom::var("v")]));
    if result.len() == 0 {
        return Err(ExecError::from("Input space must be a dictionary space which contains (key value) pairs. See dict-space function"));
    }
    let mut res_str = "".to_string();
    for res_atom in result {
        let res_expr = TryInto::<&ExpressionAtom>::try_into(&res_atom)?;
        let key = match Str::from_atom(res_expr.children().get(0).unwrap()).ok_or_else(|| ExecError::from("Key must be a string")){
            Ok(str) => str.to_string(),
            Err(err) => return Err(err),
        };
        let value = res_expr.children().get(1).unwrap();
        let value_str = encode_atom(value)?;
        res_str = res_str + ", " + &key + ": " + &value_str;
    }
    res_str.remove(0);
    res_str.remove(0);
    res_str = format!("{{{}}}", res_str);
    Ok(res_str)
}

fn encode_atom(input: &Atom) -> Result<String, ExecError> {
    let metatype = atom_to_string(&get_meta_type(&input));
    let space = DynSpace::new(GroundingSpace::new());
    let typ = get_atom_types(&space, &input);
    if metatype == "Expression" {
        let exp = TryInto::<&ExpressionAtom>::try_into(input)?;
        match encode_list(exp.clone().into_children()) {
            Ok(encoded_list) => Ok(encoded_list),
            Err(err) => Err(err),
        }
    }
    else if atom_to_string(&typ[0]) == "Number" {
        let number_atom_string = atom_to_string(&input);
        Ok(number_atom_string)
    }
    else if atom_to_string(&typ[0]) == "SpaceType" {
        match encode_dictspace(&input) {
            Ok(str) => Ok(str),
            Err(err) => Err(err)
        }
    }
    // String should be encoded using serde_json since it will encode "a" with additional escape
    // characters. Symbols like just 'a' are not supported by serde_json so proposition is to encode
    // them like strings.
    else {
        let atom_string = atom_to_string(&input);
        match serde_json::to_string(&atom_string) {
            Ok(encoded) => Ok(encoded),
            Err(err) => Err(ExecError::from(format!("Encode string failed: {}", err))),
        }
    }
}

fn json_encode(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("json-encode expects atom to encode as input");
    let input = args.get(0).ok_or_else(arg_error)?;

    match encode_atom(input) {
        Ok(str) => Ok(vec![Atom::gnd(Str::from_string(str))]),
        Err(err) => Err(err)
    }
}

fn decode_value(v: &Value) -> Result<Atom, ExecError> {
    match v {
        Value::Array(_) => {match decode_array(v.as_array().unwrap()) {
            Ok(atom) => Ok(atom),
            Err(err) => Err(err),
        }},
        Value::Null => Ok(Atom::gnd(Str::from_str("null"))),
        Value::Bool(_) => Ok(Atom::gnd(Bool(v.as_bool().unwrap()))),
        Value::String(_) => Ok(Atom::gnd(Str::from_string(v.to_string()))),
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
    let space = DynSpace::new(GroundingSpace::new());
    for key in keys {
        let value = dict.get(key).unwrap();
        match decode_value(value) {
            Ok(decoded) => space.borrow_mut().add(Atom::expr([Atom::gnd(Str::from_string(key.clone())), decoded])),
            Err(err) => return Err(err)
        }
    }
    Ok(Atom::gnd(space))
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
        ]));
    }
}
