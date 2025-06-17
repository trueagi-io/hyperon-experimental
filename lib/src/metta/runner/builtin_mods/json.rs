use hyperon_atom::{Atom, ExecError, gnd::GroundedFunctionAtom, ExpressionAtom, SymbolAtom};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_ATOM};
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, ModuleLoader, RunContext};
use crate::metta::runner::modules::MettaMod;
use crate::metta::runner::str::{atom_to_string, unescape, ATOM_TYPE_STRING};
use crate::metta::runner::str::Str;
use serde_json::Value;
use hyperon_space::ATOM_TYPE_SPACE;
use crate::metta::runner::DynSpace;
use crate::metta::runner::bool::*;
use crate::metta::runner::number::{Number, ATOM_TYPE_NUMBER};
use crate::metta::types::get_atom_types;

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

fn encode_list(input: Vec<Atom>) -> Result<String, ExecError> {
    let mut res_str = "[".to_string();
    let first_atom = input.first().unwrap();
    match encode_atom(first_atom) {
        Ok(encoded) => {res_str.push_str(&encoded);}
        Err(err) => return Err(err),
    }
    for atom in input[1..].iter() {
        match encode_atom(&atom) {
            Ok(encoded) => {res_str = res_str + ", " + &encoded;},
            Err(err) => return Err(err),
        }
    }
    res_str.push_str("]");
    Ok(res_str)
}

fn extract_key_value_pair (atom: Atom) -> Result<String, ExecError> {
    let res_expr = TryInto::<&ExpressionAtom>::try_into(&atom)?;
    let key = match Str::from_atom(res_expr.children().get(0).unwrap()).ok_or_else(|| ExecError::from("Key must be a string")){
        Ok(str) => str.to_string(),
        Err(err) => return Err(err),
    };
    let value = res_expr.children().get(1).unwrap();
    let value_str = encode_atom(value)?;
    Ok(key + ": " + &value_str)
}

fn encode_dictspace(input: &Atom) -> Result<String, ExecError> {
    let space = Atom::as_gnd::<DynSpace>(input).unwrap();

    let result = space.borrow().subst(&Atom::expr([Atom::var("k"), Atom::var("v")]),
                                      &Atom::expr([Atom::var("k"), Atom::var("v")]));
    if result.len() == 0 {
        return Err(ExecError::from("Input space must be a dictionary space which contains (key value) pairs. See dict-space function"));
    }
    let mut res_str = "{".to_string();
    let first_atom = result.first().unwrap();
    match extract_key_value_pair(first_atom.clone()) {
        Ok(encoded_key_value) => res_str.push_str(&encoded_key_value),
        Err(err) => return Err(err),
    }
    for res_atom in result[1..].iter() {
        match extract_key_value_pair(res_atom.clone()) {
            Ok(encoded_key_value) => {res_str = res_str + ", " + &encoded_key_value},
            Err(err) => return Err(err),
        }
    }
    res_str.push_str("}");
    Ok(res_str)
}

fn encode_atom(input: &Atom) -> Result<String, ExecError> {
    let space = DynSpace::new(GroundingSpace::new());
    let typ = get_atom_types(&space, &input);
    let exp = TryInto::<&ExpressionAtom>::try_into(input);
    let sym = TryInto::<&SymbolAtom>::try_into(input);
    if exp.is_ok() {
        match encode_list(exp?.clone().into_children()) {
            Ok(encoded_list) => Ok(encoded_list),
            Err(err) => Err(err),
        }
    }
    else if typ[0] == ATOM_TYPE_NUMBER {
        let number_atom_string = atom_to_string(&input);
        Ok(number_atom_string)
    }
    else if typ[0] == ATOM_TYPE_SPACE {
        match encode_dictspace(&input) {
            Ok(str) => Ok(str),
            Err(err) => Err(err)
        }
    }
    else if typ[0] == ATOM_TYPE_BOOL {
        let bool_atom = Bool::from_atom(&input).unwrap();
        match serde_json::to_string(&bool_atom.0) {
            Ok(encoded) => Ok(encoded),
            Err(err) => Err(ExecError::from(format!("Encode bool failed: {}", err))),
        }
    }
    // Symbols will use additional sym!: prefix to separate them from regular strings. null is a
    // symbol too, but it should remain "null" after encoding so decoder will see it like Value::Null instance.
    else if sym.is_ok() {
        let mut sym_name = sym.unwrap().name().to_string();
        if sym_name == "null" {
            Ok("null".to_string())
        }
        else {
            sym_name = "sym!:".to_string() + &sym_name;
            match serde_json::to_string(&sym_name) {
                Ok(encoded) => Ok(encoded),
                Err(err) => Err(ExecError::from(format!("Encode symbol failed: {}", err))),
            }
        }
    }
    // String should be encoded using serde_json since it will encode "a" with additional escape
    // characters.
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
        Value::Null => Ok(Atom::sym("null")),
        Value::Bool(_) => Ok(Atom::gnd(Bool(v.as_bool().unwrap()))),
        Value::String(_) => {
            let decoded_string = v.to_string();
            if decoded_string.contains("sym!:") {
                Ok(Atom::sym(&unescape(&decoded_string.replace("sym!:", "")).unwrap()))
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
        ]));
    }
}
