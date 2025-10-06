use std::borrow::Cow;
use hyperon_atom::{Atom, ExecError, gnd::GroundedFunctionAtom, ExpressionAtom};
use crate::metta::{ARROW_SYMBOL, ATOM_TYPE_ATOM};
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{Metta, ModuleLoader, RunContext};
use crate::metta::runner::modules::MettaMod;
use hyperon_atom::gnd::str::ATOM_TYPE_STRING;
use hyperon_atom::gnd::str::Str;
use serde_json::Value;
use hyperon_space::ATOM_TYPE_SPACE;
use crate::metta::runner::DynSpace;
use hyperon_atom::gnd::bool::*;
use hyperon_atom::gnd::number::{Number, ATOM_TYPE_NUMBER};
use std::io::Write;

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

enum JSONError {
    IO(std::io::Error),
    Runtime(String),
}

impl From<std::io::Error> for JSONError {
    fn from(err: std::io::Error) -> Self {
        Self::IO(err)
    }
}

impl From<String> for JSONError {
    fn from(err: String) -> Self {
        Self::Runtime(err)
    }
}

impl From<&str> for JSONError {
    fn from(err: &str) -> Self {
        Self::Runtime(err.into())
    }
}

impl From<JSONError> for ExecError {
    fn from(err: JSONError) -> Self {
        match err {
            JSONError::IO(err) => ExecError::Runtime(format!("IO error: {}", err)),
            JSONError::Runtime(msg) => ExecError::Runtime(msg),
        }
    }
}

fn encode_list<W: Write>(writer: &mut W, input: &[Atom]) -> Result<(), JSONError> {
    writer.write(b"[")?;
    if !input.is_empty() {
        encode_atom(writer, input.first().unwrap())?;
        for atom in &input[1..] {
            writer.write(b", ")?;
            encode_atom(writer, atom)?;
        }
    }
    writer.write(b"]")?;
    Ok(())
}

fn extract_key_value_pair<W: Write>(writer: &mut W, atom: &Atom) -> Result<(), JSONError> {
    let res_expr = TryInto::<&ExpressionAtom>::try_into(atom)?;
    let key = res_expr.children().get(0)
        .and_then(|k| Str::from_atom(k))
        .ok_or("Key/value pair is expected, key must be a string")?
        .to_string();
    let value = res_expr.children().get(1)
        .ok_or("Key/value pair is expected")?;
    writer.write(key.as_bytes())?;
    writer.write(b":")?;
    encode_atom(writer, value)?;
    Ok(())
}

fn encode_dictspace<W: Write>(writer: &mut W, input: &Atom) -> Result<(), JSONError> {
    let space = Atom::as_gnd::<DynSpace>(input).unwrap();
    let mut result = Ok(());
    let mut first_atom = true;
    writer.write(b"{")?;
    let _ = space.borrow().visit(&mut |atom: Cow<Atom>| {
        if result.is_err() {
            return;
        }
        result = match TryInto::<&ExpressionAtom>::try_into(atom.as_ref()) {
            Ok(expr) if expr.children().len() == 2 => {
                let r = if !first_atom {
                    writer.write(b", ").map(|_| ()).map_err(|e| e.into())
                } else {
                    first_atom = false;
                    Ok(())
                };
                r.and_then(|()| extract_key_value_pair(writer, &atom))
            },
            _ => Err(JSONError::Runtime("Input space must be a dictionary space consists of key-value pairs only or empty space. Use dict-space function".into())),
        }
    });
    match result {
        Err(err) => Err(err),
        Ok(()) => {
            writer.write(b"}")?;
            Ok(())
        }
    }
}

fn encode_atom<W: Write>(writer: &mut W, input: &Atom) -> Result<(), JSONError> {
    match input {
        Atom::Grounded(gnd) => {
            let typ = gnd.type_();
            if typ == ATOM_TYPE_NUMBER {
                let val = Number::from_atom(input).unwrap();
                match val {
                    Number::Integer(i) => serde_json::to_writer(writer, &i)
                        .map_err(|err| JSONError::Runtime(format!("Encode integer failed: {}", err))),
                    Number::Float(f) => serde_json::to_writer(writer, &f)
                        .map_err(|err| JSONError::Runtime(format!("Encode float failed: {}", err))),
                }
            } else if typ == ATOM_TYPE_SPACE {
                encode_dictspace(writer, &input)
            } else if typ == ATOM_TYPE_BOOL {
                let val = Bool::from_atom(input).unwrap();
                serde_json::to_writer(writer, &val.0)
                    .map_err(|err| JSONError::Runtime(format!("Encode bool failed: {}", err)))
            } else if typ == ATOM_TYPE_STRING {
                let val = Str::from_atom(input).unwrap();
                serde_json::to_writer(writer, val.as_str())
                    .map_err(|err| JSONError::Runtime(format!("Encode string failed: {}", err)))
            } else {
                Err(JSONError::Runtime(format!("Encode failed for atom (unsupported type): {:?}", input)))
            }
        },
        Atom::Expression(exp) => encode_list(writer, exp.children()),
        // Symbols will use additional sym!: prefix to separate them from regular strings. null is a
        // symbol too, but it should remain "null" after encoding so decoder will see it like Value::Null instance.
        Atom::Symbol(sym) if sym.name() == "null" => {
            writer.write(b"null").map(|_| ()).map_err(|e| e.into())
        },
        Atom::Symbol(sym) => {
            let sym_name = "sym!:".to_string() + sym.name();
            serde_json::to_writer(writer, &sym_name)
                .map_err(|err| JSONError::Runtime(format!("Encode symbol failed: {}", err)))
        },
        Atom::Variable(var) => {
            let var_name = "var!:".to_string() + &var.name();
            serde_json::to_writer(writer, &var_name)
                .map_err(|err| JSONError::Runtime(format!("Encode variable failed: {}", err)))
        },
    }
}

fn json_encode(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("json-encode expects atom to encode as input");
    let input = args.get(0).ok_or_else(arg_error)?;
    let mut buffer: Vec<u8> = Vec::new();
    encode_atom(&mut buffer, input)?;
    let json = String::from_utf8(buffer)
        .map_err(|err| ExecError::Runtime(format!("Encode atom failed: {}", err)))?;
    Ok(vec![Atom::gnd(Str::from_string(json))])
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
            else if decoded_string.starts_with("\"var!:") {
                let slice = &decoded_string[6..decoded_string.len() - 1];
                Ok(Atom::var(slice))
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
    let input = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;
    match &serde_json::from_str(input.as_str()) {
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
            !(assertEqual (json-decode 5) (Error (json-decode 5) (BadArgType 1 String Number)))
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
            !(assertEqual (let $encoded (json-encode $x) (json-decode $encoded)) $x)
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
            vec![UNIT_ATOM],
        ]));
    }

    #[cfg(feature="benchmark")]
    mod benchmarks {
        extern crate test;

        use test::Bencher;
        use hyperon_atom::Atom;
        use hyperon_space::DynSpace;
        use crate::space::grounding::*;
        use hyperon_atom::gnd::str::Str;
        use super::super::json_encode;

        #[bench]
        fn bench_json_encode(bencher: &mut Bencher) {
            let mut space = GroundingSpace::new();
            for _i in 1..1000 {
                space.add(Atom::expr([Atom::gnd(Str::from_str("some-key")), Atom::sym("some-value")]));
            }
            let space = Atom::gnd(DynSpace::new(space));
            bencher.iter(|| {
                let result = json_encode(&[space.clone()]);
                assert!(result.is_ok());
            })
        }
    }
}

