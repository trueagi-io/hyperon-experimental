use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use super::{grounded_op, regex};

use std::convert::TryInto;
use itertools::Itertools;
use crate::common::collections::CowArray;
use crate::common::collections::ImmutableString::Allocated;
use crate::metta::runner::number::Number;
use crate::metta::runner::stdlib::core::MatchOp;
use crate::metta::runner::str::{Str, ATOM_TYPE_STRING};

#[derive(Clone, Debug)]
pub struct JsonDictOp {}

grounded_op!(JsonDictOp, "json-dict");

impl Grounded for JsonDictOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, rust_type_atom::<DynSpace>()])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for JsonDictOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("json-dict expects expression as an argument");
        let expr = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or_else(arg_error)?)?;

        let atoms = expr.children();
        let mut json_space = GroundingSpace::new();
        for atom in atoms.iter() {
            let first: Atom = atom.iter().get(..1).collect();
            let rest: Atom = atom.iter().get(1..).collect();
            println!("first {:?}", first);
            println!("rest {:?}", rest);
            for sub_atom in atom.iter()
            {

            }
            json_space.add(atom.clone());
        }
        Ok(vec![Atom::gnd(DynSpace::new(json_space))])
    }
}

#[derive(Clone, Debug)]
pub struct GetValueByKeyOp {}

grounded_op!(GetValueByKeyOp, "get-value-by-key");

impl Grounded for GetValueByKeyOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_STRING, ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetValueByKeyOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-value-by-key expects json-dict space and key as input");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-value-by-key expects a space as the first argument")?;
        let key = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

        let str2 = "key1";

        let sym = sym!("key1");
        // println!("{:?}", key.clone());
        println!("{:?}", expr!("key1" x));
        println!("{:?}", expr!({2} {5}));
        // println!("{:?}", expr!({"key1"} x));
        // println!("{:?}", expr!({str2} x));
        println!("{:?}", expr!({key.clone()} x));
        // println!("{:?}", expr!({sym.clone()} x));
        // println!("{:?}", expr!({format!("{}", "key1")} x));
        println!("{:?}", space.subst(&expr!("key1" x), &expr!(x)));
        println!("{:?}", space.subst(&expr!({"key1"} x), &expr!(x)));
        println!("{:?}", space.subst(&expr!({str2} x), &expr!(x)));
        // println!("{:?}", space.subst(&expr!({key.clone()} x), &expr!(x)));
        // println!("{:?}", space.subst(&expr!({sym.clone()} x), &expr!(x)));
        // println!("{:?}", space.subst(&expr!({format!("{}", sym)} x), &expr!(x)));
        let match_op = MatchOp{};
        println!("{:?}", match_op.execute(&mut vec![expr!({space.clone()}), expr!({str2} x), expr!(x)]));

        let mut result = Vec::new();
        space.borrow().as_space().visit(&mut |atom: std::borrow::Cow<Atom>| {
            result.push(make_variables_unique(atom.into_owned()))
        });
        println!("{:?}", result);

        let result = space.subst(&expr!({str2} x), &expr!(x));



        Ok(result)
    }
}

#[derive(Clone, Debug)]
pub struct GetAllKeysOp {}

grounded_op!(GetAllKeysOp, "get-all-keys");

impl Grounded for GetAllKeysOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_UNDEFINED])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for GetAllKeysOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("get-all-keys expects json-dict space as an argument");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-all-keys expects a space as the first argument")?;

        let result = space.subst(&expr!(key value), &expr!(key));

        Ok(result)
    }
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let json_dict_op = Atom::gnd(JsonDictOp {});
    tref.register_token(regex(r"json-dict"), move |_| { json_dict_op.clone() });
    let get_value_by_key_op = Atom::gnd(GetValueByKeyOp {});
    tref.register_token(regex(r"get-value-by-key"), move |_| { get_value_by_key_op.clone() });
    let get_all_keys_op = Atom::gnd(GetAllKeysOp {});
    tref.register_token(regex(r"get-all-keys"), move |_| { get_all_keys_op.clone() });
}