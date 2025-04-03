use crate::*;
use crate::space::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use super::{grounded_op, regex, unit_result};

use std::convert::TryInto;

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
        for atom in atoms.iter() { json_space.add(atom.clone()); }
        Ok(vec![Atom::gnd(DynSpace::new(json_space))])
    }
}

#[derive(Clone, Debug)]
pub struct GetValueByKeyOp {}

grounded_op!(GetValueByKeyOp, "get-value-by-key");

impl Grounded for GetValueByKeyOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, rust_type_atom::<DynSpace>(), ATOM_TYPE_ATOM, ATOM_TYPE_UNDEFINED])
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
        let key = args.get(1).ok_or_else(arg_error)?;

        let result = space.subst(&Atom::expr([key.clone(), Atom::var("x")]), &Atom::var("x"));
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
        let arg_error = || ExecError::from("get-all-keys expects json-dict as an argument");
        let space = args.get(0).ok_or_else(arg_error)?;
        let space = Atom::as_gnd::<DynSpace>(space).ok_or("get-all-keys expects a space as the first argument")?;

        let result = space.subst(&Atom::expr([Atom::var("key"), Atom::var("x")]), &Atom::var("key"));

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metta::runner::stdlib::tests::run_program;

    #[test]
    fn metta_json_dict() {
        assert_eq!(run_program(&format!("!(json-dict key1 value1)")), Ok(vec![vec![expr!("Error" ({ JsonDictOp{} } "key1" "value1") "IncorrectNumberOfArguments")]]));
        assert_eq!(run_program(&format!("!(json-dict key1)")), Ok(vec![vec![expr!("Error" ({ JsonDictOp{} } "key1") "Atom is not an ExpressionAtom")]]));
    }

    #[test]
    fn metta_get_all_keys() {
        assert_eq!(run_program(&format!("!(assertEqual (let $jsondict (json-dict ((key1 value1) (key2 value2))) (get-all-keys $jsondict)) (superpose (key1 key2)))")), Ok(vec![vec![UNIT_ATOM]]));
        assert_eq!(run_program(&format!("!(let $jsondict (json-dict ((key1 value1) (key2 value2))) (get-all-keys key))")), Ok(vec![vec![expr!("Error" ({ GetAllKeysOp{} } "key") "get-all-keys expects a space as the first argument")]]));
    }

    #[test]
    fn metta_get_value_by_key() {
        assert_eq!(run_program(&format!("!(assertEqual (let $jsondict (json-dict ((key1 value1) (key2 value2))) (get-value-by-key $jsondict key1)) (superpose (value1)))")), Ok(vec![vec![UNIT_ATOM]]));
        assert_eq!(run_program(&format!("!(let $jsondict (json-dict ((key1 value1) (key2 value2))) (get-value-by-key key1 key2))")), Ok(vec![vec![expr!("Error" ({ GetValueByKeyOp{} } "key1" "key2") "get-value-by-key expects a space as the first argument")]]));
        assert_eq!(run_program(&format!("!(let $jsondict (json-dict ((key1 value1) (key2 value2))) (get-value-by-key key1))")), Ok(vec![vec![expr!("Error" ({ GetValueByKeyOp{} } "key1") "IncorrectNumberOfArguments")]]));
    }

    #[test]
    fn json_dict_op() {
        let res = JsonDictOp {}.execute(&mut vec![expr!("key1")]);
        assert_eq!(res, Err(ExecError::from("Atom is not an ExpressionAtom")));
    }
}


