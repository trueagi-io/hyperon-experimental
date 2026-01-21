use hyperon_atom::*;
use hyperon_atom::gnd::GroundedFunctionAtom;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use hyperon_atom::gnd::str::*;
use super::{grounded_op, unit_result, regex};

use std::convert::TryInto;

#[derive(Clone, Debug)]
pub struct PrintlnOp {}

grounded_op!(PrintlnOp, "println!");

impl Grounded for PrintlnOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_UNDEFINED, UNIT_TYPE])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for PrintlnOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("println! expects single atom as an argument");
        let atom = args.get(0).ok_or_else(arg_error)?;
        println!("{}", atom_to_string(atom));
        unit_result()
    }
}

#[derive(Clone, Debug)]
pub struct FormatArgsOp {}

grounded_op!(FormatArgsOp, "format-args");

use dyn_fmt::AsStrFormatExt;

impl Grounded for FormatArgsOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_EXPRESSION, ATOM_TYPE_STRING])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FormatArgsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("format-args expects format string as a first argument and expression as a second argument");
        let format = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        let res = format.format(args.as_slice());
        Ok(vec![Atom::gnd(Str::from_string(res))])
    }
}

fn sort_strings(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = "sort-strings expects expression with strings as a first argument";
    let list = TryInto::<&ExpressionAtom>::try_into(args.get(0).ok_or(arg_error)?).map_err(|_| arg_error)?;
    let mut strings = Vec::<&str>::with_capacity(list.children().len());
    for s in list.children() {
        let s = Atom::as_gnd::<Str>(s).ok_or(arg_error)?;
        strings.push(s.as_str());
    }
    strings.sort();
    let sorted: Vec::<Atom> = strings.into_iter()
        .map(|s| Atom::gnd(Str::from_string(s.into()))).collect();
    Ok(vec![Atom::expr(sorted)])
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let println_op = Atom::gnd(PrintlnOp{});
    tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
    let format_args_op = Atom::gnd(FormatArgsOp{});
    tref.register_token(regex(r"format-args"), move |_| { format_args_op.clone() });
    tref.register_token(regex(r#"(?s)^".*"$"#),
        |token| { let mut s = String::from(token); s.remove(0); s.pop(); Atom::gnd(Str::from_string(s)) });
    tref.register_function(GroundedFunctionAtom::new(
        r"sort-strings".into(), 
        expr!("->" "Expression" "Expression"),
        sort_strings
    ));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn println_op() {
        assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), unit_result());
    }
}
