use crate::*;
use crate::metta::*;
use crate::metta::text::Tokenizer;
use crate::metta::runner::str::*;
use super::{grounded_op, atom_to_string, unit_result, regex};

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
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_ATOM, ATOM_TYPE_EXPRESSION, ATOM_TYPE_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FormatArgsOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("format-args expects format string as a first argument and expression as a second argument");
        let format = atom_to_string(args.get(0).ok_or_else(arg_error)?);
        let args = TryInto::<&ExpressionAtom>::try_into(args.get(1).ok_or_else(arg_error)?)?;
        let args: Vec<String> = args.children().iter()
            .map(|atom| atom_to_string(atom))
            .collect();
        let res = format.format(args.as_slice());
        Ok(vec![Atom::gnd(Str::from_string(res))])
    }
}

pub fn register_runner_tokens(tref: &mut Tokenizer) {
    let println_op = Atom::gnd(PrintlnOp{});
    tref.register_token(regex(r"println!"), move |_| { println_op.clone() });
    let format_args_op = Atom::gnd(FormatArgsOp{});
    tref.register_token(regex(r"format-args"), move |_| { format_args_op.clone() });
}

pub fn register_rust_stdlib_tokens(tref: &mut Tokenizer) {
    tref.register_token(regex(r#"(?s)^".*"$"#),
        |token| { let mut s = String::from(token); s.remove(0); s.pop(); Atom::gnd(Str::from_string(s)) });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn println_op() {
        assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), unit_result());
    }
}
