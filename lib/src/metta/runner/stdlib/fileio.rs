use crate::{Atom, CustomExecute, ExecError, Grounded};
use crate::metta::{ARROW_SYMBOL, UNIT_ATOM};
use crate::metta::runner::stdlib::{grounded_op, regex, unit_result};
use crate::metta::runner::str::{Str, ATOM_TYPE_STRING};
use std::fs;
use crate::metta::text::Tokenizer;

#[derive(Clone, Debug)]
pub struct FileReadOp {}

grounded_op!(FileReadOp, "file-read");

impl Grounded for FileReadOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_STRING])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FileReadOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("file-read expects filepath in form of string atom as an argument");
        let filepath = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;

        match fs::read_to_string(filepath.as_str()) {
            Ok(message) => Ok(vec![Atom::gnd(Str::from_string(message))]),
            Err(_) => Err(ExecError::from("FailedToReadPath")),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileWriteOp {}

grounded_op!(FileWriteOp, "file-write");

impl Grounded for FileWriteOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_STRING, UNIT_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FileWriteOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("file-write expects filepath (string atom) and content (string atom) as an arguments");
        let filepath = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;
        let content = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

        match fs::write(filepath.as_str(), content.as_str()) {
            Ok(_) => unit_result(),
            Err(_) => Err(ExecError::from("FailedToWritePath")),
        }
    }
}

pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let file_read_op = Atom::gnd(FileReadOp {});
    tref.register_token(regex(r"file-read"), move |_| { file_read_op.clone() });
    let file_write_op = Atom::gnd(FileWriteOp {});
    tref.register_token(regex(r"file-write"), move |_| { file_write_op.clone() });
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::env;
    use crate::metta::runner::stdlib::tests::run_program;
    use crate::metta::UNIT_ATOM;

    #[test]
    fn metta_file_read() {
        assert_eq!(run_program("!(assertEqual (file-read \"non-existent.file\") (Error (file-read \"non-existent.file\") FailedToReadPath))"),
                   Ok(vec![vec![UNIT_ATOM]]));
        let curdir = env::current_dir().unwrap();
        env::set_current_dir(env::temp_dir()).unwrap();
        fs::write("temp.txt", "check read").expect("Can't write to temp folder");
        assert_eq!(run_program("!(assertEqual (file-read \"temp.txt\") \"check read\")"),
                   Ok(vec![vec![UNIT_ATOM]]));
        fs::remove_file("temp.txt").expect("Can't remove temp file");
        env::set_current_dir(curdir).unwrap();
    }

    #[test]
    fn metta_file_write() {
        assert_eq!(run_program("!(assertEqual (file-write \"non/existent/folder/tmp.txt\" \"content\") (Error (file-write \"non/existent/folder/tmp.txt\" \"content\") FailedToWritePath))"),
                   Ok(vec![vec![UNIT_ATOM]]));
        let curdir = env::current_dir().unwrap();
        env::set_current_dir(env::temp_dir()).unwrap();
        let _ = run_program("!(file-write \"temp.txt\" \"check read\")");
        let tmp_read = fs::read_to_string("temp.txt").unwrap();
        assert_eq!(tmp_read.as_str(), "check read");
        fs::remove_file("temp.txt").expect("Can't remove temp file");
        env::set_current_dir(curdir).unwrap();
    }
}