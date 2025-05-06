use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use crate::{sym, Atom, CustomExecute, ExecError, Grounded};
use crate::metta::{ARROW_SYMBOL, UNIT_ATOM};
use crate::metta::runner::stdlib::{grounded_op, regex, unit_result};
use crate::metta::runner::str::{Str, ATOM_TYPE_STRING};
use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};
use std::rc::Rc;
use crate::metta::text::Tokenizer;

pub const ATOM_TYPE_FILE_HANDLER : Atom = sym!("FileHandler");

#[derive(Clone, Debug)]
pub struct FileHandler(Rc<RefCell<fs::File>>);

impl FileHandler {
    fn create(path: &str) -> Result<Self, ExecError> {
        match fs::File::create_new(path)
        {
            Ok(f) => Ok(Self(Rc::new(RefCell::new(f)))),
            Err(msg) => Err(ExecError::from(msg.to_string()))
        }
    }

    fn read_as_str(&self) -> String
    {
        self.0.borrow_mut().seek(SeekFrom::Start(0)).unwrap();
        let mut contents = String::new();
        let _ = self.0.borrow_mut().read_to_string(&mut contents);
        contents
    }

    fn write(&self, content: &str) -> ()
    {
        let _ = self.0.borrow_mut().write(&content.as_bytes());
        ()
    }
}

impl Grounded for FileHandler {
    fn type_(&self) -> Atom {
        ATOM_TYPE_FILE_HANDLER
    }
}

impl Display for FileHandler {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "FileHandler-{:?}", self.0.as_ptr())
    }
}

impl PartialEq for FileHandler {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Clone, Debug)]
pub struct FileCreateOp {}

grounded_op!(FileCreateOp, "file-create");

impl Grounded for FileCreateOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_FILE_HANDLER])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FileCreateOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("file-create expects filepath in form of string atom as an argument");
        let filepath = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;

        match FileHandler::create(filepath.as_str()) {
            Ok(fhandler) => Ok(vec![Atom::gnd(fhandler)]),
            Err(msg) => Err(msg)
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileReadOp {}

grounded_op!(FileReadOp, "file-read");

impl Grounded for FileReadOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLER, ATOM_TYPE_STRING])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FileReadOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("file-read expects filehandler (from file-open) as an argument");
        let filehandler = args.get(0).ok_or_else(arg_error)?.into();
        let filehandler = Atom::as_gnd::<FileHandler>(filehandler).ok_or("file-read expects filehandler (from file-open) as an argument")?;

        // let mut contents = String::new();
        // let mut file = fs::File::open("tmp.txt").unwrap();
        // let _ = file.read_to_string(&mut contents);
        let message = filehandler.read_as_str();
        Ok(vec![Atom::gnd(Str::from_string(message))])
    }
}

#[derive(Clone, Debug)]
pub struct FileWriteOp {}

grounded_op!(FileWriteOp, "file-write");

impl Grounded for FileWriteOp {
    fn type_(&self) -> Atom {
        Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLER, ATOM_TYPE_STRING, UNIT_ATOM])
    }

    fn as_execute(&self) -> Option<&dyn CustomExecute> {
        Some(self)
    }
}

impl CustomExecute for FileWriteOp {
    fn execute(&self, args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
        let arg_error = || ExecError::from("file-write expects filehandler and content (string atom) as an arguments");
        let filehandler = args.get(0).ok_or_else(arg_error)?.into();
        let filehandler = Atom::as_gnd::<FileHandler>(filehandler).ok_or("file-write expects filehandler and content (string atom) as an arguments")?;
        let content = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

        filehandler.write(content.as_str());
        unit_result()
    }
}


pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
    let file_create_op = Atom::gnd(FileCreateOp {});
    tref.register_token(regex(r"file-create"), move |_| { file_create_op.clone() });
    let file_read_op = Atom::gnd(FileReadOp {});
    tref.register_token(regex(r"file-read"), move |_| { file_read_op.clone() });
    let file_write_op = Atom::gnd(FileWriteOp {});
    tref.register_token(regex(r"file-write"), move |_| { file_write_op.clone() });
}

// #[cfg(test)]
// mod tests {
//     use std::fs;
//     use std::env;
//     use crate::metta::runner::stdlib::tests::run_program;
//     use crate::metta::UNIT_ATOM;
//
//     #[test]
//     fn metta_file_read() {
//         assert_eq!(run_program("!(assertEqual (file-read \"non-existent.file\") (Error (file-read \"non-existent.file\") FailedToReadPath))"),
//                    Ok(vec![vec![UNIT_ATOM]]));
//         let curdir = env::current_dir().unwrap();
//         env::set_current_dir(env::temp_dir()).unwrap();
//         fs::write("temp.txt", "check read").expect("Can't write to temp folder");
//         assert_eq!(run_program("!(assertEqual (file-read \"temp.txt\") \"check read\")"),
//                    Ok(vec![vec![UNIT_ATOM]]));
//         fs::remove_file("temp.txt").expect("Can't remove temp file");
//         env::set_current_dir(curdir).unwrap();
//     }
//
//     #[test]
//     fn metta_file_write() {
//         assert_eq!(run_program("!(assertEqual (file-write \"non/existent/folder/tmp.txt\" \"content\") (Error (file-write \"non/existent/folder/tmp.txt\" \"content\") FailedToWritePath))"),
//                    Ok(vec![vec![UNIT_ATOM]]));
//         let curdir = env::current_dir().unwrap();
//         env::set_current_dir(env::temp_dir()).unwrap();
//         let _ = run_program("!(file-write \"temp.txt\" \"check read\")");
//         let tmp_read = fs::read_to_string("temp.txt").unwrap();
//         assert_eq!(tmp_read.as_str(), "check read");
//         fs::remove_file("temp.txt").expect("Can't remove temp file");
//         env::set_current_dir(curdir).unwrap();
//     }
// }