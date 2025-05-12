use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use crate::{sym, Atom, ExecError, Grounded};
use crate::metta::{ARROW_SYMBOL, UNIT_ATOM};
use crate::metta::runner::stdlib::unit_result;
use crate::metta::runner::str::{Str, ATOM_TYPE_STRING};
use std::fs;
use std::fs::OpenOptions;
use std::io::{Read, Seek, SeekFrom, Write};
use std::rc::Rc;
use crate::space::grounding::GroundingSpace;
use crate::metta::text::SExprParser;
use crate::metta::runner::{ModuleLoader, RunContext, DynSpace, Metta, MettaMod};
use crate::atom::gnd::*;

pub static FILEIO_METTA: &'static str = include_str!("fileio.metta");
pub const ATOM_TYPE_FILE_HANDLER : Atom = sym!("FileHandle");

#[derive(Clone, Debug)]
pub struct FileHandle(Rc<RefCell<fs::File>>);

impl FileHandle {
    fn open(path: &str, options: &str) -> Result<Self, ExecError> {

        let mut opened_file= &mut OpenOptions::new();
        if options.contains("r") { opened_file = opened_file.read(true); }
        else {opened_file = opened_file.read(false);}

        if options.contains("w") { opened_file = opened_file.write(true); }
        else {opened_file = opened_file.write(false);}

        if options.contains("c") { opened_file = opened_file.create(true); }
        else {opened_file = opened_file.create(false);}

        if options.contains("a") { opened_file = opened_file.append(true); }
        else {opened_file = opened_file.truncate(true).append(false);}

        match opened_file.open(path)
        {
            Ok(file) => Ok(Self(Rc::new(RefCell::new(file)))),
            Err(_) => Err(ExecError::from("Failed to open file with provided arguments"))
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

impl Grounded for FileHandle {
    fn type_(&self) -> Atom {
        ATOM_TYPE_FILE_HANDLER
    }
}

impl Display for FileHandle {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "FileHandle-{:?}", self.0.as_ptr())
    }
}

impl PartialEq for FileHandle {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

#[derive(Debug)]
pub(crate) struct FileioModLoader;

impl ModuleLoader for FileioModLoader {
    fn load(&self, context: &mut RunContext) -> Result<(), String> {

        let space = DynSpace::new(GroundingSpace::new());
        context.init_self_module(space, None);

        let _ = self.load_tokens(context.module(), context.metta.clone())?;

        let parser = SExprParser::new(FILEIO_METTA);
        context.push_parser(Box::new(parser));

        Ok(())
    }

    fn load_tokens(&self, target: &MettaMod, _metta: Metta) -> Result<(), String> {
        let mut tref = target.tokenizer().borrow_mut();

        tref.register_function(GroundedFunctionAtom::new(
            r"file-open".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_STRING, ATOM_TYPE_FILE_HANDLER]),
            file_open));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-read".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLER, ATOM_TYPE_STRING]),
            file_read));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-write".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLER, ATOM_TYPE_STRING, UNIT_ATOM]),
            file_write));

        Ok(())
    }
}

fn file_open(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-open expects filepath and options in form of string atoms as an argument");
    let filepath = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;
    let options = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

    let fhandler = FileHandle::open(filepath.as_str(), options.as_str())?;
    Ok(vec![Atom::gnd(fhandler)])
}

fn file_read(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-read expects filehandle as an argument");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-read expects filehandle as an argument")?;

    let message = filehandle.read_as_str();
    Ok(vec![Atom::gnd(Str::from_string(message))])
}

fn file_write(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-write expects filehandle and content (string atom) as an arguments");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-write expects filehandle and content (string atom) as an arguments")?;
    let content = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

    filehandle.write(content.as_str());
    unit_result()
}

// pub(super) fn register_context_independent_tokens(tref: &mut Tokenizer) {
//     let file_create_op = Atom::gnd(FileCreateOp {});
//     tref.register_token(regex(r"file-create"), move |_| { file_create_op.clone() });
//     let file_open_op = Atom::gnd(FileOpenOp {});
//     tref.register_token(regex(r"file-open"), move |_| { file_open_op.clone() });
//     let file_read_op = Atom::gnd(FileReadOp {});
//     tref.register_token(regex(r"file-read"), move |_| { file_read_op.clone() });
//     let file_read_oo_op = Atom::gnd(FileReadOOOp {});
//     tref.register_token(regex(r"file-readOO"), move |_| { file_read_oo_op.clone() });
//     let file_write_op = Atom::gnd(FileWriteOp {});
//     tref.register_token(regex(r"file-write"), move |_| { file_write_op.clone() });
// }

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