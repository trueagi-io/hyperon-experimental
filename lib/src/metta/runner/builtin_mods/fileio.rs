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
use crate::metta::runner::number::{Number, ATOM_TYPE_NUMBER};

pub static FILEIO_METTA: &'static str = include_str!("fileio.metta");
pub const ATOM_TYPE_FILE_HANDLE: Atom = sym!("FileHandle");

#[derive(Clone, Debug)]
pub struct FileHandle(Rc<RefCell<fs::File>>);

impl FileHandle {
    fn open(path: &str, options: &str) -> Result<Self, ExecError> {

        let mut opened_file = OpenOptions::new();
        let opened_file= opened_file
            .read(options.contains("r"))
            .write(options.contains("w"))
            .create(options.contains("c"))
            .append(options.contains("a"))
            .truncate(options.contains("t"));

        match opened_file.open(path)
        {
            Ok(file) => Ok(Self(Rc::new(RefCell::new(file)))),
            Err(_) => Err(ExecError::from(format!("Failed to open file with provided path={} and options={}", path, options)))
        }
    }

    fn read_to_string(&self) -> Result<String, ExecError>
    {
        let mut contents = String::new();
        match self.0.borrow_mut().read_to_string(&mut contents) {
            Ok(_) => Ok(contents),
            Err(message) => Err(ExecError::from(format!("Failed to read file contents: {}", message)))
        }
    }

    fn write(&self, content: &str) -> Result<(), ExecError>
    {
        match self.0.borrow_mut().write(&content.as_bytes()) {
            Ok(_) => Ok(()),
            Err(message) => Err(ExecError::from(format!("Failed to write content to file: {}", message)))
        }
    }

    fn seek(&self, pos: SeekFrom) -> Result<(), ExecError>
    {
        match self.0.borrow_mut().seek(pos) {
            Ok(_) => Ok(()),
            Err(message) => Err(ExecError::from(format!("Seek operation failed: {}", message)))
        }
    }

    fn read_exact (&self, mut buf: Vec<u8>) -> Result<String, ExecError>
    {
        match self.0.borrow_mut().read(&mut *buf) {
            Ok(_) => {match String::from_utf8(buf) {
                Ok(res_str) => Ok(res_str),
                Err(msg) => Err(ExecError::from(format!("Read exact failed: {}", msg)))
            }},
            Err(message) => Err(ExecError::from(format!("Read exact failed: {}", message)))
        }
    }
}

impl Grounded for FileHandle {
    fn type_(&self) -> Atom {
        ATOM_TYPE_FILE_HANDLE
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
            r"file-open!".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_STRING, ATOM_TYPE_STRING, ATOM_TYPE_FILE_HANDLE]),
            file_open));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-read-to-string!".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLE, ATOM_TYPE_STRING]),
            file_read_to_string));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-write!".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLE, ATOM_TYPE_STRING, UNIT_ATOM]),
            file_write));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-seek!".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLE, ATOM_TYPE_NUMBER, UNIT_ATOM]),
            file_seek));

        tref.register_function(GroundedFunctionAtom::new(
            r"file-read-exact!".into(),
            Atom::expr([ARROW_SYMBOL, ATOM_TYPE_FILE_HANDLE, ATOM_TYPE_NUMBER, UNIT_ATOM]),
            file_read_exact));

        Ok(())
    }
}

fn file_open(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-open! expects filepath and options in form of string atoms as an argument");
    let filepath = args.get(0).and_then(Str::from_atom).ok_or_else(arg_error)?;
    let options = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

    let fhandler = FileHandle::open(filepath.as_str(), options.as_str())?;
    Ok(vec![Atom::gnd(fhandler)])
}

fn file_read_to_string(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-read-to-string! expects filehandle as an argument");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-read! expects filehandle as an argument")?;

    let message = filehandle.read_to_string();
    Ok(vec![Atom::gnd(Str::from_string(message?))])
}

fn file_write(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-write! expects filehandle and content (string atom) as an arguments");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-write! expects filehandle and content (string atom) as an arguments")?;
    let content = args.get(1).and_then(Str::from_atom).ok_or_else(arg_error)?;

    filehandle.write(content.as_str()).expect("Failed to write file");
    unit_result()
}

fn file_seek(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-seek! expects filehandle and start byte (number) as an arguments");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-seek! expects filehandle and start byte (number) as an arguments")?;
    let seek_from: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();
    let _ = filehandle.seek(SeekFrom::Start(seek_from as u64));
    unit_result()
}

fn file_read_exact(args: &[Atom]) -> Result<Vec<Atom>, ExecError> {
    let arg_error = || ExecError::from("file-read-exact! expects filehandle and number of bytes to read (number) as an arguments");
    let filehandle = args.get(0).ok_or_else(arg_error)?.into();
    let filehandle = Atom::as_gnd::<FileHandle>(filehandle).ok_or("file-read-exact! expects filehandle and number of bytes to read (number) as an arguments")?;
    let num_of_bytes: i64 = args.get(1).and_then(Number::from_atom).ok_or_else(arg_error)?.into();

    let buf = vec![0u8; num_of_bytes as usize];

    let res = filehandle.read_exact(buf);

    Ok(vec![Atom::gnd(Str::from_string(res?))])
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use crate::metta::*;
    use crate::metta::runner::run_program;
    use rand::{distr::Alphanumeric, Rng};

    #[test]
    fn test_filehandle() {

        let filename: String = rand::rng()
            .sample_iter(&Alphanumeric)
            .take(7)
            .map(char::from)
            .collect();

        let filename = Path::new("..").join(std::env::temp_dir()).join(format!("{}.txt", filename));
        let filename = filename.to_str().unwrap().replace("\\", "\\\\");

        let program = format!("
            !(import! &self fileio)
            !(bind! &fhandle (file-open! \"{}\" \"rwc\"))
            !(file-write! &fhandle \"check write/read\")
            !(file-seek! &fhandle 0)
            !(assertEqual (file-read-to-string! &fhandle) \"check write/read\")
            !(file-seek! &fhandle 4)
            !(assertEqual (file-read-to-string! &fhandle) \"k write/read\")
            !(file-seek! &fhandle 0)
            !(assertEqual (file-read-exact! &fhandle 5) \"check\")
        ", filename);

        let res = run_program(program.as_str());

        std::fs::remove_file(filename).expect("File not removed");

        assert_eq!(res, Ok(vec![
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM],
            vec![UNIT_ATOM]
        ]));
    }
}