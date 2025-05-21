use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

pub static JSON_METTA: &'static str = include_str!("json.metta");
pub const ATOM_TYPE_JSON_HANDLE: Atom = sym!("JsonHandle");

#[derive(Clone, Debug)]
pub struct JsonHandle(Rc<RefCell<fs::File>>);