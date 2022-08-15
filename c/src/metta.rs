use hyperon::metta::text::*;
use hyperon::metta::interpreter;
use hyperon::metta::interpreter::InterpretedAtom;
use hyperon::common::plan::StepResult;

use crate::util::*;
use crate::atom::*;
use crate::space::*;

use std::os::raw::*;
use regex::Regex;

// Tokenizer

pub struct tokenizer_t {
    tokenizer: Tokenizer, 
}

#[no_mangle]
pub extern "C" fn tokenizer_new() -> *mut tokenizer_t {
    Box::into_raw(Box::new(tokenizer_t{ tokenizer: Tokenizer::new() })) 
}

#[no_mangle]
pub unsafe extern "C" fn tokenizer_free(tokenizer: *mut tokenizer_t) {
    drop(Box::from_raw(tokenizer)); 
}

type atom_constr_t = extern "C" fn(*const c_char, *mut c_void) -> *mut atom_t;

#[repr(C)]
pub struct droppable_t {
    ptr: *mut c_void,
    free: Option<extern "C" fn(ptr: *mut c_void)>,
}

impl Drop for droppable_t {
    fn drop(&mut self) {
        let free = (*self).free;
        if let Some(free) = free {
            free(self.ptr);
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn tokenizer_register_token(tokenizer: *mut tokenizer_t,
    regex: *const c_char, constr: atom_constr_t, context: droppable_t) {
    let regex = Regex::new(cstr_as_str(regex)).unwrap();
    (*tokenizer).tokenizer.register_token(regex, move |token| {
        let catom = Box::from_raw(constr(str_as_cstr(token).as_ptr(), context.ptr));
        catom.atom
    });
}

#[no_mangle]
pub extern "C" fn tokenizer_clone(tokenizer: *const tokenizer_t) -> *mut tokenizer_t {
    let tokenizer = unsafe { (*tokenizer).tokenizer.clone() };
    Box::into_raw(Box::new(tokenizer_t{ tokenizer })) 
}

// SExprParser

pub struct sexpr_parser_t<'a> {
    parser: SExprParser<'a>, 
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_parser_new<'a>(text: *const c_char) -> *mut sexpr_parser_t<'a> {
    Box::into_raw(Box::new(sexpr_parser_t{ parser: SExprParser::new(cstr_as_str(text)) }))
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_parser_free(parser: *mut sexpr_parser_t) {
    drop(Box::from_raw(parser)) 
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_parser_parse(parser: *mut sexpr_parser_t,
        tokenizer: *const tokenizer_t) -> *mut atom_t {
    (*parser).parser.parse(&(*tokenizer).tokenizer)
        .map_or(std::ptr::null_mut(), |atom| { atom_to_ptr(atom) })
}

// SExprSpace

pub struct sexpr_space_t {
    space: SExprSpace, 
}

#[no_mangle]
pub extern "C" fn sexpr_space_new(tokenizer: *mut tokenizer_t) -> *mut sexpr_space_t {
    let tokenizer = unsafe{ Box::from_raw(tokenizer) };
    Box::into_raw(Box::new(sexpr_space_t{ space: SExprSpace::new(tokenizer.tokenizer) })) 
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_free(space: *mut sexpr_space_t) {
    drop(Box::from_raw(space)) 
}

// TODO: think how to return the result string in case of error
#[no_mangle]
pub unsafe extern "C" fn sexpr_space_add_str(space: *mut sexpr_space_t, text: *const c_char) -> bool {
    Ok(()) == (*space).space.add_str(cstr_as_str(text))
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_into_grounding_space(sexpr: *const sexpr_space_t,
        gnd: *mut grounding_space_t) {
    (*sexpr).space.into_grounding_space(&mut (*gnd).space);
}

#[no_mangle] pub static ATOM_TYPE_UNDEFINED: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_UNDEFINED };
#[no_mangle] pub static ATOM_TYPE_TYPE: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_TYPE };
#[no_mangle] pub static ATOM_TYPE_ATOM: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_ATOM };
#[no_mangle] pub static ATOM_TYPE_SYMBOL: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_SYMBOL };
#[no_mangle] pub static ATOM_TYPE_VARIABLE: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_VARIABLE };
#[no_mangle] pub static ATOM_TYPE_EXPRESSION: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_EXPRESSION };
#[no_mangle] pub static ATOM_TYPE_GROUNDED: &atom_t = &atom_t{ atom: hyperon::metta::ATOM_TYPE_GROUNDED };

#[no_mangle]
pub unsafe extern "C" fn check_type(space: *const grounding_space_t, atom: *const atom_t, typ: *const atom_t) -> bool {
    hyperon::metta::types::check_type(&(*space).space, &(*atom).atom, &(*typ).atom)
}

#[no_mangle]
pub unsafe extern "C" fn validate_atom(space: *const grounding_space_t, atom: *const atom_t) -> bool {
    hyperon::metta::types::validate_atom(&(*space).space, &(*atom).atom)
}

#[no_mangle]
pub extern "C" fn get_atom_types(space: *const grounding_space_t, atom: *const atom_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let space = unsafe{ &(*space).space };
    let atom = unsafe{ &(*atom).atom };
    let types = hyperon::metta::types::get_atom_types(space, atom);
    return_atoms(&types, callback, context);
}

// MeTTa interpreter API

pub struct step_result_t {
    result: StepResult<Vec<InterpretedAtom>>,
}

#[no_mangle]
pub extern "C" fn interpret_init(space: *mut grounding_space_t, expr: *const atom_t) -> *mut step_result_t {
    let step = unsafe { interpreter::interpret_init((*space).space.clone(), &(*expr).atom) };
    Box::into_raw(Box::new(step_result_t{ result: step }))
}

#[no_mangle]
pub extern "C" fn interpret_step(step: *mut step_result_t) -> *mut step_result_t {
    let step = unsafe { Box::from_raw(step) };
    let next = interpreter::interpret_step(step.result);
    Box::into_raw(Box::new(step_result_t{ result: next }))
}

#[no_mangle]
pub extern "C" fn step_has_next(step: *const step_result_t) -> bool {
    unsafe{ (*step).result.has_next() }
}

#[no_mangle]
pub extern "C" fn step_get_result(step: *mut step_result_t,
        callback: c_atoms_callback_t, context: *mut c_void) {
    let step = unsafe{ Box::from_raw(step) };
    match step.result {
        StepResult::Return(mut res) => {
            let res = res.drain(0..).map(|res| res.into_tuple().0).collect();
            return_atoms(&res, callback, context);
        },
        StepResult::Error(_) => return_atoms(&vec![], callback, context),
        _ => panic!("Not expected step result: {:?}", step.result),
    }
}

#[no_mangle]
pub extern "C" fn step_to_str(step: *const step_result_t, callback: c_str_callback_t, context: *mut c_void) {
    let result = unsafe{ &(*step).result };
    callback(str_as_cstr(format!("{:?}", result).as_str()).as_ptr(), context);
}
