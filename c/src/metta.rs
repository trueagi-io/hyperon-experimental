use hyperon::metta::text::*;
use hyperon::metta::types::AtomType;
use hyperon::metta::interpreter;
use hyperon::metta::interpreter::InterpreterResult;
use hyperon::common::plan::StepResult;

use crate::util::*;
use crate::atom::*;
use crate::space::*;

use std::os::raw::*;
use regex::Regex;

// Tokenizer

#[allow(non_camel_case_types)]
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

#[no_mangle]
pub unsafe extern "C" fn tokenizer_register_token(tokenizer: *mut tokenizer_t,
    regex: *const c_char, constr: atom_constr_t, context: droppable_t) {
    let regex = Regex::new(cstr_as_str(regex)).unwrap();
    (*tokenizer).tokenizer.register_token(regex, move |token| {
        let catom = Box::from_raw(constr(str_as_cstr(token).as_ptr(), context.ptr));
        catom.atom
    });
}

// SExprParser

#[allow(non_camel_case_types)]
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

#[allow(non_camel_case_types)]
pub struct sexpr_space_t {
    space: SExprSpace, 
}

#[no_mangle]
pub extern "C" fn sexpr_space_new() -> *mut sexpr_space_t {
    Box::into_raw(Box::new(sexpr_space_t{ space: SExprSpace::new() })) 
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

#[allow(non_camel_case_types)]
type atom_constr_t = extern "C" fn(*const c_char, *mut c_void) -> *mut atom_t;

#[allow(non_camel_case_types)]
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
pub unsafe extern "C" fn sexpr_space_register_token(space: *mut sexpr_space_t,
    regex: *const c_char, constr: atom_constr_t, context: droppable_t) {
    let regex = Regex::new(cstr_as_str(regex)).unwrap();
    (*space).space.register_token(regex, move |token| {
        let catom = Box::from_raw(constr(str_as_cstr(token).as_ptr(), context.ptr));
        catom.atom
    });
}

#[no_mangle]
pub unsafe extern "C" fn sexpr_space_into_grounding_space(sexpr: *const sexpr_space_t,
        gnd: *mut grounding_space_t) {
    (*sexpr).space.into_grounding_space(&mut (*gnd).space);
}

#[allow(non_camel_case_types)]
pub struct atom_type_t {
    pub typ: AtomType,
}

#[no_mangle]
pub static ATOM_TYPE_UNDEFINED: &atom_type_t = &atom_type_t{ typ: AtomType::Undefined };

#[no_mangle]
pub unsafe extern "C" fn atom_type_specific(atom: *mut atom_t) -> *mut atom_type_t {
    let c_atom = Box::from_raw(atom);
    Box::into_raw(Box::new(atom_type_t{ typ: AtomType::Specific(c_atom.atom) })) 
}

#[no_mangle]
pub unsafe extern "C" fn atom_type_free(typ: *const atom_type_t) {
    if typ != ATOM_TYPE_UNDEFINED {
        drop(Box::from_raw(typ as *mut atom_type_t)) 
    }
}

#[no_mangle]
pub unsafe extern "C" fn check_type(space: *const grounding_space_t, atom: *const atom_t, typ: *const atom_type_t) -> bool {
    hyperon::metta::types::check_type(&(*space).space, &(*atom).atom, &(*typ).typ)
}

#[no_mangle]
pub unsafe extern "C" fn validate_atom(space: *const grounding_space_t, atom: *const atom_t) -> bool {
    hyperon::metta::types::validate_atom(&(*space).space, &(*atom).atom)
}

// MeTTa interpreter API

#[no_mangle]
pub extern "C" fn interpret(space: *mut grounding_space_t, expr: *const atom_t,
        callback: atoms_callback_t, data: *mut c_void) {
    let res = unsafe { hyperon::metta::interpreter::interpret((*space).space.clone(), &(*expr).atom) };
    match res {
        Ok(vec) => return_atoms(&vec, callback, data),
        Err(_) => return_atoms(&vec![], callback, data),
    }
}

#[allow(non_camel_case_types)]
pub struct step_result_t {
    result: StepResult<InterpreterResult>,
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
        callback: atoms_callback_t, data: *mut c_void) {
    let step = unsafe{ Box::from_raw(step) };
    let res = step.result.get_result().map(|mut res| res.drain(0..).map(|(atom, _)| atom).collect());
    match res {
        Ok(vec) => return_atoms(&vec, callback, data),
        Err(_) => return_atoms(&vec![], callback, data),
    }
}

#[no_mangle]
pub extern "C" fn step_to_str(step: *const step_result_t, callback: c_str_callback_t, context: *mut c_void) {
    callback(str_as_cstr(format!("{:?}", unsafe{ &(*step).result }).as_str()).as_ptr(), context);
}
