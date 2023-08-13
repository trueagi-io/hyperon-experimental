use hyperon::common::shared::Shared;
use hyperon::space::DynSpace;
use hyperon::Atom;
use hyperon::metta::text::*;
use hyperon::metta::interpreter;
use hyperon::metta::interpreter::InterpretedAtom;
use hyperon::common::plan::StepResult;
use hyperon::metta::runner::Metta;
use hyperon::rust_type_atom;

use crate::util::*;
use crate::atom::*;
use crate::space::*;

use std::os::raw::*;
use regex::Regex;
use std::path::PathBuf;

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Tokenizer and Parser Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Represents a handle to a Tokenizer state machine, able to recognize meaningful substrings in text
/// @ingroup tokenizer_and_parser_group
/// @note `tokenizer_t` handles must be freed with `tokenizer_free()`
///
#[repr(C)]
pub struct tokenizer_t {
    /// Internal.  Should not be accessed directly
    tokenizer: *const RustTokenizer,
}

struct RustTokenizer(std::cell::RefCell<Tokenizer>);

impl From<Shared<Tokenizer>> for tokenizer_t {
    fn from(tokenizer: Shared<Tokenizer>) -> Self {
        Self{ tokenizer: std::rc::Rc::into_raw(tokenizer.0).cast() }
    }
}

impl tokenizer_t {
    fn borrow_inner(&self) -> &mut Tokenizer {
        let cell = unsafe{ &mut *(&(&*self.tokenizer).0 as *const std::cell::RefCell<Tokenizer>).cast_mut() };
        cell.get_mut()
    }
    fn clone_handle(&self) -> Shared<Tokenizer> {
        unsafe{ std::rc::Rc::increment_strong_count(self.tokenizer); }
        unsafe{ Shared(std::rc::Rc::from_raw(self.tokenizer.cast())) }
    }
    fn into_handle(self) -> Shared<Tokenizer> {
        unsafe{ Shared(std::rc::Rc::from_raw(self.tokenizer.cast())) }
    }
}

/// @brief Creates a new Tokenizer
/// @ingroup tokenizer_and_parser_group
/// @return an `tokenizer_t` handle to access the newly created Tokenizer
/// @note The returned `tokenizer_t` handle must be freed with `tokenizer_free`
///
#[no_mangle]
pub extern "C" fn tokenizer_new() -> tokenizer_t {
    Shared::new(Tokenizer::new()).into()
}

/// @brief Frees a Tokenizer handle
/// @ingroup tokenizer_and_parser_group
/// @param[in]  tokenizer  The `tokenizer_t` handle to free
/// @note When the last `tokenizer_t` handle for an underlying Tokenizer has been freed, then the
///    Tokenizer will be deallocated
///
#[no_mangle]
pub extern "C" fn tokenizer_free(tokenizer: tokenizer_t) {
    let tokenizer = tokenizer.into_handle();
    drop(tokenizer);
}

/// @struct token_api_t
/// @brief A table of callback functions to implement custom atom parsing
/// @ingroup tokenizer_and_parser_group
/// @see tokenizer_register_token
///
#[repr(C)]
pub struct token_api_t {

    /// @brief Creates a new Atom based the provided text
    /// @param[in]  str  A pointer to a C-style text string, that matched the associated regular expression
    /// @param[in]  context  A pointer to the `context` object supplied to `tokenizer_register_token()`
    /// @return An Atom created in response to the supplied text string
    ///
    construct_atom: extern "C" fn(str: *const c_char, context: *mut c_void) -> atom_t,

    /// @brief Frees the `context`, passed to `tokenizer_register_token()`, along with all other associated resources
    /// @param[in]  context  The pointer to the `context` to free
    /// @note Assigning NULL to this field means the context does not need to be freed
    ///
    free_context: Option<extern "C" fn(context: *mut c_void)>
}

//Internal wrapper to make sure the cleanup function gets called on the Token context
struct CToken {
    context: *mut c_void,
    api: *const token_api_t
}

impl Drop for CToken {
    fn drop(&mut self) {
        let free = unsafe{ (&*(*self).api).free_context };
        if let Some(free) = free {
            free(self.context);
        }
    }
}

/// @brief Registers a new custom Token with a Tokenizer
/// @ingroup tokenizer_and_parser_group
/// @param[in]  tokenizer  A pointer to the Tokenizer with which to register the Token
/// @param[in]  regex  A regular expression to match the incoming text, triggering this token to execute
/// @param[in]  api  A table of functions to manage the parsing when a Token is matched
/// @param[in]  context  A caller-defined structure to communicate any state necessary to implement the Token parser
/// @note Hyperon uses the Rust regex engine and syntax, [documented here](https://docs.rs/regex/latest/regex/).
///
#[no_mangle]
pub extern "C" fn tokenizer_register_token(tokenizer: *mut tokenizer_t,
    regex: *const c_char, api: *const token_api_t, context: *mut c_void) {
    let tokenizer = unsafe{ &*tokenizer }.borrow_inner();
    let regex = Regex::new(cstr_as_str(regex)).unwrap();
    let c_token = CToken{ context, api };
    tokenizer.register_token(regex, move |token| {
        let constr = unsafe{ (&*c_token.api).construct_atom };
        let atom = constr(str_as_cstr(token).as_ptr(), c_token.context);
        atom.into_inner()
    });
}

/// @brief Performs a "deep copy" of a Tokenizer
/// @ingroup tokenizer_and_parser_group
/// @param[in]  tokenizer  A pointer to the Tokenizer to clone
/// @return The new Tokenizer, containing all registered Tokens belonging to the original Tokenizer
/// @note The returned `tokenizer_t` must be freed with `tokenizer_free()`
///
#[no_mangle]
pub extern "C" fn tokenizer_clone(tokenizer: *const tokenizer_t) -> tokenizer_t {
    let tokenizer = unsafe{ &*tokenizer }.borrow_inner();
    Shared::new(tokenizer.clone()).into()
}

// SExprParser

pub type sexpr_parser_t<'a> = SharedApi<SExprParser<'a>>;

#[no_mangle]
pub extern "C" fn sexpr_parser_new<'a>(text: *const c_char) -> *mut sexpr_parser_t<'a> {
    sexpr_parser_t::new(SExprParser::new(cstr_as_str(text)))
}

#[no_mangle]
pub extern "C" fn sexpr_parser_free(parser: *mut sexpr_parser_t) {
    sexpr_parser_t::drop(parser)
}

#[no_mangle]
pub extern "C" fn sexpr_parser_parse(
    parser: *mut sexpr_parser_t,
    tokenizer: *const tokenizer_t) -> atom_t
{
    let parser = unsafe{ &mut *parser };
    let tokenizer = unsafe{ &*tokenizer }.borrow_inner();
    parser.borrow_mut().parse(tokenizer).unwrap().into()
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// MeTTa Type System
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

//TODO After Alpha.  These should be static rather than functions, once I get atom_t to be fully repr(C),
// so the caller doesn't need to worry about freeing them
#[no_mangle] pub extern "C" fn ATOM_TYPE_UNDEFINED() -> atom_t { hyperon::metta::ATOM_TYPE_UNDEFINED.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_TYPE() -> atom_t { hyperon::metta::ATOM_TYPE_TYPE.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_ATOM() -> atom_t { hyperon::metta::ATOM_TYPE_ATOM.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_SYMBOL() -> atom_t { hyperon::metta::ATOM_TYPE_SYMBOL.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_VARIABLE() -> atom_t { hyperon::metta::ATOM_TYPE_VARIABLE.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_EXPRESSION() -> atom_t { hyperon::metta::ATOM_TYPE_EXPRESSION.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_GROUNDED() -> atom_t { hyperon::metta::ATOM_TYPE_GROUNDED.into() }
#[no_mangle] pub extern "C" fn ATOM_TYPE_GROUNDED_SPACE() -> atom_t { rust_type_atom::<DynSpace>().into() }

#[no_mangle]
pub extern "C" fn check_type(space: *const space_t, atom: *const atom_ref_t, typ: *const atom_ref_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ &*atom }.borrow();
    let typ = unsafe{ &*typ }.borrow();
    hyperon::metta::types::check_type(dyn_space.borrow().as_space(), atom, typ)
}

#[no_mangle]
pub extern "C" fn validate_atom(space: *const space_t, atom: *const atom_ref_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ &*atom }.borrow();
    hyperon::metta::types::validate_atom(dyn_space.borrow().as_space(), atom)
}

#[no_mangle]
pub extern "C" fn get_atom_types(space: *const space_t, atom: *const atom_ref_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ (&*atom).borrow() };
    let types = hyperon::metta::types::get_atom_types(dyn_space.borrow().as_space(), atom);
    return_atoms(&types, callback, context);
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// MeTTa Intperpreter Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

pub struct step_result_t<'a> {
    result: StepResult<'a, Vec<InterpretedAtom>, (Atom, Atom)>,
}

#[no_mangle]
pub extern "C" fn interpret_init<'a>(space: *mut space_t, expr: *const atom_ref_t) -> *mut step_result_t<'a> {
    let dyn_space = unsafe{ &*space }.borrow();
    let expr = unsafe{ (&*expr).borrow() };
    let step = interpreter::interpret_init(dyn_space.clone(), expr);
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
        callback: c_atom_vec_callback_t, context: *mut c_void) {
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

/// Writes a text description of the step_result_t into the provided buffer and returns the number of bytes
/// written, or that would have been written had the buf_len been large enough, excluding the
/// string terminator.
#[no_mangle]
pub extern "C" fn step_to_str(step: *const step_result_t, buf: *mut c_char, buf_len: usize) -> usize {
    let result = unsafe{ &(*step).result };
    write_debug_into_buf(result, buf, buf_len)
}

pub type metta_t = SharedApi<Metta>;

#[no_mangle]
pub extern "C" fn metta_new(space: *mut space_t, tokenizer: *mut tokenizer_t, cwd: *const c_char) -> *mut metta_t {
    let dyn_space = unsafe{ &*space }.borrow();
    let tokenizer = unsafe{ &*tokenizer }.clone_handle();
    metta_t::new(Metta::from_space_cwd(dyn_space.clone(), tokenizer, PathBuf::from(cstr_as_str(cwd))))
}

#[no_mangle]
pub extern "C" fn metta_clone(metta: *mut metta_t) -> *mut metta_t {
    let metta = unsafe{ &(*metta) };
    metta_t::from_shared(metta.shared())
}

#[no_mangle]
pub extern "C" fn metta_free(metta: *mut metta_t) {
    metta_t::drop(metta);
}

#[no_mangle]
pub extern "C" fn metta_space(metta: *mut metta_t) -> space_t {
    let space = unsafe{ &*metta }.borrow().space();
    space.into()
}

#[no_mangle]
pub extern "C" fn metta_tokenizer(metta: *mut metta_t) -> tokenizer_t {
    let tokenizer = unsafe{ &*metta }.borrow().tokenizer();
    tokenizer.into()
}

#[no_mangle]
pub extern "C" fn metta_run(metta: *mut metta_t, parser: *mut sexpr_parser_t,
        output: c_atom_vec_callback_t, out_context: *mut c_void) {
    let metta = unsafe{ &*metta }.borrow();
    let mut parser = unsafe{ &mut *parser }.borrow_mut();
    let results = metta.run(&mut parser);
    // TODO: return erorrs properly after step_get_result() is changed to return errors.
    for result in results.expect("Returning errors from C API is not implemented yet") {
        return_atoms(&result, output, out_context);
    }
}

#[no_mangle]
pub extern "C" fn metta_evaluate_atom(metta: *mut metta_t, atom: atom_t,
        output: c_atom_vec_callback_t, out_context: *mut c_void) {
    let metta = unsafe{ &*metta }.borrow();
    let atom = atom.into_inner();
    let result = metta.evaluate_atom(atom)
        .expect("Returning errors from C API is not implemented yet");
    return_atoms(&result, output, out_context);
}

#[no_mangle]
pub extern "C" fn metta_load_module(metta: *mut metta_t, name: *const c_char) {
    let metta = unsafe{ &*metta }.borrow();
    // TODO: return erorrs properly
    metta.load_module(PathBuf::from(cstr_as_str(name)))
        .expect("Returning errors from C API is not implemented yet");
}
