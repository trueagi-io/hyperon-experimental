use hyperon::common::shared::Shared;
use hyperon::space::DynSpace;
use hyperon::metta::text::*;
use hyperon::metta::interpreter;
use hyperon::metta::interpreter::InterpreterState;
use hyperon::metta::runner::{Metta, RunContext, ModId, RunnerState, Environment, EnvBuilder};
use hyperon::metta::runner::modules::{FsModuleFormat, ModuleLoader, ModuleDescriptor};
use hyperon::rust_type_atom;

use crate::util::*;
use crate::atom::*;
use crate::space::*;

use std::os::raw::*;
use std::path::{Path, PathBuf};

use regex::Regex;

//LONG-TERM ISSUE: There is no cross-platform way to go from a Path to a CString and back.
//This is in no small part caused by the fact that the code we are interacting with needs to know how
// to interpret the path encoding, which is platform-dependent.  Paths on Windows are particularly
// problematic.
//Going via UTF-8 encoded `str` is definitely wrong, but at least it's the same kind of wrong on
// all platforms.  https://internals.rust-lang.org/t/pathbuf-to-cstring/12560/10
//

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Tokenizer and Parser Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Represents a handle to a Tokenizer, capable of recognizing meaningful Token substrings in text
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
        let cell = unsafe{ &mut (&mut *self.tokenizer.cast_mut()).0 };
        cell.get_mut()
    }
    fn into_handle(self) -> Shared<Tokenizer> {
        unsafe{ Shared(std::rc::Rc::from_raw(self.tokenizer.cast())) }
    }
}

/// @brief Creates a new Tokenizer, without any registered Tokens
/// @ingroup tokenizer_and_parser_group
/// @return an `tokenizer_t` handle to access the newly created Tokenizer
/// @note The returned `tokenizer_t` handle must be freed with `tokenizer_free()`
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

/// @brief Registers a new custom Token in a Tokenizer
/// @ingroup tokenizer_and_parser_group
/// @param[in]  tokenizer  A pointer to the Tokenizer in which to register the Token
/// @param[in]  regex  A regular expression to match the incoming text, triggering this token to generate a new atom
/// @param[in]  api  A table of functions to manage the token
/// @param[in]  context  A caller-defined structure to communicate any state necessary to implement the Token parser
/// @note Hyperon uses the Rust RegEx engine and syntax, [documented here](https://docs.rs/regex/latest/regex/).
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

/// @brief Represents an S-Expression Parser state machine, to parse input text into an Atom
/// @ingroup tokenizer_and_parser_group
/// @note `sexpr_parser_t` objects must be freed with `sexpr_parser_free()`
///
#[repr(C)]
pub struct sexpr_parser_t {
    /// Internal.  Should not be accessed directly
    parser: *mut RustSExprParser,
    err_string: *mut c_char,
}

impl sexpr_parser_t {
    fn free_err_string(&mut self) {
        if !self.err_string.is_null() {
            let string = unsafe{ std::ffi::CString::from_raw(self.err_string) };
            drop(string);
            self.err_string = core::ptr::null_mut();
        }
    }
}

#[derive(Clone)]
enum RustSExprParser {
    Borrowed(SExprParser<'static>),
    Owned(OwnedSExprParser),
}

impl From<SExprParser<'static>> for sexpr_parser_t {
    fn from(parser: SExprParser<'static>) -> Self {
        RustSExprParser::Borrowed(parser).into()
    }
}

impl From<OwnedSExprParser> for sexpr_parser_t {
    fn from(parser: OwnedSExprParser) -> Self {
        RustSExprParser::Owned(parser).into()
    }
}

impl From<RustSExprParser> for sexpr_parser_t {
    fn from(parser: RustSExprParser) -> Self {
        Self{
            parser: Box::into_raw(Box::new(parser)),
            err_string: core::ptr::null_mut(),
        }
    }
}

impl sexpr_parser_t {
    fn into_inner_enum(mut self) -> RustSExprParser {
        self.free_err_string();
        unsafe{ *Box::from_raw(self.parser) }
    }
    fn into_boxed_dyn(self) -> Box<dyn Parser> {
        let boxed_parser = self.into_inner_enum();
        match boxed_parser {
            RustSExprParser::Borrowed(parser) => Box::new(parser),
            RustSExprParser::Owned(parser) => Box::new(parser),
        }
    }
    fn borrow_inner_enum(&self) -> &RustSExprParser {
        unsafe{ &*self.parser }
    }
    fn borrow_dyn_mut(&mut self) -> &mut dyn Parser {
        match unsafe{ &mut *self.parser } {
            RustSExprParser::Borrowed(parser) => parser,
            RustSExprParser::Owned(parser) => parser,
        }
    }
    fn borrow_sexpr_parser_mut(&mut self) -> &mut SExprParser<'static> {
        match unsafe{ &mut *self.parser } {
            RustSExprParser::Borrowed(parser) => parser,
            RustSExprParser::Owned(_) => panic!("Fatal Error: Feature unsupported for owned src buffers"),
        }
    }
}

/// @brief Creates a new S-Expression Parser
/// @ingroup tokenizer_and_parser_group
/// @param[in]  text  A C-style string containing the input text to parse
/// @return The new `sexpr_parser_t`, ready to parse the text
/// @note The returned `sexpr_parser_t` must be freed with `sexpr_parser_free()` or passed to another
///    function that takes ownership
/// @warning The returned `sexpr_parser_t` borrows a reference to the `text`, so the returned
///    `sexpr_parser_t` must be freed before the `text` is freed or allowed to go out of scope.
///
#[no_mangle]
pub extern "C" fn sexpr_parser_new(text: *const c_char) -> sexpr_parser_t {
    SExprParser::new(cstr_as_str(text)).into()
}

/// @brief Creates a new S-Expression Parser, for situations where you must deallocate the text buffer
///    before parsing is complete
/// @ingroup tokenizer_and_parser_group
/// @param[in]  text  A C-style string containing the input text to parse.  This function will make an
///    internal copy of the text
/// @return The new `sexpr_parser_t`, ready to parse the text
/// @note The returned `sexpr_parser_t` must be freed with `sexpr_parser_free()` or passed to another
///    function that takes ownership
///
#[no_mangle]
pub extern "C" fn sexpr_parser_new_copy_src(text: *const c_char) -> sexpr_parser_t {
    OwnedSExprParser::new(cstr_as_str(text).to_string()).into()
}

/// @brief Creates a new S-Expression Parser from an existing `sexpr_parser_t`
/// @ingroup tokenizer_and_parser_group
/// @param[in]  parser  The source `sexpr_parser_t` to clone
/// @return The new `sexpr_parser_t`, ready to parse the text
/// @note The returned `sexpr_parser_t` must be freed with `sexpr_parser_free()`
/// @note A cloned parser can be thought of as an independent read cursor referencing the same source text.
/// @warning The returned `sexpr_parser_t` borrows a reference to the same `text` pointer as the original,
///    so the returned `sexpr_parser_t` must be freed before the `text` is freed or allowed to go out of scope.
///
#[no_mangle]
pub extern "C" fn sexpr_parser_clone(parser: *const sexpr_parser_t) -> sexpr_parser_t {
    let parser = unsafe{ &*parser }.borrow_inner_enum();
    parser.clone().into()
}

/// @brief Frees an S-Expression Parser
/// @ingroup tokenizer_and_parser_group
/// @param[in]  parser  The `sexpr_parser_t` handle to free
///
#[no_mangle]
pub extern "C" fn sexpr_parser_free(parser: sexpr_parser_t) {
    let parser = parser.into_inner_enum();
    drop(parser);
}

/// @brief Parses the text associated with an `sexpr_parser_t`, and creates the corresponding Atom
/// @ingroup tokenizer_and_parser_group
/// @param[in]  parser  A pointer to the Parser, which is associated with the text to parse
/// @param[in]  tokenizer  A pointer to the Tokenizer, to use to interpret atoms within the expression
/// @return The new `atom_t`, which may be an Expression atom with many child atoms.  Returns a `none`
///    atom if parsing is finished, or an error expression atom if a parse error occurred.
/// @note The caller must take ownership responsibility for the returned `atom_t`, and ultimately free
///    it with `atom_free()` or pass it to another function that takes ownership responsibility
/// @note If this function encounters an error, the error may be accessed with `sexpr_parser_err_str()`
///
#[no_mangle]
pub extern "C" fn sexpr_parser_parse(
    parser: *mut sexpr_parser_t,
    tokenizer: *const tokenizer_t) -> atom_t
{
    let parser = unsafe{ &mut *parser };
    parser.free_err_string();
    let rust_parser = parser.borrow_dyn_mut();
    let tokenizer = unsafe{ &*tokenizer }.borrow_inner();
    match rust_parser.next_atom(tokenizer) {
        Ok(atom) => atom.into(),
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            parser.err_string = err_cstring.into_raw();
            atom_t::null()
        }
    }
}

/// @brief Returns the error string associated with the last `sexpr_parser_parse` call
/// @ingroup tokenizer_and_parser_group
/// @param[in]  parser  A pointer to the Parser, which is associated with the text to parse
/// @return A pointer to the C-string containing the parse error that occurred, or NULL if no
///     parse error occurred
/// @warning The returned pointer should NOT be freed.  It must never be accessed after the
///     sexpr_parser_t has been freed, or any subsequent call to `sexpr_parser_parse` or
///     `sexpr_parser_parse_to_syntax_tree` has been made.
///
#[no_mangle]
pub extern "C" fn sexpr_parser_err_str(
    parser: *const sexpr_parser_t) -> *const c_char {
    let parser = unsafe{ &*parser };
    parser.err_string
}

/// @brief Represents a component in a syntax tree created by parsing MeTTa code
/// @ingroup tokenizer_and_parser_group
/// @note `syntax_node_t` objects must be freed with `syntax_node_free()`
///
#[repr(C)]
pub struct syntax_node_t {
    /// Internal.  Should not be accessed directly
    node: *mut RustSyntaxNode,
}

struct RustSyntaxNode(SyntaxNode);

impl From<SyntaxNode> for syntax_node_t {
    fn from(node: SyntaxNode) -> Self {
        Self{ node: Box::into_raw(Box::new(RustSyntaxNode(node))) }
    }
}

impl From<Option<SyntaxNode>> for syntax_node_t {
    fn from(node: Option<SyntaxNode>) -> Self {
        match node {
            Some(node) => Self{ node: Box::into_raw(Box::new(RustSyntaxNode(node))) },
            None => syntax_node_t::null()
        }
    }
}

impl syntax_node_t {
    fn into_inner(self) -> SyntaxNode {
        unsafe{ (*Box::from_raw(self.node)).0 }
    }
    fn borrow(&self) -> &SyntaxNode {
        &unsafe{ &*self.node }.0
    }
    fn is_null(&self) -> bool {
        self.node == core::ptr::null_mut()
    }
    fn null() -> Self {
        Self{node: core::ptr::null_mut()}
    }
}

/// @brief The type of language construct respresented by a syntax_node_t
/// @ingroup tokenizer_and_parser_group
///
#[repr(C)]
pub enum syntax_node_type_t {
    /// @brief A Comment, beginning with a ';' character
    COMMENT,
    /// @brief A variable.  A symbol immediately preceded by a '$' sigil
    VARIABLE_TOKEN,
    /// @brief A String Literal.  All text between non-escaped '"' (double quote) characters
    STRING_TOKEN,
    /// @brief Word Token.  Any other whitespace-delimited token that isn't a VARIABLE_TOKEN or STRING_TOKEN
    WORD_TOKEN,
    /// @brief Open Parenthesis.  A non-escaped '(' character indicating the beginning of an expression
    OPEN_PAREN,
    /// @brief Close Parenthesis.  A non-escaped ')' character indicating the end of an expression
    CLOSE_PAREN,
    /// @brief Whitespace. One or more whitespace chars
    WHITESPACE,
    /// @brief Leftover Text that remains unparsed after a parse error has occurred
    LEFTOVER_TEXT,
    /// @brief A Group of nodes between an `OPEN_PAREN` and a matching `CLOSE_PAREN`
    EXPRESSION_GROUP,
    /// @brief A Group of nodes that cannot be combined into a coherent atom due to a parse error,
    ///     even if some of the individual nodes could represent valid atoms
    ERROR_GROUP,
}

impl From<SyntaxNodeType> for syntax_node_type_t {
    fn from(node_type: SyntaxNodeType) -> Self {
        match node_type {
            SyntaxNodeType::Comment => Self::COMMENT,
            SyntaxNodeType::VariableToken => Self::VARIABLE_TOKEN,
            SyntaxNodeType::StringToken => Self::STRING_TOKEN,
            SyntaxNodeType::WordToken => Self::WORD_TOKEN,
            SyntaxNodeType::OpenParen => Self::OPEN_PAREN,
            SyntaxNodeType::CloseParen => Self::CLOSE_PAREN,
            SyntaxNodeType::Whitespace => Self::WHITESPACE,
            SyntaxNodeType::LeftoverText => Self::LEFTOVER_TEXT,
            SyntaxNodeType::ExpressionGroup => Self::EXPRESSION_GROUP,
            SyntaxNodeType::ErrorGroup => Self::ERROR_GROUP,
        }
    }
}

/// @brief Function signature for a callback providing access to a `syntax_node_t`
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  The `syntax_node_t` being provided.  This node should not be modified or freed by the callback.
/// @param[in]  context  The context state pointer initially passed to the upstream function initiating the callback.
///
pub type c_syntax_node_callback_t = extern "C" fn(node: *const syntax_node_t, context: *mut c_void);

/// @brief Parses the text associated with an `sexpr_parser_t`, and creates a syntax tree
/// @ingroup tokenizer_and_parser_group
/// @param[in]  parser  A pointer to the Parser, which is associated with the text to parse
/// @return The new `syntax_node_t` representing the root of the parsed tree
/// @note The caller must take ownership responsibility for the returned `syntax_node_t`, and ultimately free
///   it with `syntax_node_free()`
///
#[no_mangle]
pub extern "C" fn sexpr_parser_parse_to_syntax_tree(parser: *mut sexpr_parser_t) -> syntax_node_t
{
    let parser = unsafe{ &mut *parser };
    parser.free_err_string();
    let rust_parser = parser.borrow_sexpr_parser_mut();
    rust_parser.parse_to_syntax_tree().into()
}

/// @brief Frees a syntax_node_t
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  The `syntax_node_t` to free
///
#[no_mangle]
pub extern "C" fn syntax_node_free(node: syntax_node_t) {
    let node = node.into_inner();
    drop(node);
}

/// @brief Creates a deep copy of a `syntax_node_t`
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the `syntax_node_t`
/// @return The `syntax_node_t` representing the cloned syntax node
/// @note The caller must take ownership responsibility for the returned `syntax_node_t`, and ultimately free
///   it with `syntax_node_free()`
///
#[no_mangle]
pub extern "C" fn syntax_node_clone(node: *const syntax_node_t) -> syntax_node_t {
    let node = unsafe{ &*node }.borrow();
    node.clone().into()
}

/// @brief Performs a depth-first iteration of all child syntax nodes within a syntax tree
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the top-level `syntax_node_t` representing the syntax tree
/// @param[in]  callback  A function that will be called to provide a vector of all type atoms associated with the `atom` argument atom
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
///
#[no_mangle]
pub extern "C" fn syntax_node_iterate(node: *const syntax_node_t,
    callback: c_syntax_node_callback_t, context: *mut c_void) {
    let node = unsafe{ &*node }.borrow();
    node.visit_depth_first(|node| {
        let node = syntax_node_t{node: (node as *const SyntaxNode).cast_mut().cast()};
        callback(&node, context);
    });
}

/// @brief Returns the type of a `syntax_node_t`
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the `syntax_node_t`
/// @return The `syntax_node_type_t` representing the type of the syntax node
///
#[no_mangle]
pub extern "C" fn syntax_node_type(node: *const syntax_node_t) -> syntax_node_type_t {
    let node = unsafe{ &*node }.borrow();
    node.node_type.into()
}

/// @brief Returns `true` if a syntax node represents the end of the stream
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the `syntax_node_t`
/// @return The boolean value indicating if the node is a a null node
///
#[no_mangle]
pub extern "C" fn syntax_node_is_null(node: *const syntax_node_t) -> bool {
    unsafe{ &*node }.is_null()
}

/// @brief Returns `true` if a syntax node is a leaf (has no children) and `false` otherwise
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the `syntax_node_t`
/// @return The boolean value indicating if the node is a leaf
///
#[no_mangle]
pub extern "C" fn syntax_node_is_leaf(node: *const syntax_node_t) -> bool {
    let node = unsafe{ &*node }.borrow();
    node.node_type.is_leaf()
}

/// @brief Returns the beginning and end positions in the parsed source of the text represented by the syntax node
/// @ingroup tokenizer_and_parser_group
/// @param[in]  node  A pointer to the `syntax_node_t`
/// @param[out]  range_start  A pointer to a value, into which the starting offset of the range will be written
/// @param[out]  range_end  A pointer to a value, into which the ending offset of the range will be written
///
#[no_mangle]
pub extern "C" fn syntax_node_src_range(node: *const syntax_node_t, range_start: *mut usize, range_end: *mut usize) {
    let node = unsafe{ &*node }.borrow();
    unsafe{ *range_start = node.src_range.start; }
    unsafe{ *range_end = node.src_range.end; }
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// MeTTa Language and Types
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

//TODO After Alpha.  These MeTTa keyword atoms should be static objects rather than functions, once I make
// atom_t fully repr(C), then I will change this, so the caller doesn't need to worry about freeing the
// return values

/// @brief Checks if an atom is a MeTTa error expression
/// @ingroup metta_language_group
/// @param[in]  atom  A pointer to an `atom_t` or an `atom_ref_t` representing the atom to check
/// @return `true` is the atom is a MeTTa error expression
///
#[no_mangle]
pub extern "C" fn atom_is_error(atom: *const atom_ref_t) -> bool {
    let atom = unsafe{ &*atom }.borrow();
    hyperon::metta::atom_is_error(atom)
}

/// @brief Renders the text message from an error expression atom into a buffer
/// @ingroup metta_language_group
/// @param[in]  atom  The error expression atom from which to extract the error message
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the message string, minus the string terminator character.  If
///    `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
///    called again with a larger buffer.
/// @warning The `atom` argument must be an error expression, otherwise this function will panic
///
#[no_mangle]
pub extern "C" fn atom_error_message(atom: *const atom_ref_t, buf: *mut c_char, buf_len: usize) -> usize {
    let atom = unsafe{ &*atom }.borrow();
    write_into_buf(hyperon::metta::atom_error_message(atom), buf, buf_len)
}

/// @brief Creates a Symbol atom for the special MeTTa symbol: "%Undefined%"
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_UNDEFINED() -> atom_t { hyperon::metta::ATOM_TYPE_UNDEFINED.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Type", used to indicate that an atom represents the type of another atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_TYPE() -> atom_t { hyperon::metta::ATOM_TYPE_TYPE.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Atom", used to indicate that an atom's type is a generic atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_ATOM() -> atom_t { hyperon::metta::ATOM_TYPE_ATOM.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Symbol", used to indicate that an atom's type is a symbol atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_SYMBOL() -> atom_t { hyperon::metta::ATOM_TYPE_SYMBOL.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Variable", used to indicate that an atom's type is a variable atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_VARIABLE() -> atom_t { hyperon::metta::ATOM_TYPE_VARIABLE.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Expression", used to indicate that an atom's type is an expression atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_EXPRESSION() -> atom_t { hyperon::metta::ATOM_TYPE_EXPRESSION.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol: "Grounded", used to indicate that an atom's type is a grounded atom
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_GROUNDED() -> atom_t { hyperon::metta::ATOM_TYPE_GROUNDED.into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol used to indicate that an atom's type is a wrapper around a Space
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Symbol atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn ATOM_TYPE_GROUNDED_SPACE() -> atom_t { rust_type_atom::<DynSpace>().into() }

/// @brief Creates a Symbol atom for the special MeTTa symbol used to indicate empty results
/// returned by function.
/// @ingroup metta_language_group
/// @return  The `atom_t` representing the Void atom
/// @note The returned `atom_t` must be freed with `atom_free()`
///
#[no_mangle] pub extern "C" fn EMPTY_SYMBOL() -> atom_t {
    hyperon::metta::EMPTY_SYMBOL.into()
}

/// @brief Checks whether Atom `atom` has Type `typ` in context of `space`
/// @ingroup metta_language_group
/// @param[in]  space  A pointer to the `space_t` representing the space context in which to perform the check
/// @param[in]  atom  A pointer to the `atom_t` or `atom_ref_t` representing the atom whose Type the function will check
/// @param[in]  typ  A pointer to the `atom_t` or `atom_ref_t` representing the type to check against
/// @return  `true` if the Atom's Type is a match, otherwise `false`
/// @note This function can be used for a simple type check when there is no need to know type parameters
///
#[no_mangle]
pub extern "C" fn check_type(space: *const space_t, atom: *const atom_ref_t, typ: *const atom_ref_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ &*atom }.borrow();
    let typ = unsafe{ &*typ }.borrow();
    hyperon::metta::types::check_type(dyn_space.borrow().as_space(), atom, typ)
}

/// @brief Checks whether `atom` is correctly typed
/// @ingroup metta_language_group
/// @param[in]  space  A pointer to the `space_t` representing the space context in which to perform the check
/// @param[in]  atom  A pointer to the `atom_t` or `atom_ref_t` representing the atom whose Type the function will check
/// @return  `true` if the Atom is correctly typed, otherwise `false`
/// @note This function can be used to check if function arguments have correct types
///
#[no_mangle]
pub extern "C" fn validate_atom(space: *const space_t, atom: *const atom_ref_t) -> bool {
    let dyn_space = unsafe{ &*space }.borrow();
    let atom = unsafe{ &*atom }.borrow();
    hyperon::metta::types::validate_atom(dyn_space.borrow().as_space(), atom)
}

/// @brief Provides all types for `atom` in the context of `space`
/// @ingroup metta_language_group
/// @param[in]  space  A pointer to the `space_t` representing the space context in which to access the Atom's types
/// @param[in]  atom  A pointer to the `atom_t` or `atom_ref_t` representing the atom whose Types the function will access
/// @param[in]  callback  A function that will be called to provide a vector of all type atoms associated with the `atom` argument atom
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note `%Undefined%` will be provided if `atom` has no type assigned. An empty vector will be provided if `atom`
///    is a function call but expected types of arguments are not compatible with passed values
///
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

/// @brief Contains the state for an in-flight interpreter operation
/// @ingroup interpreter_group
/// @note The `step_result_t` API is very low-level; It provides direct access to the interpreter
///    without a an environment, parser, or tokenizer.  Usually the `runner_state_t` API is a better
///    choice for executing MeTTa code in most situations.
/// @note A `step_result_t` is initially created by `interpret_init()`.  Each call to `interpret_step()`, in
///    a loop, consumes the `step_result_t` and creates a new one.  When the interpreter operation has
///    fully resolved, `step_get_result()` can provide the final results.  Ownership of the `step_result_t`
///    must ultimately be released with `step_get_result()`.
/// @see interpret_init
/// @see interpret_step
/// @see step_get_result
///
#[repr(C)]
pub struct step_result_t {
    /// Internal.  Should not be accessed directly
    result: *mut RustStepResult,
}

struct RustStepResult(InterpreterState<'static, DynSpace>);

impl From<InterpreterState<'static, DynSpace>> for step_result_t {
    fn from(state: InterpreterState<'static, DynSpace>) -> Self {
        Self{ result: Box::into_raw(Box::new(RustStepResult(state))) }
    }
}

impl step_result_t {
    fn into_inner(self) -> InterpreterState<'static, DynSpace> {
        unsafe{ Box::from_raw(self.result).0 }
    }
    fn borrow(&self) -> &InterpreterState<'static, DynSpace> {
        &unsafe{ &*(&*self).result }.0
    }
}

/// @brief Initializes an interpreter operation and take the initial step
/// @ingroup interpreter_group
/// @param[in]  space  A pointer to the Space in which to perform the operation
/// @param[in]  expr  A pointer to an `atom_t` or `atom_ref_t` Expression atom to interpret
/// @return A `step_result_t` representing the outcome from the initial step
/// @note The returned value may represent an error, an immediate result value, or it may be necessary to
///    call `interpret_step()` in a loop to fully evaluate the execution plan.  Ultimately `step_get_result()`
///    must be called to release the returned `step_result_t`
///
#[no_mangle]
pub extern "C" fn interpret_init(space: *mut space_t, expr: *const atom_ref_t) -> step_result_t {
    let dyn_space = unsafe{ &*space }.borrow();
    let expr = unsafe{ (&*expr).borrow() };
    let step = interpreter::interpret_init(dyn_space.clone(), expr);
    step.into()
}

/// @brief Takes a subsequent step in an in-flight interpreter operation
/// @ingroup interpreter_group
/// @param[in]  step  The existing state for the in-flight interpreter operation
/// @return A new `step_result_t` representing the outcome from the step
///
#[no_mangle]
pub extern "C" fn interpret_step(step: step_result_t) -> step_result_t {
    let step = step.into_inner();
    let next = interpreter::interpret_step(step);
    next.into()
}

/// @brief Renders a text description of a `step_result_t` into a buffer
/// @ingroup interpreter_group
/// @param[in]  step  A pointer to a `step_result_t` to render
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the description string, minus the string terminator character.  If
///    `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
///    called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn step_to_str(step: *const step_result_t, buf: *mut c_char, buf_len: usize) -> usize {
    let step = unsafe{ &*step }.borrow();
    write_debug_into_buf(step, buf, buf_len)
}

/// @brief Examines a `step_result_t` to determine if more work is needed
/// @ingroup interpreter_group
/// @param[in]  step  A pointer to the `step_result_t` representing the in-flight interpreter operation
/// @return `true` if the operation plan indicates that more work is needed to finalize results, otherwise `false`
///
#[no_mangle]
pub extern "C" fn step_has_next(step: *const step_result_t) -> bool {
    let step = unsafe{ &*step }.borrow();
    step.has_next()
}

/// @brief Consumes a `step_result_t` and provides the ultimate outcome of a MeTTa interpreter session
/// @ingroup interpreter_group
/// @param[in]  step  A pointer to a `step_result_t` to render
/// @param[in]  callback  A function that will be called to provide a vector of all atoms resulting from the interpreter session
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
///
#[no_mangle]
pub extern "C" fn step_get_result(step: step_result_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let step = step.into_inner();
    match step.into_result() {
        Ok(res) => return_atoms(&res, callback, context),
        Err(_) => return_atoms(&vec![], callback, context),
    }
}

/// @brief A top-level MeTTa runner
/// @ingroup interpreter_group
/// @note A `metta_t` must be freed with `metta_free()`
/// @see metta_new
/// @see metta_free
///
#[repr(C)]
pub struct metta_t {
    /// Internal.  Should not be accessed directly
    metta: *mut RustMettaRunner,
    err_string: *mut c_char,
}

impl metta_t {
    fn free_err_string(&mut self) {
        if !self.err_string.is_null() {
            let string = unsafe{ std::ffi::CString::from_raw(self.err_string) };
            drop(string);
            self.err_string = core::ptr::null_mut();
        }
    }
}

struct RustMettaRunner(Metta);

impl From<Metta> for metta_t {
    fn from(metta: Metta) -> Self {
        Self{
            metta: Box::into_raw(Box::new(RustMettaRunner(metta))),
            err_string: core::ptr::null_mut(),
        }
    }
}

impl metta_t {
    fn borrow(&self) -> &Metta {
        &unsafe{ &*self.metta }.0
    }
    fn into_inner(mut self) -> Metta {
        self.free_err_string();
        unsafe{ Box::from_raw(self.metta).0 }
    }
}

/// @brief Creates a new top-level MeTTa Runner, with only the Rust stdlib loaded
/// @ingroup interpreter_group
/// @return A `metta_t` handle to the newly created runner
/// @note The caller must take ownership responsibility for the returned `metta_t`, and free it with `metta_free()`
///
#[no_mangle]
pub extern "C" fn metta_new() -> metta_t {
    let metta = Metta::new(None);
    metta.into()
}

/// @brief Creates a new top-level MeTTa Runner, with the specified `stdlib` module loaded
/// @ingroup interpreter_group
/// @param[in]  space  A pointer to a handle for the Space for use in the Runner's top-level module
/// @param[in]  environment  An `env_builder_t` handle to configure the environment to use
/// @param[in]  loader_callback  The `mod_loader_callback_t` for a function to load the stdlib, if it is
///     not already loaded.  Pass NULL to use the default `stdlib`
/// @param[in]  callback_context  A pointer to a caller-defined structure that will be passed to the
///     `loader_callback` function
/// @return A `metta_t` handle to the newly created Runner
/// @note The caller must take ownership responsibility for the returned `metta_t`, and free it with `metta_free()`
/// @note Most callers can simply call `metta_new`.  This function is provided to support languages
///     with their own stdlib, that needs to be loaded before the init.metta file is run
///
#[no_mangle]
pub extern "C" fn metta_new_with_space_environment_and_stdlib(space: *mut space_t,
    env_builder: env_builder_t, loader_callback: mod_loader_callback_t, callback_context: *mut c_void) -> metta_t
{
    let dyn_space = unsafe{ &*space }.borrow();
    let env_builder = if env_builder.is_default() {
        None
    } else {
        Some(env_builder.into_inner())
    };
    let loader_wrapper;
    let loader = match loader_callback {
        Some(callback) => {
            loader_wrapper = CModLoaderWrapper{ name: "stdlib".to_string(), callback, callback_context };
            Some(&loader_wrapper as &dyn ModuleLoader)
        },
        None => None
    };

    let metta = Metta::new_with_stdlib_loader(loader, Some(dyn_space.clone()), env_builder);
    metta.into()
}

/// Internal wrapper to turn a mod_loader_callback_t into an trait object that implements [ModuleLoader]
#[derive(Debug)]
struct CModLoaderWrapper {
    name: String,
    //NOTE: This function type matches the internals of mod_loader_callback_t.  This is necessary because
    //  CBindGen has some unfortunate inconsistencies around how Rust types are included in the C header
    callback: extern "C" fn(run_context: *mut run_context_t, descriptor: *const module_descriptor_t, callback_context: *mut c_void),
    callback_context: *mut c_void,
}

//FUTURE TODO.  See QUESTION around CFsModFmtLoader about whether we trust the C plugins to be reentrant
unsafe impl Send for CModLoaderWrapper {}
unsafe impl Sync for CModLoaderWrapper {}

impl ModuleLoader for CModLoaderWrapper {
    fn name(&self) -> Result<String, String> {
        Ok(self.name.clone())
    }
    fn load(&self, context: &mut RunContext, descriptor: ModuleDescriptor) -> Result<(), String> {
        let mut c_context = run_context_t::from(context);

        (self.callback)(&mut c_context, &descriptor.into(), self.callback_context);

        Ok(())
    }
}

/// @brief Creates a new core MeTTa runner, with no loaded stdlib nor initialization
/// @ingroup interpreter_group
/// @param[in]  space  A pointer to a handle for the Space for use as the space of the top-level module
/// @param[in]  environment  An `env_builder_t` handle to configure the environment to use
/// @return A `metta_t` handle to the newly created runner
/// @note The caller must take ownership responsibility for the returned `metta_t`, and free it with `metta_free()`
/// @note This function does not load any stdlib, nor does it run the `init.metta` file from the environment
///
#[no_mangle]
pub extern "C" fn metta_new_core(space: *mut space_t, env_builder: env_builder_t) -> metta_t {
    let dyn_space = unsafe{ &*space }.borrow();
    let env_builder = if env_builder.is_default() {
        None
    } else {
        Some(env_builder.into_inner())
    };
    let metta = Metta::new_core(dyn_space.clone(), env_builder);
    metta.into()
}

/// @brief Clones a `metta_t` handle
/// @ingroup interpreter_group
/// @param[in]  metta  The handle to clone
/// @return The newly cloned `metta_t` handle, pointing to the same underlying runner
/// @note The caller must take ownership responsibility for the returned `metta_t`, and free it with `metta_free()`
///
#[no_mangle]
pub extern "C" fn metta_clone_handle(metta: *const metta_t) -> metta_t {
    let metta = unsafe{ &*metta }.borrow();
    metta.clone().into()
}

/// @brief Frees a `metta_t` handle
/// @ingroup interpreter_group
/// @param[in]  metta  The handle to free
/// @note The underlying runner may be deallocated if all handles that refer to it have been freed, otherwise
///    the runner itself won't be freed
///
#[no_mangle]
pub extern "C" fn metta_free(metta: metta_t) {
    let metta = metta.into_inner();
    drop(metta);
}

/// @brief Returns the error string associated with the last `metta_run`, `metta_evaluate_atom`,
///     or `metta_load_module` call
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the MeTTa handle
/// @return A pointer to the C-string containing the error that occurred, or NULL if no
///     error occurred
/// @warning The returned pointer should NOT be freed.  It must never be accessed after the
///     metta_t has been freed, or any subsequent call to `metta_run`, `metta_evaluate_atom`, or
///     `metta_load_module` has been made.
///
#[no_mangle]
pub extern "C" fn metta_err_str(metta: *const metta_t) -> *const c_char {
    let metta = unsafe{ &*metta };
    metta.err_string
}

/// @brief Compares two `metta_t` handles to test whether the referenced MeTTa runner is the same
/// @ingroup interpreter_group
/// @param[in]  a  A pointer to the first runner handle
/// @param[in]  b  A pointer to the first runner handle
/// @return True if the two handles reference the same runner, otherwise False
///
#[no_mangle]
pub extern "C" fn metta_eq(a: *const metta_t, b: *const metta_t) -> bool {
    let a = unsafe{ &*a }.borrow();
    let b = unsafe{ &*b }.borrow();
    *a == *b
}

/// @brief Provides access to the Space of the runner's top-level module
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle
/// @return A Space handle, to access the Space of the runner's top-level module
/// @note The caller must take ownership responsibility for the returned `space_t` and free it with `space_free()`
///
#[no_mangle]
pub extern "C" fn metta_space(metta: *mut metta_t) -> space_t {
    let metta = unsafe{ &*metta }.borrow();
    metta.space().clone().into()
}

/// @brief Provides access to the Tokenizer of the runner's top-level module
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle
/// @return A Tokenizer handle, to access the Tokenizer of the runner's top-level module
/// @note The caller must take ownership responsibility for the returned `tokenizer_t` and free it with `tokenizer_free()`
///
#[no_mangle]
pub extern "C" fn metta_tokenizer(metta: *mut metta_t) -> tokenizer_t {
    let metta = unsafe{ &*metta }.borrow();
    metta.tokenizer().clone().into()
}

/// @brief Renders the working directory of the runner's environment into a buffer
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle
/// @param[out]  buf  A buffer into which the path will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the path string, minus the string terminator character.  Returns 0 if the
///    runner's environment has no working directory.
/// @note  If `return_value > buf_len + 1`, then the text was not fully rendered and this function
///    should be called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn metta_working_dir(metta: *const metta_t, buf: *mut c_char, buf_len: usize) -> usize {
    let metta = unsafe{ &*metta }.borrow();
    let text = match metta.environment().working_dir() {
        Some(path) => path.display().to_string(),
        None => "".to_string()
    };
    write_into_buf(&text, buf, buf_len)
}

/// @brief Runs the MeTTa runner until the input text has been fully parsed and evaluated
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle
/// @param[in]  parser  An S-Expression Parser containing the MeTTa text
/// @param[in]  callback  A function that will be called to provide a vector of atoms produced by the evaluation
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note If this function encounters an error, the callback will not be called and the error may be accessed with `metta_err_str()`
/// @warning  Ownership of the provided parser will be taken by this function, so it must not be subsequently accessed
///     nor freed.
///
#[no_mangle]
pub extern "C" fn metta_run(metta: *mut metta_t, parser: sexpr_parser_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let metta = unsafe{ &mut *metta };
    metta.free_err_string();
    let mut parser = parser.into_boxed_dyn();
    let rust_metta = metta.borrow();
    let results = rust_metta.run(&mut *parser);
    match results {
        Ok(results) => {
            for result in results {
                return_atoms(&result, callback, context);
            }
        },
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            metta.err_string = err_cstring.into_raw();
        }
    }
}

/// @brief Runs the MeTTa runner to evaluate an input Atom
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle
/// @param[in]  atom  The `atom_t` representing the atom to evaluate
/// @param[in]  callback  A function that will be called to provide a vector of atoms produced by the evaluation
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @note If this function encounters an error, the callback will not be called and the error may be accessed with `metta_err_str()`
/// @warning This function takes ownership of the provided `atom_t`, so it must not be subsequently accessed or freed
///
#[no_mangle]
pub extern "C" fn metta_evaluate_atom(metta: *mut metta_t, atom: atom_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let metta = unsafe{ &mut *metta };
    metta.free_err_string();
    let atom = atom.into_inner();
    let rust_metta = metta.borrow();
    let result = rust_metta.evaluate_atom(atom);
    match result {
        Ok(result) => return_atoms(&result, callback, context),
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            metta.err_string = err_cstring.into_raw();
        }
    }
}

/// @brief Loads a module directly into the runner, from a mod_loader_callback_t
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the handle specifying the runner into which to load the module
/// @param[in]  name  A C-string specifying a name for the module
/// @param[in]  private_to  If the module is private, pass a pointer to the `module_descriptor_t` of the
///    the parent module.  Passing NULL will make the module available for loading by any other module
/// @param[in]  loader_callback  The `mod_loader_callback_t` for a function to load the module
/// @param[in]  callback_context  A pointer to a caller-defined structure that will be passed to the
///    `loader_callback` function
/// @return  The `module_id_t` for the loaded module, or `invalid` if there was an error
/// @note  This function might be useful to provide MeTTa modules that are built-in as part of your
///    application
/// @note If this function encounters an error, the error may be accessed with `metta_err_str()`
///
#[no_mangle]
pub extern "C" fn metta_load_module_direct(metta: *mut metta_t,
        name: *const c_char,
        private_to: *const module_descriptor_t,
        loader_callback: mod_loader_callback_t,
        callback_context: *mut c_void) -> module_id_t {

    let metta = unsafe{ &mut *metta };
    metta.free_err_string();
    let rust_metta = metta.borrow();
    let name = cstr_as_str(name);
    let private_to = if private_to.is_null() {
        None
    } else {
        let parent_descriptor = unsafe { &*private_to }.borrow();
        Some(parent_descriptor)
    };
    let loader_callback = loader_callback.unwrap();
    let loader = CModLoaderWrapper{ name: name.to_string(), callback: loader_callback, callback_context };

    match rust_metta.load_module_direct(&loader, private_to) {
        Ok(mod_id) => mod_id.into(),
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            metta.err_string = err_cstring.into_raw();
            ModId::INVALID.into()
        }
    }
}

/// @brief Loads a module into the runner from a module resource at a file system path
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the handle specifying the runner into which to load the module
/// @param[in]  path  A C-string specifying the path from which to load the module
/// @param[in]  name  A C-string specifying a name for the module, or NULL if you want the module to
///    be private and unable to be loaded by name
/// @return  The `module_id_t` for the loaded module, or `invalid` if there was an error
/// @note  This function effectively bypasses the catalog, for situations where you wish to load a
///    specific module from disk
/// @note If this function encounters an error, the error may be accessed with `metta_err_str()`
///
#[no_mangle]
pub extern "C" fn metta_load_module_at_path(metta: *mut metta_t,
        path: *const c_char, name: *const c_char) -> module_id_t {

    let metta = unsafe{ &mut *metta };
    metta.free_err_string();
    let rust_metta = metta.borrow();
    let path = PathBuf::from(cstr_as_str(path));
    let mod_name = if !name.is_null() {
        Some(cstr_as_str(name))
    } else {
        None
    };

    match rust_metta.load_module_at_path(path, mod_name) {
        Ok(mod_id) => mod_id.into(),
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            metta.err_string = err_cstring.into_raw();
            ModId::INVALID.into()
        }
    }
}

/// @brief Returns the Space for a loaded module
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the handle specifying the runner into which to load the module
/// @param[in]  mod_id  The `module_id_t` of the loaded module to access
/// @return  The `space_t` for the module's Space
/// @note The returned `space_t` must be freed with `space_free`
///
#[no_mangle]
pub extern "C" fn metta_get_module_space(metta: *const metta_t, mod_id: module_id_t) -> space_t {
    let metta = unsafe{ &*metta }.borrow();
    let mod_id = mod_id.into_inner();

    metta.module_space(mod_id).into()
}

/// @brief An interface object providing access to the MeTTa run interface
/// @ingroup interpreter_group
///
#[repr(C)]
pub struct run_context_t {
    /// Internal.  Should not be accessed directly
    context: *mut RustRunContext,
}

//LP-TODO-NEXT I need to implement a solution to automatically retire run_context_t so we can throw a
// predictable error when a stale run_context_t is accessed.  This is particularly important for the Python
// layer because it's harder to exercise lifecycle discipline in Python and a bug in Python shouldn't lead
// to invlid memory access

struct RustRunContext(RunContext<'static, 'static, 'static>);

impl From<&mut RunContext<'_, '_, '_>> for run_context_t {
    fn from(context_ref: &mut RunContext<'_, '_, '_>) -> Self {
        Self {
            context: (context_ref as *mut RunContext<'_, '_, '_>).cast()
        }
    }
}

impl run_context_t {
    fn borrow(&self) -> &RunContext<'static, 'static, 'static> {
        unsafe{ &*self.context.cast::<RunContext<'static, 'static, 'static>>() }
    }
    fn borrow_mut(&mut self) -> &mut RunContext<'static, 'static, 'static> {
        unsafe{ &mut *self.context.cast::<RunContext<'static, 'static, 'static>>() }
    }
}

/// @brief Appends the parser to the Run Context's queue of input to run
/// @ingroup interpreter_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @param[in]  parser  An S-Expression Parser containing the MeTTa source code to execute
///
#[no_mangle]
pub extern "C" fn run_context_push_parser(run_context: *mut run_context_t, parser: sexpr_parser_t) {
    let context = unsafe{ &mut *run_context }.borrow_mut();
    let parser = parser.into_boxed_dyn();

    context.push_parser(parser)
}

/// @brief Returns a pointer to the `metta_t` runner that a run context is executing within
/// @ingroup interpreter_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @return A `metta_t` handle to access the runner
/// @note  The returned `metta_t` handle is must be freed using `metta_free`
///
//TODO: I would like a way to return a borrowed `*const metta_t` to avoid the Arc clone and subsequent
// refcount decrement.  Unfortunately I'd need to make a more complicated wrapper in the vein of atom_ref_t
// in order to make this work, and I am not sure the performance is worth the complexity right now.
#[no_mangle]
pub extern "C" fn run_context_get_metta(run_context: *const run_context_t) -> metta_t {
    let context = unsafe{ &*run_context }.borrow();
    context.metta().clone().into()
}

/// @brief Provides access to the Space of the currently running module
/// @ingroup interpreter_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @return A Space handle, to access the Space of the currently running module
/// @note The caller must take ownership responsibility for the returned `space_t` and free it with
///    `space_free()`
///
#[no_mangle]
pub extern "C" fn run_context_get_space(run_context: *const run_context_t) -> space_t {
    let context = unsafe{ &*run_context }.borrow();
    context.module().space().clone().into()
}

/// @brief Provides access to the Tokenizer of the currently running module
/// @ingroup interpreter_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @return A Tokenizer handle, to access the Tokenizer of the currently running module
/// @note The caller must take ownership responsibility for the returned `tokenizer_t` and free it with
///     `tokenizer_free()`
///
#[no_mangle]
pub extern "C" fn run_context_get_tokenizer(run_context: *const run_context_t) -> tokenizer_t {
    let context = unsafe{ &*run_context }.borrow();
    context.module().tokenizer().clone().into()
}

/// @brief Represents the state of an in-flight MeTTa execution run
/// @ingroup interpreter_group
/// @note A `runner_state_t` is initially created by `runner_state_new_with_parser()`.  Each call to `metta_run_step()`, in
///    a loop, advances the evaluation progress by some amount.  When the runner operations have
///    fully resolved, `runner_state_is_complete()` will return true.  Ownership of the `runner_state_t`
///    must ultimately be released with `runner_state_free()`.
///
#[repr(C)]
pub struct runner_state_t {
    /// Internal.  Should not be accessed directly
    state: *mut RustRunnerState,
    err_string: *mut c_char,
}

struct RustRunnerState(RunnerState<'static, 'static>);

impl runner_state_t {
    fn free_err_string(&mut self) {
        if !self.err_string.is_null() {
            let string = unsafe{ std::ffi::CString::from_raw(self.err_string) };
            drop(string);
            self.err_string = core::ptr::null_mut();
        }
    }
}

impl From<RunnerState<'static, 'static>> for runner_state_t {
    fn from(state: RunnerState<'static, 'static>) -> Self {
        Self{
            state: Box::into_raw(Box::new(RustRunnerState(state))),
            err_string: core::ptr::null_mut(),
        }
    }
}

impl runner_state_t {
    fn into_inner(mut self) -> RunnerState<'static, 'static> {
        self.free_err_string();
        unsafe{ Box::from_raw(self.state).0 }
    }
    fn borrow(&self) -> &RunnerState<'static, 'static> {
        &unsafe{ &*(&*self).state }.0
    }
    fn borrow_mut(&mut self) -> &mut RunnerState<'static, 'static> {
        &mut unsafe{ &mut *(&*self).state }.0
    }
}

/// @brief Creates a runner_state_t, to use for step-wise execution of MeTTa text
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle in which to perform the run
/// @param[in]  parser An S-Expression Parser containing the MeTTa text
/// @return The newly created `runner_state_t`, which can begin evaluating MeTTa code
/// @warning  Ownership of the provided parser will be taken by this function, so it must not be subsequently accessed
///     nor freed.
/// @note The returned `runner_state_t` handle must be freed with `runner_state_free()`
///
#[no_mangle]
pub extern "C" fn runner_state_new_with_parser(metta: *const metta_t, parser: sexpr_parser_t) -> runner_state_t {
    let metta = unsafe{ &*metta }.borrow();
    let parser = parser.into_boxed_dyn();
    let state = RunnerState::new_with_parser(metta, parser);
    state.into()
}

/// @brief Creates a runner_state_t, to use for step-wise execution of a list of atoms
/// @ingroup interpreter_group
/// @param[in]  metta  A pointer to the runner handle in which to perform the run
/// @param[in]  atoms A pointer to an `atom_vec_t` containing the atoms to run
/// @return The newly created `runner_state_t`, which can begin evaluating MeTTa code
/// @warning  The referenced `atoms` `atom_vec_t` must not be modified nor freed while the
///    `runner_state_t` remains active
/// @note The returned `runner_state_t` handle must be freed with `runner_state_free()`
///
#[no_mangle]
pub extern "C" fn runner_state_new_with_atoms(metta: *const metta_t, atoms: *const atom_vec_t) -> runner_state_t {
    let metta = unsafe{ &*metta }.borrow();
    let atoms = unsafe{ &*atoms }.as_slice();
    let state = RunnerState::new_with_atoms(metta, atoms);
    state.into()
}

/// @brief Frees a runner_state_t
/// @ingroup interpreter_group
/// @param[in]  node  The `runner_state_t` to free
///
#[no_mangle]
pub extern "C" fn runner_state_free(state: runner_state_t) {
    let state = state.into_inner();
    drop(state);
}

/// @brief Returns the error string associated with the last `runner_state_step`
/// @ingroup interpreter_group
/// @param[in]  state  A pointer to the runner state
/// @return A pointer to the C-string containing the error that occurred, or NULL if no
///     error occurred
/// @warning The returned pointer should NOT be freed.  It must never be accessed after the
///     runner_state_t has been freed, or any subsequent call to `runner_state_step` has been made.
///
#[no_mangle]
pub extern "C" fn runner_state_err_str(state: *const runner_state_t) -> *const c_char {
    let state = unsafe{ &*state };
    state.err_string
}

/// @brief Runs one step of the runner
/// @ingroup interpreter_group
/// @param[in]  state  A pointer to the in-flight runner state
/// @note If this function encounters an error, the error may be accessed with `runner_state_err_str()`
///
#[no_mangle]
pub extern "C" fn runner_state_step(state: *mut runner_state_t) {
    let state = unsafe{ &mut *state };
    state.free_err_string();
    let rust_state = state.borrow_mut();
    match rust_state.run_step() {
        Ok(_) => {},
        Err(err) => {
            let err_cstring = std::ffi::CString::new(err).unwrap();
            state.err_string = err_cstring.into_raw();
        }
    }
}

/// @brief Returns whether or not the runner_state_t has completed all outstanding work
/// @ingroup interpreter_group
/// @param[in]  state  The `runner_state_t` to inspect
/// @return `true` if the runner has already concluded, or `false` if there is more work to do
///
#[no_mangle]
pub extern "C" fn runner_state_is_complete(state: *const runner_state_t) -> bool {
    let state = unsafe{ &*state }.borrow();
    state.is_complete()
}

/// @brief Renders a text description of a `runner_state_t` into a buffer
/// @ingroup interpreter_group
/// @param[in]  state  A pointer to a `runner_state_t` to render
/// @param[out]  buf  A buffer into which the text will be rendered
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the description string, minus the string terminator character.  If
///    `return_value > buf_len + 1`, then the text was not fully rendered and this function should be
///    called again with a larger buffer.
///
#[no_mangle]
pub extern "C" fn runner_state_to_str(state: *const runner_state_t, buf: *mut c_char, buf_len: usize) -> usize {
    let state = unsafe{ &*state }.borrow();
    write_debug_into_buf(state, buf, buf_len)
}

/// @brief Accesses the current in-flight results in the runner_state_t
/// @ingroup interpreter_group
/// @param[in]  state  The `runner_state_t` within which to preview results
/// @param[in]  callback  A function that will be called to provide a vector of atoms produced by the evaluation
/// @param[in]  context  A pointer to a caller-defined structure to facilitate communication with the `callback` function
/// @warning The provided results will be overwritten by the next call to `metta_run_step`, so the caller
///     must clone the result atoms if they are needed for an extended period of time
///
#[no_mangle]
pub extern "C" fn runner_state_current_results(state: *const runner_state_t,
        callback: c_atom_vec_callback_t, context: *mut c_void) {
    let state = unsafe{ &*state }.borrow();
    let results = state.current_results();
    for result in results {
        return_atoms(result, callback, context);
    }
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Environment Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Renders the config_dir path from the common environment into a text buffer
/// @ingroup environment_group
/// @param[out]  buf  A buffer into which the text will be written
/// @param[in]  buf_len  The maximum allocated size of `buf`
/// @return The length of the path string, minus the string terminator character.  If
/// `return_value > buf_len + 1`, then the text was not fully written and this function should be
/// called again with a larger buffer.  This function will return 0 if there is no config_dir.
///
#[no_mangle]
pub extern "C" fn environment_config_dir(buf: *mut c_char, buf_len: usize) -> usize {
    match Environment::common_env().config_dir() {
        Some(path) => write_into_buf(path.display(), buf, buf_len),
        None => write_into_buf("", buf, buf_len) //Write just the terminator char, if there is room
    }
}

/// @brief Represents an environment initialization, in progress
/// @ingroup environment_group
/// @note `env_builder_t` must be given to `environment_init_finish()` to properly release it
///
#[repr(C)]
pub struct env_builder_t {
    /// Internal.  Should not be accessed directly
    builder: *mut RustEnvBuilder,
}

struct RustEnvBuilder(EnvBuilder);

impl From<EnvBuilder> for env_builder_t {
    fn from(builder: EnvBuilder) -> Self {
        Self{ builder: Box::into_raw(Box::new(RustEnvBuilder(builder))) }
    }
}

impl env_builder_t {
    fn is_default(&self) -> bool {
        self.builder.is_null()
    }
    fn into_inner(self) -> EnvBuilder {
        if self.is_default() {
            panic!("Fatal Error, default env_builder_t cannot be accessed")
        }
        unsafe{ Box::from_raw(self.builder).0 }
    }
    fn null() -> Self {
        Self {builder: core::ptr::null_mut()}
    }
}

/// @brief Begins initialization of an environment
/// @ingroup environment_group
/// @return The `env_builder_t` object representing the in-process environment initialization
/// @note The `env_builder_t` must be passed to either `env_builder_init_common_env`
///     or `metta_new_with_space` in order to properly deallocate it
///
#[no_mangle]
pub extern "C" fn env_builder_start() -> env_builder_t {
    EnvBuilder::new().into()
}

/// @brief Creates an `env_builder_t` to specify that the default common environment should be used
/// @ingroup environment_group
/// @return The `env_builder_t` object specifying the common environment
/// @note This function exists to supply an argument to `metta_new_with_space` when no special
///     behavior is desired
/// @note The `env_builder_t` must be passed to `metta_new_with_space`
///
#[no_mangle]
pub extern "C" fn env_builder_use_default() -> env_builder_t {
    env_builder_t::null()
}

/// @brief A convenience to create an `env_builder_t`, to specify that a unit-test environment should be used
/// @ingroup environment_group
/// @return The `env_builder_t` object specifying the unit test environment
/// @note This function exists to supply an argument to `metta_new_with_space` when performing unit testing
/// @note The `env_builder_t` must be passed to `metta_new_with_space`
///
#[no_mangle]
pub extern "C" fn env_builder_use_test_env() -> env_builder_t {
    EnvBuilder::test_env().into()
}

/// @brief Finishes initialization of the common environment
/// @ingroup environment_group
/// @param[in]  builder  The in-process environment builder state to install as the common environment
/// @return  True if the environment was sucessfully initialized with the provided builder state.  False
///     if the environment had already been initialized by a prior call
///
#[no_mangle]
pub extern "C" fn env_builder_init_common_env(builder: env_builder_t) -> bool {
    let builder = builder.into_inner();
    builder.try_init_common_env().is_ok()
}

/// @brief Sets the working directory for the environment
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
/// @param[in]  path  A C-string specifying a path to a working directory, to search for modules to load.
///     Passing `NULL` will unset the working directory
/// @note This working directory is not required to be the same as the process working directory, and
///   it will not change as the process' working directory is changed
///
#[no_mangle]
pub extern "C" fn env_builder_set_working_dir(builder: *mut env_builder_t, path: *const c_char) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = if path.is_null() {
        builder.set_working_dir(None)
    } else {
        builder.set_working_dir(Some(&PathBuf::from(cstr_as_str(path))))
    };
    *builder_arg_ref = builder.into();
}

/// @brief Sets the config directory for the environment.  A directory at the specified path
/// will be created, and its contents populated with default values, if one does not already exist
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
/// @param[in]  path  A C-style string specifying a path to a working directory, to search for modules to load
///
#[no_mangle]
pub extern "C" fn env_builder_set_config_dir(builder: *mut env_builder_t, path: *const c_char) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = if path.is_null() {
        panic!("Fatal Error: path cannot be NULL");
    } else {
        builder.set_config_dir(&PathBuf::from(cstr_as_str(path)))
    };
    *builder_arg_ref = builder.into();
}

/// @brief Configures the environment to create the config dir if it doesn't already exist
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
///
#[no_mangle]
pub extern "C" fn env_builder_create_config_dir(builder: *mut env_builder_t) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = builder.create_config_dir();
    *builder_arg_ref = builder.into();
}

/// @brief Configures the environment so that no config directory will be read nor created
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
///
#[no_mangle]
pub extern "C" fn env_builder_disable_config_dir(builder: *mut env_builder_t) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = builder.set_no_config_dir();
    *builder_arg_ref = builder.into();
}

/// @brief Configures the environment for use in unit testing
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
/// @param[in]  is_test  True if the environment is a unit-test environment, False otherwise
///
#[no_mangle]
pub extern "C" fn env_builder_set_is_test(builder: *mut env_builder_t, is_test: bool) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = builder.set_is_test(is_test);
    *builder_arg_ref = builder.into();
}

/// @brief Adds a directory to search for module imports
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
/// @param[in]  path  A C-style string specifying a path to a working directory, to search for modules to load
/// @note The paths will be searched in the order they are added to the `env_builder_t``
///
#[no_mangle]
pub extern "C" fn env_builder_push_include_path(builder: *mut env_builder_t, path: *const c_char) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let builder = if path.is_null() {
        panic!("Fatal Error: path cannot be NULL");
    } else {
        builder.push_include_path(PathBuf::from(cstr_as_str(path)))
    };
    *builder_arg_ref = builder.into();
}

/// @brief Adds logic to interpret a foreign format for MeTTa modules loaded from the file system
/// @ingroup environment_group
/// @param[in]  builder  A pointer to the in-process environment builder state
/// @param[in]  api  A pointer to the `mod_file_fmt_api_t` table of functions to define the behavior of the
///    module format
/// @param[in]  payload  A pointer to a user-defined structure to store information related to this format
/// @warning The data referenced by both the `api` and the `payload` pointers must remain valid for the
///    entire life of the environment.  In the case of the common_env, that is the entire life of the program
/// @note Formats will be tried in the order they are added to the `env_builder_t``
///
#[no_mangle]
pub extern "C" fn env_builder_push_fs_module_format(builder: *mut env_builder_t, api: *const mod_file_fmt_api_t, payload: *const c_void) {
    let builder_arg_ref = unsafe{ &mut *builder };
    let builder = core::mem::replace(builder_arg_ref, env_builder_t::null()).into_inner();
    let c_loader = CFsModFmtLoader::new(api, payload);
    let builder = builder.push_fs_module_format(c_loader);
    *builder_arg_ref = builder.into();
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Module Interface
// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

/// @brief Identifies the properties on a specific module, including its name and version
/// @ingroup module_group
/// @note `module_descriptor_t` objects must be freed with `module_descriptor_free` or passed to
///    a function that assumes responsibility for freeing them
///
#[repr(C)]
pub struct module_descriptor_t {
    /// Internal.  Should not be accessed directly
    descriptor: *mut RustModuleDescriptor,
}

#[derive(Clone)]
enum RustModuleDescriptor {
    Descriptor(ModuleDescriptor),
    Err(String)
}

impl From<ModuleDescriptor> for module_descriptor_t {
    fn from(descriptor: ModuleDescriptor) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(RustModuleDescriptor::Descriptor(descriptor))) }
    }
}

//LP-TODO-NEXT.  The "interpreter" documentation group is bloating.  Should break out a separate "runner" group
//LP-TODO-NEXT.  The Error pathway of the module_descriptor_t is no longer used.  So I can simplify this type
//LP-TODO-NEXT.  Add an error pathway to run_context_t, that mirrors the error pathway in metta_t, so calls where a run_context_t is passed can propagate errors back to the caller
//LP-TODO-Next.  Add an error-status result to the callbacks.  At a conceptual level, this error type fundamentally the same exec_error_t / ExecError

impl module_descriptor_t {
    fn new_err(err: String) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(RustModuleDescriptor::Err(err))) }
    }
    fn new_from_enum(rust_enum: RustModuleDescriptor) -> Self {
        Self{ descriptor: Box::into_raw(Box::new(rust_enum)) }
    }
    fn into_rust_enum(self) -> RustModuleDescriptor {
        unsafe{ *Box::from_raw(self.descriptor) }
    }
    fn borrow_rust_enum(&self) -> &RustModuleDescriptor {
        unsafe{ &*self.descriptor }
    }
    fn borrow(&self) -> &ModuleDescriptor {
        match unsafe{ &*self.descriptor } {
            RustModuleDescriptor::Descriptor(desc) => desc,
            RustModuleDescriptor::Err(_err) => panic!("Fatal error.  Attempt to access Error module_descriptor_t")
        }
    }
}

/// @brief Identifies a loaded module inside a specific `metta_t` MeTTa runner
/// @ingroup module_group
/// @note It is not necessary to free `module_id_t` types
///
#[repr(C)]
pub struct module_id_t {
    /// Internal.  Should not be accessed directly
    id: usize,
}

impl From<ModId> for module_id_t {
    fn from(mod_id: ModId) -> Self {
        module_id_t{ id: mod_id.0 }
    }
}

impl module_id_t {
    fn into_inner(self) -> ModId {
        ModId(self.id)
    }
}

/// @brief Returns `true` is a module_id_t is valid, otherwise returns `false`
/// @ingroup module_group
/// @param[in]  mod_id  A pointer to the `module_id_t` to test for validity
/// @return `true` if the module_id_t is valid, otherwise returns `false`
///
#[no_mangle]
pub extern "C" fn module_id_is_valid(mod_id: *const module_id_t) -> bool {
    let mod_id = unsafe{ &*mod_id };
    ModId(mod_id.id) != ModId::INVALID
}

/// @brief Creates a new module_descriptor_t with the specified name
/// @ingroup module_group
/// @param[in]  name  A C-style string containing the name of the module
/// @return The new `module_descriptor_t`
/// @note The returned `module_descriptor_t` must be freed with `module_descriptor_free()`
///
#[no_mangle]
pub extern "C" fn module_descriptor_new(name: *const c_char) -> module_descriptor_t {
    ModuleDescriptor::new(cstr_as_str(name).to_string()).into()
}

/// @brief Creates a new module_descriptor_t that represents the error attempting to interpret a module
/// @ingroup module_group
/// @param[in]  err_str  A C-style string containing the error message
/// @return The new error `module_descriptor_t`
///
#[no_mangle]
pub extern "C" fn module_descriptor_error(err_str: *const c_char) -> module_descriptor_t {
    module_descriptor_t::new_err(cstr_as_str(err_str).to_string()).into()
}

/// @brief Creates a new module_descriptor_t that is a clone of the argument passed
/// @ingroup module_group
/// @param[in]  descriptor  The `module_descriptor_t` to clone
/// @return The new `module_descriptor_t`
/// @note The returned `module_descriptor_t` must be freed with `module_descriptor_free()`
///
#[no_mangle]
pub extern "C" fn module_descriptor_clone(descriptor: *const module_descriptor_t) -> module_descriptor_t {
    let rust_enum = unsafe{ &*descriptor }.borrow_rust_enum();
    module_descriptor_t::new_from_enum(rust_enum.clone())
}

/// @brief Frees a module_descriptor_t
/// @ingroup module_group
/// @param[in]  descriptor  The `module_descriptor_t` to free
///
#[no_mangle]
pub extern "C" fn module_descriptor_free(descriptor: module_descriptor_t) {
    let descriptor = descriptor.into_rust_enum();
    drop(descriptor);
}

/// @brief A callback to loads a module into a runner, by making calls into the `run_context_t`
/// @ingroup module_group
/// @param[in]  run_context  The `run_context_t` to provide access to the MeTTa run interface
/// @param[in]  descriptor  The `module_descriptor_t` to pass to `run_context_init_self_module`
/// @param[in]  callback_context  The state pointer initially passed to the upstream function
///
pub type mod_loader_callback_t = Option<extern "C" fn(run_context: *mut run_context_t, descriptor: *const module_descriptor_t, callback_context: *mut c_void)>;

/// @struct mod_file_fmt_api_t
/// @brief A table of functions to load MeTTa modules from an arbitrary format
/// @ingroup module_group
/// @warning All of the functions in this interface may be called from threads outside the main
///    thread, and may be called concurrently.  Therefore these functions must be fully reentrant.
///
#[repr(C)]
pub struct mod_file_fmt_api_t {

    /// @brief Constructs a path for a module with a given name that resides in a parent directory
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  parent_dir  A NULL-terminated string, representing the path to the parent directory
    ///    in the file system
    /// @param[in]  mod_name  A NULL-terminated string, representing the name of the module
    /// @param[out]  dst_buf  The buffer into which to write the output path, followed by a NULL character
    /// @param[in]  buf_size  The size of the allocated dst_buf.  This function must not overwrite the
    ///    output buffer
    /// @return the number of bytes written into the `dst_buf` by the function, including a NULL terminator
    ///    character.  If the path does not fit in the `dst_buf`, then this function should return 0
    /// @note The implementation does not need to check the validity of the returned path.  Results from
    ///    this method will be passed to `try_path` to perform validity checking
    ///
    //QUESTION: The Rust interface allows for a single module loader to check multiple path variations.
    //    That could be implemented here in the C interface but it would complicate the API functions
    //    by requiring multiple buffers to be returned, and currently it's not needed.
    path_for_name: extern "C" fn(payload: *const c_void, parent_dir: *const c_char, mod_name: *const c_char, dst_buf: *mut c_char, buf_size: usize) -> usize,

    /// @brief Tests a path in the file system to determine if a valid module resides at the path
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  path  A NULL-terminated string, representing a path in the file system to test
    /// @param[in]  mod_name  A NULL-terminated string, representing the name of the module
    /// @return any non-NULL value if the `path` contains a valid module in the format, otherwise NULL.
    ///    If a non-NULL value is returned from this function, it may be passed to the `load` function
    ///    as the `callback_context` argument.  If a non-NULL value is returned, it will eventually
    ///    trigger a call to `free_callback_context` so the returned value can be an allocated pointer.
    ///
    //TODO: This function will also be responsible for returning a module version, through an [out] arg,
    // when I add versions in the near future
    try_path: extern "C" fn(payload: *const c_void, path: *const c_char, mod_name: *const c_char) -> *mut c_void,

    /// @brief Loads the module into the runner, by making calls into the `run_context_t`
    /// @param[in]  payload  The payload passed to `env_builder_push_fs_module_format` when the
    ///    format was initialized.  This function must not modify the payload
    /// @param[in]  path  A NULL-terminated string, representing a path in the file system pointing to
    ///    the module
    /// @param[in]  run_context  The `run_context_t` to provide access to the MeTTa run interface
    /// @param[in]  descriptor  The `module_descriptor_t` to pass to `run_context_init_self_module`
    ///
    ///QUESTION: What should the error reporting pathway look like?
    ///QUESTION: Is it worth trying to unify this function prototype with the `mod_loader_callback_t` type?
    ///   The argument in favor is that they're fundamentally doing the same thing, but the function params
    ///   are different on account of having a payload, and removing the `payload` argument complicates the
    ///   API as it becomes necessary to repackage the payload inside the `callback_context`
    load: extern "C" fn(payload: *const c_void, run_context: *mut run_context_t, descriptor: *const module_descriptor_t, callback_context: *mut c_void),

    /// @brief Frees a user-defined structure that may have been allocated in `try_path`.
    /// @param[in]  callback_context  The value returned from `try_path`, if it was non-NULL
    ///
    free_callback_context: Option<extern "C" fn(callback_context: *mut c_void)>,
}

#[derive(Clone, Debug)]
struct CFsModFmtLoader {
    api: *const mod_file_fmt_api_t,
    payload: *const c_void,
    callback_context: *mut c_void,
    mod_name: Option<String>,
}

impl CFsModFmtLoader {
    fn new(api: *const mod_file_fmt_api_t, payload: *const c_void) -> Self {
        Self {api, payload, callback_context: core::ptr::null_mut(), mod_name: None }
    }
}

//QUESTION: What should our multi-thread cross-language API look like?  (I am seeing this as establishing
//  a pattern we can also use for Grounded Atoms, Spaces, etc.)
//
//I see several possible approaches:
// 1.) Let the C implementations ensure their own concurrency.  This preserves the option for optimal speed,
//  but might invite problems with badly-behaved extensions.  Especially for host languages where the
//  interpreter is bound to a specific thread such as Python or JavaScript.
// 2.) Implement a single-threaded API where we transact all interactions with the host language through
//  queues to a single interface thread.  This is less rope for the user to hang themselves, but will be
//  a fundamental bottleneck, so I'd prefer not to go this route
//
//My strong opinion is we leave the C API unlimited (ie. Option 1).  Then we can implement the queueing
//  & synchronization for Python within the hyperonpy layer.  My preference would be to undertake
//  https://github.com/trueagi-io/hyperon-experimental/issues/283 before trying to implement the queueing &
//  synchronization layer.  My reasoning is that the bug-surface-area will be a lot smaller in Rust than in C++
unsafe impl Send for CFsModFmtLoader {}
unsafe impl Sync for CFsModFmtLoader {}

impl FsModuleFormat for CFsModFmtLoader {
    fn paths_for_name(&self, parent_dir: &Path, mod_name: &str) -> Vec<PathBuf> {
        let api = unsafe{ &*self.api };

        let parent_dir_c_string = str_as_cstr(parent_dir.to_str().unwrap());
        let mod_name_c_string = str_as_cstr(mod_name);
        const BUF_SIZE: usize = 512;
        let mut buffer = [0i8; BUF_SIZE];

        let bytes_written = (api.path_for_name)(
            self.payload,
            parent_dir_c_string.as_ptr(),
            mod_name_c_string.as_ptr(),
            buffer.as_mut_ptr(),
            BUF_SIZE
        );
        if bytes_written > 0 {
            vec![PathBuf::from(cstr_as_str(buffer[0..=bytes_written].as_ptr()))]
        } else {
            vec![]
        }
    }
    fn try_path(&self, path: &Path, mod_name: Option<&str>) -> Option<Box<dyn ModuleLoader>> {
        let api = unsafe{ &*self.api };
        let path_c_string = str_as_cstr(path.to_str().unwrap());
        let mod_name = match mod_name {
            Some(mod_name) => mod_name,
            None => path.file_stem().unwrap().to_str().unwrap()
        };
        let mod_name_c_string = str_as_cstr(mod_name);

        let result_context = (api.try_path)(self.payload, path_c_string.as_ptr(), mod_name_c_string.as_ptr());
        if !result_context.is_null() {
            let mut new_loader = self.clone();
            new_loader.mod_name = Some(mod_name.to_string());
            new_loader.callback_context = result_context;
            Some(Box::new(new_loader))
        } else {
            None
        }
    }
}

impl ModuleLoader for CFsModFmtLoader {
    fn name(&self) -> Result<String, String> {
        Ok(self.mod_name.clone().unwrap())
    }
    fn load(&self, context: &mut RunContext, descriptor: ModuleDescriptor) -> Result<(), String> {
        let api = unsafe{ &*self.api };
        let mut c_context = run_context_t::from(context);

        (api.load)(self.payload, &mut c_context, &descriptor.into(), self.callback_context);

        Ok(())
    }
}

impl Drop for CFsModFmtLoader {
    fn drop(&mut self) {
        let api = unsafe{ &*self.api };

        if let Some(free_func) = api.free_callback_context {
            if !self.callback_context.is_null() {
                free_func(self.callback_context)
            }
        }
    }
}

/// @brief Called within a module `loader` function to initialize the new module
/// @ingroup module_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @param[in]  descriptor  A `module_descriptor_t` for the new module being initialized.  This should
///    be the descriptor argument received in the loader function.  This function takes ownership of the
///    the `module_descriptor_t` so it should not be freed
/// @param[in]  space  A pointer to a handle for the new module's space
/// @param[in]  working_dir_path   A C-style string specifying a file system path to use as the module's
///    working directory.  Passing `NULL` means the module does not have a working directory
/// @note this function must be called exactly once within a module loader function
///
#[no_mangle]
pub extern "C" fn run_context_init_self_module(run_context: *mut run_context_t,
        descriptor: *const module_descriptor_t,
        space: *mut space_t,
        working_dir_path: *const c_char) {

    let context = unsafe{ &mut *run_context }.borrow_mut();
    let descriptor = unsafe{ &*descriptor }.borrow();
    let dyn_space = unsafe{ &*space }.borrow();
    let path = if working_dir_path.is_null() {
        None
    } else {
        Some(PathBuf::from(cstr_as_str(working_dir_path)))
    };

    context.init_self_module(descriptor.clone(), dyn_space.clone(), path);
}

/// @brief Resolves a module name in the context of a running module, and loads that module
///    if it's not already loaded
/// @ingroup module_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @param[in]  name  A C-style string containing the module name
/// @return  The module_id_t for the loaded module, or `invalid` if there was an error
///
#[no_mangle]
pub extern "C" fn run_context_load_module(run_context: *mut run_context_t, name: *const c_char) -> module_id_t {
    let run_context = unsafe{ &mut *run_context }.borrow_mut();
    let result = run_context.load_module(cstr_as_str(name));
    match result {
        Ok(mod_id) => mod_id.into(),
        Err(_err) => {
            //TODO, propagate the error, so the caller can access it.
            // let err_cstring = std::ffi::CString::new(err).unwrap();
            // metta.err_string = err_cstring.into_raw();
            ModId::INVALID.into()
        }
    }
}

/// @brief Imports a dependency module into the currently running module.  This is "import *" behavior
/// @ingroup module_group
/// @param[in]  run_context  A pointer to the `run_context_t` to access the runner API
/// @param[in]  mod_id  The loaded `module_id_t` of the module to import
///
#[no_mangle]
pub extern "C" fn run_context_import_dependency(run_context: *mut run_context_t, mod_id: module_id_t) {
    let context = unsafe{ &mut *run_context }.borrow_mut();

    context.module().import_dependency_legacy(context.metta(), mod_id.into_inner()).unwrap();
}

//LP-TODO-NEXT: Add custom format loading, and custom stdlib tests for Rust