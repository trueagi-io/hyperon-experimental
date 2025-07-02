//! MeTTa parser implementation.

use hyperon_atom::*;
use hyperon_atom::gnd::*;

use core::ops::Range;
use std::iter::Peekable;
use std::iter::Enumerate;
use regex::Regex;
use std::rc::Rc;
use unicode_reader::CodePoints;
use std::io;
use std::io::Read;

#[derive(Clone, Debug)]
pub struct Tokenizer {
    tokens: Vec<TokenDescr>,
}

#[derive(Clone)]
struct TokenDescr {
    regex: Regex,
    constr: Rc<AtomConstr>,
}

impl std::fmt::Debug for TokenDescr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TokenDescr{{ regex: {:?}, constr: {:?} }}", self.regex, Rc::as_ptr(&self.constr))
    }
}

type AtomConstr = dyn Fn(&str) -> Result<Atom, String>;

impl Tokenizer {

    pub fn new() -> Self {
        Self{ tokens: Vec::new() }
    }

    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.register_token_with_func_ptr(regex, Rc::new(move |the_str| Ok(constr(the_str))))
    }

    pub fn register_fallible_token<C: 'static + Fn(&str) -> Result<Atom, String>>(&mut self, regex: Regex, constr: C) {
        self.register_token_with_func_ptr(regex, Rc::new(constr))
    }

    pub fn register_token_with_regex_str<C: 'static + Fn(&str) -> Atom>(&mut self, regex: &str, constr: C) {
        let regex = Regex::new(regex).unwrap();
        self.register_token(regex, constr)
    }

    pub fn register_function<T: 'static + GroundedFunction>(&mut self, func: GroundedFunctionAtom<T>) {
        let regex = Regex::new(func.name()).unwrap();
        let constr = move |_token: &str| -> Atom { Atom::gnd(func.clone()) };
        self.register_token(regex, constr)
    }

    /// Moves all tokenizer entries from `from` into `self`, leaving `from` empty
    ///
    /// NOTE: Tokens are tried in reverse order, so `move_front` actually adds entries that will be tried
    /// **last** in the priority order
    pub fn move_front(&mut self, from: &mut Tokenizer) {
        from.move_back(self);
        self.move_back(from);
    }

    /// Moves all tokenizer entries from `from` into `self`, leaving `from` empty
    ///
    /// NOTE: Tokens are tried in reverse order, so `move_back` actually adds entries that will be tried
    /// **first** in the priority order
    pub fn move_back(&mut self, from: &mut Tokenizer) {
        self.tokens.append(&mut from.tokens);
    }

    pub fn find_token(&self, token: &str) -> Option<&AtomConstr> {
        self.tokens.iter().rfind(|descr| {
            match descr.regex.find_at(token, 0) {
                Some(m) => m.start() == 0 && m.end() == token.len(),
                None => false,
            }
        }).map(|descr| &*(descr.constr))
    }

    /// Registers the regex-function pair, for a function that's already wrapped in an RC pointer
    pub(crate) fn register_token_with_func_ptr(&mut self, regex: Regex, constr: Rc<AtomConstr>) {
        self.tokens.push(TokenDescr{ regex, constr: constr })
    }

    /// Returns the constructor function associated with an exact regex string, or None if the Tokenizer
    /// does not contain the specified regex
    pub(crate) fn find_exact(&self, regex_str: &str) -> Option<Rc<AtomConstr>> {
        self.tokens.iter().rfind(|descr| {
            descr.regex.as_str() == regex_str
        }).map(|descr| descr.constr.clone())
    }

    pub fn remove_token(&mut self, regex_str: &str) {
        if let Some(pos) = self.tokens.iter().position(|descr| descr.regex.as_str() == regex_str) {
            self.tokens.remove(pos);
        }
    }

}

/// The meaning of a parsed syntactic element, generated from a substring in the input text
#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug)]
pub enum SyntaxNodeType {
    /// Comment line.  All text between a non-escaped ';' and a newline
    Comment,
    /// Variable.  A symbol immediately preceded by a '$' sigil
    VariableToken,
    /// String Literal.  All text between non-escaped '"' (double quote) characters
    StringToken,
    /// Word Token.  Any other whitespace-delimited token that isn't a [Variable](SyntaxNodeType::VariableToken),
    ///   or [StringToken](SyntaxNodeType::StringToken)
    WordToken,
    /// Open Parenthesis.  A non-escaped '(' character indicating the beginning of an expression
    OpenParen,
    /// Close Parenthesis.  A non-escaped ')' character indicating the end of an expression
    CloseParen,
    /// Whitespace. One or more whitespace chars
    Whitespace,
    /// Text that remains unparsed after a parse error has occurred
    LeftoverText,
    /// A Group of [SyntaxNode]s between an [OpenParen](SyntaxNodeType::OpenParen) and a matching
    ///   [CloseParen](SyntaxNodeType::CloseParen)
    ExpressionGroup,
    /// Syntax Nodes that cannot be combined into a coherent atom due to a parse error, even if some
    /// of the individual nodes could represent valid atoms
    ErrorGroup,
}

impl SyntaxNodeType {
    /// Returns `true` is the SyntaxNodeType is a leaf (incapable of hosting sub-nodes).  Returns `false`
    ///   for "group" node tyes.
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::ExpressionGroup |
            Self::ErrorGroup => false,
            _ => true
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug)]
pub struct SyntaxNode {
    pub node_type: SyntaxNodeType,
    pub src_range: Range<usize>,
    pub sub_nodes: Vec<SyntaxNode>,
    pub parsed_text: Option<String>,
    pub message: Option<String>,
    pub is_complete: bool,
}

impl SyntaxNode {
    fn new(node_type: SyntaxNodeType, src_range: Range<usize>, sub_nodes: Vec<SyntaxNode>) -> SyntaxNode {
        Self {
            node_type,
            src_range,
            parsed_text: None,
            sub_nodes,
            message: None,
            is_complete: true
        }
    }

    fn new_token_node(node_type: SyntaxNodeType, src_range: Range<usize>, parsed_text: String) -> SyntaxNode {
        let mut node = SyntaxNode::new(node_type, src_range, vec![]);
        node.parsed_text = Some(parsed_text);
        node
    }

    fn incomplete_with_message(node_type: SyntaxNodeType, src_range: Range<usize>, sub_nodes: Vec<SyntaxNode>, message: String) -> SyntaxNode {
        let mut node = SyntaxNode::new(node_type, src_range, sub_nodes);
        node.message = Some(message);
        node.is_complete = false;
        node
    }

    /// Creates a new error group.  Gets the error message associated with the last node
    fn new_error_group(src_range: Range<usize>, sub_nodes: Vec<SyntaxNode>) -> SyntaxNode {
        let message = sub_nodes[sub_nodes.len()-1].message.clone();
        let mut node = SyntaxNode::new(SyntaxNodeType::ErrorGroup, src_range, sub_nodes);
        node.message = message;
        node.is_complete = false;
        node
    }

    /// Transforms a root SyntaxNode into an [Atom]
    pub fn as_atom(&self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {

        //If we have an incomplete node, it's an error
        if !self.is_complete {
            return Err(self.message.clone().unwrap())
        }

        match self.node_type {
            SyntaxNodeType::Comment |
            SyntaxNodeType::Whitespace => Ok(None),
            SyntaxNodeType::OpenParen |
            SyntaxNodeType::CloseParen => Ok(None),
            SyntaxNodeType::VariableToken => {
                let token_text = self.parsed_text.as_ref().unwrap();
                let new_var_atom = Atom::var(token_text);
                Ok(Some(new_var_atom))
            },
            SyntaxNodeType::StringToken |
            SyntaxNodeType::WordToken => {
                let token_text = self.parsed_text.as_ref().unwrap();
                let constr = tokenizer.find_token(token_text);
                if let Some(constr) = constr {
                    let new_atom = constr(token_text)
                        .map_err(|e| format!("byte range = ({:?}) | {e}", self.src_range))?;
                    Ok(Some(new_atom))
                } else {
                    let new_atom = Atom::sym(token_text);
                    Ok(Some(new_atom))
                }
            },
            SyntaxNodeType::ExpressionGroup => {
                let mut err_encountered = Ok(());
                let expr_children: Vec<Atom> = self.sub_nodes.iter().filter_map(|node| {
                    match node.as_atom(tokenizer) {
                        Err(err) => {
                            err_encountered = Err(err);
                            None
                        },
                        Ok(atom) => atom
                    }
                }).collect();
                match err_encountered {
                    Ok(_) => {
                        let new_expr_atom = Atom::expr(expr_children);
                        Ok(Some(new_expr_atom))
                    },
                    Err(err) => Err(err)
                }
            },
            SyntaxNodeType::LeftoverText |
            SyntaxNodeType::ErrorGroup => {unreachable!()}
        }
    }

    /// Visits all the nodes in a parsed syntax tree in a depth-first order
    pub fn visit_depth_first<C>(&self, mut callback: C)
        where C: FnMut(&SyntaxNode)
    {
        self.visit_depth_first_internal(&mut callback);
    }

    fn visit_depth_first_internal<C>(&self, callback: &mut C)
        where C: FnMut(&SyntaxNode)
    {
        for sub_node in self.sub_nodes.iter() {
            sub_node.visit_depth_first_internal(callback);
        }
        callback(self);
    }
}

/// Implemented on a type that yields atoms to be interpreted as MeTTa code.  Typically
/// by parsing source text
pub trait Parser {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String>;
}

impl<R: Iterator<Item=io::Result<char>>> Parser for SExprParser<R> {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        self.parse(tokenizer)
    }
}

impl Parser for &mut (dyn Parser + '_) {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        (**self).next_atom(tokenizer)
    }
}

/// Proxy type to allow constructing [SExprParser] from different types
/// automatically. CharReader implements [From] for different types, while
/// [SExprParser::new] inputs `Into<CharReader<R>>` argument.
pub struct CharReader<R: Iterator<Item=io::Result<char>>>(R);

impl<R: Iterator<Item=io::Result<char>>> CharReader<R> {
    /// Construct new instance from char reading iterator
    pub fn new(chars: R) -> Self {
        Self(chars)
    }
}

impl<R: Iterator<Item=io::Result<char>>> Iterator for CharReader<R> {
    type Item = io::Result<char>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<R: Read> From<R> for CharReader<CodePoints<std::io::Bytes<R>>> {
    fn from(input: R) -> Self {
        let chars: CodePoints<_> = input.bytes().into();
        Self(chars)
    }
}

impl<R: Iterator<Item=io::Result<u8>>> From<R> for CharReader<CodePoints<R>> {
    fn from(input: R) -> Self {
        let chars: CodePoints<_> = input.into();
        Self(chars)
    }
}

impl<'a> From<&'a str> for CharReader<std::iter::Map<std::str::Chars<'a>, fn(char) -> io::Result<char>>> {
    fn from(text: &'a str) -> Self {
        Self(text.chars().map(Ok))
    }
}

impl From<String> for CharReader<std::iter::Map<std::vec::IntoIter<char>, fn(char) -> io::Result<char>>> {
    fn from(text: String) -> Self {
        let chars: Vec<char> = text.chars().collect();
        Self(chars.into_iter().map(Ok))
    }
}

impl<'a> From<&'a String> for CharReader<std::iter::Map<std::str::Chars<'a>, fn(char) -> io::Result<char>>> {
    fn from(text: &'a String) -> Self {
        Self(text.chars().map(Ok))
    }
}

/// Provides a parser for MeTTa code written in S-Expression Syntax
///
/// NOTE: The SExprParser type is short-lived, and can be created cheaply to evaluate a specific block
/// of MeTTa source code.
pub struct SExprParser<R: Iterator<Item=io::Result<char>>> {
    it: Peekable<Enumerate<CharReader<R>>>,
    last_idx: usize,
}

impl<R: Iterator<Item=io::Result<char>>> SExprParser<R> {

    pub fn new<I: Into<CharReader<R>>>(chars: I) -> Self {
        Self{ it: chars.into().enumerate().peekable(), last_idx: 0 }
    }

    pub fn parse(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        loop {
            match self.parse_to_syntax_tree()? {
                Some(node) => {
                    if let Some(atom) = node.as_atom(tokenizer)? {
                        return Ok(Some(atom))
                    }
                },
                None => {
                    return Ok(None);
                },
            }
        }
    }

    fn peek(&mut self) -> Result<Option<(usize, char)>, String> {
        match self.it.peek() {
            Some(&(idx, Ok(c))) => Ok(Some((idx, c))),
            None => Ok(None),
            Some((idx, Err(err))) => Err(format!("Input read error at position {}: {}", idx, err)),
        }
    }

    fn next(&mut self) -> Result<Option<(usize, char)>, String> {
        match self.it.next() {
            Some((idx, Ok(c))) => {
                self.last_idx = idx;
                Ok(Some((idx, c)))
            },
            None => Ok(None),
            Some((idx, Err(err))) => {
                self.last_idx = idx;
                Err(format!("Input read error at position {}: {}", idx, err))
            },
        }
    }

    fn skip_next(&mut self) {
        match self.it.next() {
            Some((idx, _)) => self.last_idx = idx,
            _ => {},
        }
    }

    pub fn parse_to_syntax_tree(&mut self) -> Result<Option<SyntaxNode>, String> {
        if let Some((idx, c)) = self.peek()? {
            match c {
                ';' => {
                    return self.parse_comment();
                },
                _ if c.is_whitespace() => {
                    let whispace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, vec![]);
                    self.skip_next();
                    return Ok(Some(whispace_node));
                },
                '$' => {
                    return self.parse_variable().map(Some);
                },
                '(' => {
                    return self.parse_expr().map(Some);
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, vec![]);
                    self.skip_next();
                    let leftover_text_node = self.parse_leftovers(idx + 1, "Unexpected right bracket".to_string())?;
                    let error_group_node = SyntaxNode::new_error_group(idx..self.cur_idx(), vec![close_paren_node, leftover_text_node]);
                    return Ok(Some(error_group_node));
                },
                _ => {
                    return self.parse_token();
                },
            };
        }
        Ok(None)
    }

    ///WARNING: may be (often is) == to text.len(), and thus can't be used as an index to read a char
    fn cur_idx(&mut self) -> usize {
        if let Some(&(idx, _)) = self.it.peek() {
            idx
        } else {
            self.last_idx + 1
        }
    }

    /// Parse to the next `\n` newline
    fn parse_comment(&mut self) -> Result<Option<SyntaxNode>, String> {
        if let Some((start_idx, _c)) = self.peek()? {
            while let Some((_idx, c)) = self.peek()? {
                match c {
                    '\n' => break,
                    _ => self.skip_next(),
                }
            }
            let range = start_idx..self.cur_idx();
            Ok(Some(SyntaxNode::new(SyntaxNodeType::Comment, range, vec![])))
        } else {
            Ok(None)
        }
    }

    fn parse_leftovers(&mut self, start_idx: usize, message: String) -> Result<SyntaxNode, String> {
        while let Some(_) = self.next()? {}
        let range = start_idx..self.cur_idx();
        Ok(SyntaxNode::incomplete_with_message(SyntaxNodeType::LeftoverText, range, vec![], message))
    }

    fn parse_expr(&mut self) -> Result<SyntaxNode, String> {
        let start_idx = self.cur_idx();
        let mut child_nodes: Vec<SyntaxNode> = Vec::new();

        let open_paren_node = SyntaxNode::new(SyntaxNodeType::OpenParen, start_idx..start_idx+1, vec![]);
        child_nodes.push(open_paren_node);
        self.skip_next();

        while let Some((idx, c)) = self.peek()? {
            match c {
                ';' => {
                    let comment_node = self.parse_comment()?.unwrap();
                    child_nodes.push(comment_node);
                },
                _ if c.is_whitespace() => {
                    let whitespace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, vec![]);
                    child_nodes.push(whitespace_node);
                    self.skip_next();
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, vec![]);
                    child_nodes.push(close_paren_node);
                    self.skip_next();

                    let expr_node = SyntaxNode::new(SyntaxNodeType::ExpressionGroup, start_idx..self.cur_idx(), child_nodes);
                    return Ok(expr_node);
                },
                _ => {
                    if let Some(parsed_node) = self.parse_to_syntax_tree()? {
                        let is_err = !parsed_node.is_complete;
                        child_nodes.push(parsed_node);

                        //If we hit an error parsing a child, then bubble it up
                        if is_err {
                            let error_group_node = SyntaxNode::new_error_group(start_idx..self.cur_idx(), child_nodes);
                            return Ok(error_group_node);
                        }
                    } else {
                        let leftover_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::ErrorGroup, start_idx..self.cur_idx(), child_nodes, "Unexpected end of expression member".to_string());
                        return Ok(leftover_node);
                    }
                },
            }
        }
        let leftover_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::ErrorGroup, start_idx..self.cur_idx(), child_nodes, "Unexpected end of expression".to_string());
        Ok(leftover_node)
    }

    fn parse_token(&mut self) -> Result<Option<SyntaxNode>, String> {
        match self.peek()? {
            Some((_idx, '"')) => {
                let string_node = self.parse_string()?;
                Ok(Some(string_node))
            },
            Some((_idx, _)) => {
                let word_node = self.parse_word()?;
                Ok(Some(word_node))
            },
            None => Ok(None)
        }
    }

    fn parse_string(&mut self) -> Result<SyntaxNode, String> {
        let mut token = String::new();
        let start_idx = self.cur_idx();

        if let Some((_idx, '"')) = self.next()? {
            token.push('"');
        } else {
            let leftover_text_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::LeftoverText, start_idx..self.cur_idx(), vec![], "Double quote expected".to_string());
            return Ok(leftover_text_node);
        }
        while let Some((char_idx, c)) = self.next()? {
            if c == '"' {
                token.push('"');
                let string_node = SyntaxNode::new_token_node(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), token);
                return Ok(string_node);
            }
            if c == '\\' {
                let escape_err = |cur_idx| { SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, char_idx..cur_idx, vec![], "Invalid escape sequence".to_string()) };

                match self.next()? {
                    Some((_idx, c)) => {
                        let val = match c {
                            '\'' | '\"' | '\\' => c, //single quote, double quote, & backslash
                            'n' => '\n', // newline
                            'r' => '\r', // carriage return
                            't' => '\t', // tab
                            'x' => { // hex sequence
                                match self.parse_2_digit_radix_char(16)? {
                                    Some(c) => c,
                                    None => {return Ok(escape_err(self.cur_idx())); }
                                }
                            },
                            'u' => { // unicode sequence
                                match self.parse_unicode_sequence()? {
                                    Some(c) => c,
                                    None => { return Ok(escape_err(self.cur_idx()));
                                }
                            }}
                            _ => {
                                return Ok(escape_err(self.cur_idx()));
                            }
                        };
                        token.push(val);
                    },
                    None => {
                        let leftover_text_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Escaping sequence is not finished".to_string());
                        return Ok(leftover_text_node);
                    },
                }
            } else {
                token.push(c);
            }
        }
        let unclosed_string_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Unclosed String Literal".to_string());
        Ok(unclosed_string_node)
    }

    /// Parses a 2-digit value from the parser at the current location
    fn parse_2_digit_radix_char(&mut self, radix: u32) -> Result<Option<char>, String> {
        let high = self.next()?.and_then(|(_, c)| c.to_digit(radix));
        let high = match high {
            Some(high) => high << 4,
            None => return Ok(None),
        };
        let low = self.next()?.and_then(|(_, c)| c.to_digit(radix));
        let low = match low {
            Some(low) => low,
            None => return Ok(None),
        };
        let hex = high | low;
        if hex <= 0x7F {
            Ok(char::from_u32(hex))
        } else {
            Ok(None)
        }
    }

    fn parse_unicode_sequence(&mut self) -> Result<Option<char>, String> {
        // unicode sequence presumably looks like this '\u{0123}'
        let mut result_u32: u32 = 0;
        let mut cur_idx = 0;
        match self.next()? {
            Some((_, '{')) => (),
            _ => return Ok(None),
        };
        loop
        {
            let next_char = match self.next()? {
                Some(char_val) => char_val.1,
                None => return Ok(None),
            };
            if next_char == '{' {continue}
            if next_char == '}' {break}
            cur_idx += 1;
            if cur_idx > 8 { return Ok(None) }
            let char_to_digit = match next_char.to_digit(16) {
                Some(digit) => digit,
                None => return Ok(None),
            };
            result_u32 = (result_u32 << 4) | char_to_digit;
        }
        Ok(char::from_u32(result_u32))
    }

    fn parse_word(&mut self) -> Result<SyntaxNode, String> {
        let mut token = String::new();
        let start_idx = self.cur_idx();

        while let Some((_idx, c)) = self.peek()? {
            if c.is_whitespace() || c == '(' || c == ')' || c == ';' {
                break;
            }
            token.push(c);
            self.skip_next();
        }

        let word_node = SyntaxNode::new_token_node(SyntaxNodeType::WordToken, start_idx..self.cur_idx(), token);
        Ok(word_node)
    }

    fn parse_variable(&mut self) -> Result<SyntaxNode, String> {
        let (start_idx, _c) = self.peek()?.unwrap();
        self.skip_next();

        let mut token = String::new();
        while let Some((_idx, c)) = self.peek()? {
            if c.is_whitespace() || c == '(' || c == ')' {
                break;
            }
            if c == '#' {
                let leftover_node = self.parse_leftovers(start_idx, "'#' char is reserved for internal usage".to_string());
                return leftover_node;
            }
            token.push(c);
            self.skip_next();
        }
        let var_token_node = SyntaxNode::new_token_node(SyntaxNodeType::VariableToken, start_idx..self.cur_idx(), token);
        Ok(var_token_node)
    }

}

impl Parser for &[Atom] {
    fn next_atom(&mut self, _tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        if let Some((atom, rest)) = self.split_first() {
            *self = rest;
            Ok(Some(atom.clone()))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
pub(crate) fn metta_atom(atom_str: &str) -> Atom {
    let mut parser = SExprParser::new(atom_str);
    let atom = parser.parse(&Tokenizer::new()).unwrap().expect("Single atom is expected");
    atom
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_var() {
        assert_eq!(vec![expr!(n)], parse_atoms("$n"));
    }

    #[test]
    fn test_text_sym() {
        assert_eq!(vec![expr!("test")], parse_atoms("test"));
    }

    #[test]
    fn test_text_quoted_string() {
        assert_eq!(vec![expr!("\"te st\"")], parse_atoms("\"te st\""));
    }

    #[test]
    fn test_text_escape_chars() {
        // Tab
        assert_eq!(vec![expr!("\"test\ttab\"")], parse_atoms(r#""test\ttab""#));
        // Newline
        assert_eq!(vec![expr!("\"test\nnewline\"")], parse_atoms(r#""test\nnewline""#));
        // ANSI Sequence
        assert_eq!(vec![expr!("\"\x1b[1;32m> \x1b[0m\"")], parse_atoms(r#""\x1b[1;32m> \x1b[0m""#));
        // Escaping a quote
        assert_eq!(vec![expr!("\"test\"quote\"")], parse_atoms(r#""test\"quote""#));
        // Two-digit hex code
        assert_eq!(vec![expr!("\"test\x7Fmax\"")], parse_atoms(r#""test\x7fmax""#));
        // Parse failure, code out of range
        assert!(parse_atoms(r#""test\xFF""#).len() == 0);
    }

    #[test]
    fn test_text_recognize_full_token() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.register_token(Regex::new(r"b").unwrap(),
            |_| Atom::value("b"));

        let mut parser = SExprParser::new("ab");

        assert_eq!(Ok(Some(expr!("ab"))), parser.parse(&tokenizer));
        assert_eq!(Ok(None), parser.parse(&tokenizer));
    }

    #[test]
    fn test_text_gnd() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.register_token(Regex::new(r"\d+").unwrap(),
            |token| Atom::value(token.parse::<i32>().unwrap()));

        let mut parser = SExprParser::new("(3d 42)");

        assert_eq!(Ok(Some(expr!("3d" {42}))), parser.parse(&tokenizer));
        assert_eq!(Ok(None), parser.parse(&tokenizer));
    }

    #[test]
    fn test_text_expr() {
        assert_eq!(vec![expr!("=" ("fac" n) ("*" n ("fac" ("-" n "1"))))],
            parse_atoms("(= (fac $n) (* $n (fac (- $n 1))))"));
    }

    #[test]
    fn test_text_few_expr() {
        assert_eq!(vec![expr!(("a")), expr!(("b"))],
            parse_atoms("(a) (b)"));
    }

    #[test]
    fn test_next_token() {
        let text = "n)";
        let mut parser = SExprParser::new(text);

        let node = parser.parse_token().unwrap().unwrap();
        assert_eq!("n".to_string(), text[node.src_range]);
        assert_eq!(Ok(Some((1, ')'))), parser.next());

        let text = "n;Some comments.";
        let mut parser = SExprParser::new(text);

        let node = parser.parse_token().unwrap().unwrap();
        assert_eq!("n".to_string(), text[node.src_range]);
        assert_eq!(Ok(Some((1, ';'))), parser.next());
    }

    #[test]
    fn test_next_string_errors() {
        let mut parser = SExprParser::new("a");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Double quote expected", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Escaping sequence is not finished", node.message.unwrap());
    }

    #[test]
    fn test_non_ascii() {
        let text = "ж)";
        let mut parser = SExprParser::new(text);
        let node = parser.parse_token().unwrap().unwrap();
        assert_eq!("ж".to_string(), text[node.src_range]);
        assert_eq!(Ok(Some((2, ')'))), parser.next());

        let text = "⟮;Some comments.";
        let mut parser = SExprParser::new(text);

        let node = parser.parse_token().unwrap().unwrap();
        assert_eq!("⟮".to_string(), text[node.src_range]);
        assert_eq!(Ok(Some((3, ';'))), parser.next());
    }

    #[test]
    fn test_parse_unicode() {
        let mut parser = SExprParser::new("\"\\u{0123}\"");
        assert_eq!(Ok(SyntaxNode::new_token_node(SyntaxNodeType::StringToken, 0..10, "\"\u{0123}\"".into())), parser.parse_string());

        let mut parser = SExprParser::new("\"\\u{0123\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\u0123}\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\u0123\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\u0{123}\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\u{defg}\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\u{130AC}\"");
        assert_eq!(Ok(SyntaxNode::new_token_node(SyntaxNodeType::StringToken, 0..11, "\"\u{130AC}\"".into())), parser.parse_string());

        let mut parser = SExprParser::new("\"\\u{130ac}\"");
        assert_eq!(Ok(SyntaxNode::new_token_node(SyntaxNodeType::StringToken, 0..11, "\"\u{130ac}\"".into())), parser.parse_string());

        let mut parser = SExprParser::new("\"\\u{012345678\"");
        let node = parser.parse_string().unwrap();
        assert!(!node.is_complete);
        assert_eq!("Invalid escape sequence", node.message.unwrap());

    }

    #[test]
    fn test_unbalanced_brackets() {
        let mut parser = SExprParser::new("(a)))");
        let _ = parser.parse(&Tokenizer::new());
        assert_eq!(Err(String::from("Unexpected right bracket")), parser.parse(&Tokenizer::new()));
    }

    #[test]
    fn test_error_from_tokenizer() {
        //NOTE: This test relies on an intentional bug in the regex, so that it will accept an invalid
        // float.  However it could be hit in legitimate cases, such as an integer that overflows the
        // type's capacity before we implement bigint, or any type where the representation's actual
        // contours can't be captured by a regex.
        let mut tokenizer = Tokenizer::new();
        tokenizer.register_fallible_token(Regex::new(r"[\-\+]?\d+.\d+").unwrap(),
            |token| Ok(Atom::gnd(crate::metta::runner::number::Number::from_float_str(token)?))
        );
        let mut parser = SExprParser::new("12345678901234567:8901234567890");
        assert!(parser.parse(&tokenizer).is_err());
    }

    #[test]
    fn test_comment_base() {
        let program = ";(a 4)
                  (b 5)";
        let expected = vec![expr!("b" "5")];
        let res = parse_atoms(program);
        assert_eq!(res, expected);
    }

    #[test]
    fn test_comment_in_sexpr() {
        let program = " (a ; 4)
                  5)";
        let expected = vec![expr!("a" "5")];
        let res = parse_atoms(program);
        assert_eq!(res, expected);
    }

    #[test]
    fn test_comment_in_sexpr_before_closing_bracket() {
        let program = " (a 5 ; 4)
                  )";
        let expected = vec![expr!("a" "5")];
        let res = parse_atoms(program);
        assert_eq!(res, expected);
    }

    #[test]
    fn test_comment_endl() {
        let program = " (a 4);
                  (b 5)";
        let expected = vec![expr!("a" "4"), expr!("b" "5")];
        let res = parse_atoms(program);
        assert_eq!(res, expected);
    }

    fn parse_atoms(program: &str) -> Vec<Atom> {
        let tokenizer = Tokenizer::new();
        let mut parser = SExprParser::new(program);
        let mut result = Vec::new();
        while let Ok(Some(atom)) = parser.parse(&tokenizer) {
            result.push(atom);
        }
        result
    }

    #[test]
    fn test_lattice_in_var_name() {
        let mut parser = SExprParser::new("$a#");
        assert_eq!(Err(String::from("'#' char is reserved for internal usage")), parser.parse(&Tokenizer::new()));
    }

    #[test]
    fn override_token_definition() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.register_token(Regex::new(r"A").unwrap(), |_| Atom::sym("A"));
        assert_eq!(tokenizer.find_token("A").unwrap()("A").unwrap(), Atom::sym("A"));
        tokenizer.register_token(Regex::new(r"A").unwrap(), |_| Atom::sym("B"));
        assert_eq!(tokenizer.find_token("A").unwrap()("A").unwrap(), Atom::sym("B"));
    }

    #[test]
    fn test_owned_sexprparser() {
        let tokenizer = Tokenizer::new();
        let mut parser;
        {
            let owned = r#"One (two 3) "four""#.to_owned();
            parser = SExprParser::new(owned);
        }
        let mut results: Vec<Atom> = vec![];
        while let Ok(Some(atom)) = parser.next_atom(&tokenizer) {
            results.push(atom);
        }
        let expected = vec![sym!("One"), expr!("two" "3"), sym!(r#""four""#)];
        assert_eq!(results, expected);
    }

    #[test]
    fn test_comment_in_symbol() {
        let program = "(a; 4)
                  5)";
        let expected = vec![expr!("a" "5")];
        let res = parse_atoms(program);
        assert_eq!(res, expected);
    }

}
