//! MeTTa parser implementation.

use crate::*;

use core::ops::Range;
use std::str::CharIndices;
use std::iter::Peekable;
use regex::Regex;
use std::rc::Rc;

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

}

/// The meaning of a parsed syntactic element, generated from a substring in the input text
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

impl Parser for SExprParser<'_> {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        self.parse(tokenizer)
    }
}

impl Parser for &mut (dyn Parser + '_) {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        (**self).next_atom(tokenizer)
    }
}

/// Provides a parser for MeTTa code written in S-Expression Syntax
///
/// NOTE: The SExprParser type is short-lived, and can be created cheaply to evaluate a specific block
/// of MeTTa source code.
#[derive(Clone)]
pub struct SExprParser<'a> {
    text: &'a str,
    it: Peekable<CharIndices<'a>>,
}

impl<'a> SExprParser<'a> {
    pub fn new(text: &'a str) -> Self {
        Self{ text, it: text.char_indices().peekable() }
    }

    pub fn parse(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        loop {
            match self.parse_to_syntax_tree() {
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

    pub fn parse_to_syntax_tree(&mut self) -> Option<SyntaxNode> {
        if let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let comment_node = self.parse_comment().unwrap();
                    return Some(comment_node);
                },
                _ if c.is_whitespace() => {
                    let whispace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, vec![]);
                    self.it.next();
                    return Some(whispace_node);
                },
                '$' => {
                    let var_node = self.parse_variable();
                    return Some(var_node);
                },
                '(' => {
                    let expr_node = self.parse_expr();
                    return Some(expr_node);
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, vec![]);
                    self.it.next();
                    let leftover_text_node = self.parse_leftovers("Unexpected right bracket".to_string());
                    let error_group_node = SyntaxNode::new_error_group(idx..self.cur_idx(), vec![close_paren_node, leftover_text_node]);
                    return Some(error_group_node);
                },
                _ => {
                    let token_node = self.parse_token();
                    return token_node;
                },
            }
        }
        None
    }

    ///WARNING: may be (often is) == to text.len(), and thus can't be used as an index to read a char
    fn cur_idx(&mut self) -> usize {
        if let Some((idx, _)) = self.it.peek() {
            *idx
        } else {
            self.text.len()
        }
    }

    /// Parse to the next `\n` newline
    fn parse_comment(&mut self) -> Option<SyntaxNode> {
        if let Some((start_idx, _c)) = self.it.peek().cloned() {
            while let Some((_idx, c)) = self.it.peek() {
                match c {
                    '\n' => break,
                    _ => { self.it.next(); }
                }
            }
            let range = start_idx..self.cur_idx();
            Some(SyntaxNode::new(SyntaxNodeType::Comment, range, vec![]))
        } else {
            None
        }
    }

    fn parse_leftovers(&mut self, message: String) -> SyntaxNode {
        let start_idx = self.cur_idx();
        while let Some(_) = self.it.next() {}
        let range = start_idx..self.cur_idx();
        SyntaxNode::incomplete_with_message(SyntaxNodeType::LeftoverText, range, vec![], message)
    }

    fn parse_expr(&mut self) -> SyntaxNode {
        let start_idx = self.cur_idx();
        let mut child_nodes: Vec<SyntaxNode> = Vec::new();

        let open_paren_node = SyntaxNode::new(SyntaxNodeType::OpenParen, start_idx..start_idx+1, vec![]);
        child_nodes.push(open_paren_node);
        self.it.next();

        while let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let comment_node = self.parse_comment().unwrap();
                    child_nodes.push(comment_node);
                },
                _ if c.is_whitespace() => {
                    let whitespace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, vec![]);
                    child_nodes.push(whitespace_node);
                    self.it.next();
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, vec![]);
                    child_nodes.push(close_paren_node);
                    self.it.next();

                    let expr_node = SyntaxNode::new(SyntaxNodeType::ExpressionGroup, start_idx..self.cur_idx(), child_nodes);
                    return expr_node;
                },
                _ => {
                    if let Some(parsed_node) = self.parse_to_syntax_tree() {
                        let is_err = !parsed_node.is_complete;
                        child_nodes.push(parsed_node);

                        //If we hit an error parsing a child, then bubble it up
                        if is_err {
                            let error_group_node = SyntaxNode::new_error_group(start_idx..self.cur_idx(), child_nodes);
                            return error_group_node;
                        }
                    } else {
                        let leftover_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::ErrorGroup, start_idx..self.cur_idx(), child_nodes, "Unexpected end of expression member".to_string());
                        return leftover_node;
                    }
                },
            }
        }
        let leftover_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::ErrorGroup, start_idx..self.cur_idx(), child_nodes, "Unexpected end of expression".to_string());
        leftover_node
    }

    fn parse_token(&mut self) -> Option<SyntaxNode> {
        match self.it.peek().cloned() {
            Some((_idx, '"')) => {
                let string_node = self.parse_string();
                Some(string_node)
            },
            Some((_idx, _)) => {
                let word_node = self.parse_word();
                Some(word_node)
            },
            None => None
        }
    }

    fn parse_string(&mut self) -> SyntaxNode {
        let mut token = String::new();
        let start_idx = self.cur_idx();

        if let Some((_idx, '"')) = self.it.next() {
            token.push('"');
        } else {
            let leftover_text_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::LeftoverText, start_idx..self.cur_idx(), vec![], "Double quote expected".to_string());
            return leftover_text_node;
        }
        while let Some((char_idx, c)) = self.it.next() {
            if c == '"' {
                token.push('"');
                let string_node = SyntaxNode::new_token_node(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), token);
                return string_node;
            }
            if c == '\\' {
                let escape_err = |cur_idx| { SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, char_idx..cur_idx, vec![], "Invalid escape sequence".to_string()) };
                match self.it.next() {
                    Some((_idx, c)) => {
                        let val = match c {
                            '\'' | '\"' | '\\' => c, //single quote, double quote, & backslash
                            'n' => '\n', // newline
                            'r' => '\r', // carriage return
                            't' => '\t', // tab
                            'x' => { // hex sequence
                                match self.parse_2_digit_radix_value(16) {
                                    Some(code_val) => code_val.into(),
                                    None => {return escape_err(self.cur_idx()); }
                                }
                            },
                            'u' => { // unicode sequence
                                match self.parse_unicode_sequence() {
                                    Some(char_val) => char_val.into(),
                                    None => { return escape_err(self.cur_idx());
                                }
                            }}
                            _ => {
                                return escape_err(self.cur_idx());
                            }
                        };
                        token.push(val);
                    },
                    None => {
                        let leftover_text_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Escaping sequence is not finished".to_string());
                        return leftover_text_node;
                    },
                }
            } else {
                token.push(c);
            }
        }
        let unclosed_string_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Unclosed String Literal".to_string());
        unclosed_string_node
    }

    /// Parses a 2-digit value from the parser at the current location
    fn parse_2_digit_radix_value(&mut self, radix: u32) -> Option<u8> {
        self.it.next()
        .and_then(|(_, digit1)| digit1.is_digit(radix).then(|| digit1))
        .and_then(|digit1| TryInto::<u8>::try_into(digit1).ok())
        .and_then(|byte1| self.it.next().map(|(_, digit2)| (byte1, digit2)))
        .and_then(|(byte1, digit2)| digit2.is_digit(radix).then(|| (byte1, digit2)))
        .and_then(|(byte1, digit2)| TryInto::<u8>::try_into(digit2).ok().map(|byte2| (byte1, byte2)))
        .and_then(|(byte1, byte2)| {
            let digits_buf = &[byte1, byte2];
            u8::from_str_radix(core::str::from_utf8(digits_buf).unwrap(), radix).ok()
        }).and_then(|code_val| (code_val <= 0x7F).then(|| code_val))
    }

    fn parse_unicode_sequence(&mut self) -> Option<char> {
        // unicode sequence presumably looks like this '\\u{0123}'
        let mut char_vec = vec!['\\', 'u'];
        loop
        {
            let next_char = match self.it.next() {
                Some(char_val) => char_val.1,
                None => return None,
            };
            char_vec.push(next_char);
            if next_char == '}' {break}
        }
        let resstr = unescaper::unescape(&String::from_iter(char_vec.clone())).unwrap();
        let res_vec: Vec<char> = resstr.chars().collect();
        Some(res_vec[0])
    }

    fn parse_word(&mut self) -> SyntaxNode {
        let mut token = String::new();
        let start_idx = self.cur_idx();

        while let Some((_idx, c)) = self.it.peek() {
            if c.is_whitespace() || *c == '(' || *c == ')' || *c == ';' {
                break;
            }
            token.push(*c);
            self.it.next();
        }

        let word_node = SyntaxNode::new_token_node(SyntaxNodeType::WordToken, start_idx..self.cur_idx(), token);
        word_node
    }

    fn parse_variable(&mut self) -> SyntaxNode {
        let (start_idx, _c) = self.it.peek().cloned().unwrap();
        let mut tmp_it = self.it.clone();
        tmp_it.next();

        let mut token = String::new();
        while let Some((_idx, c)) = tmp_it.peek() {
            if c.is_whitespace() || *c == '(' || *c == ')' {
                break;
            }
            if *c == '#' {
                let leftover_node = self.parse_leftovers("'#' char is reserved for internal usage".to_string());
                return leftover_node;
            }
            token.push(*c);
            tmp_it.next();
        }
        self.it = tmp_it;
        let var_token_node = SyntaxNode::new_token_node(SyntaxNodeType::VariableToken, start_idx..self.cur_idx(), token);
        var_token_node
    }

}

/// An version of [SExprParser] that owns its input text buffer so it has a `'static` lifetime
#[derive(Clone)]
pub struct OwnedSExprParser {
    text: String,
    last_pos: usize,
}

impl OwnedSExprParser {
    pub fn new(text: String) -> Self {
        Self{text, last_pos: 0}
    }
}

impl Parser for OwnedSExprParser {
    fn next_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        if self.last_pos >= self.text.len() {
            return Ok(None);
        }
        let slice = &self.text[self.last_pos..self.text.len()];
        let mut parser = SExprParser::new(slice);
        let result = parser.parse(tokenizer);
        self.last_pos = self.last_pos + parser.cur_idx();
        result
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

        let node = parser.parse_token().unwrap();
        assert_eq!("n".to_string(), text[node.src_range]);
        assert_eq!(Some((1, ')')), parser.it.next());

        let text = "n;Some comments.";
        let mut parser = SExprParser::new(text);

        let node = parser.parse_token().unwrap();
        assert_eq!("n".to_string(), text[node.src_range]);
        assert_eq!(Some((1, ';')), parser.it.next());
    }

    #[test]
    fn test_next_string_errors() {
        let mut parser = SExprParser::new("a");
        let node = parser.parse_string();
        assert!(!node.is_complete);
        assert_eq!("Double quote expected", node.message.unwrap());

        let mut parser = SExprParser::new("\"\\");
        let node = parser.parse_string();
        assert!(!node.is_complete);
        assert_eq!("Escaping sequence is not finished", node.message.unwrap());
    }

    #[test]
    fn test_parse_unicode() {
        let mut parser = SExprParser::new("\"\\u{0123}\"");
        let node = parser.parse_string();
        let node_atom = node.as_atom(&Tokenizer::new());
        assert_eq!(unescaper::unescape("\"\\u{0123}\"").unwrap(), node_atom.unwrap().unwrap().to_string());

        let mut parser = SExprParser::new("\"\\u{0123\"");
        let node = parser.parse_string();
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
            |token| Ok(Atom::gnd(metta::runner::number::Number::from_float_str(token)?))
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
        let mut parser = OwnedSExprParser::new(r#"One (two 3) "four""#.to_string());
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
