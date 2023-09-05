//! MeTTa parser implementation.

use core::convert::TryFrom;

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

type AtomConstr = dyn Fn(&str) -> Atom;

impl Tokenizer {

    pub fn new() -> Self {
        Self{ tokens: Vec::new() }
    }

    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.tokens.push(TokenDescr{ regex, constr: Rc::new(constr) })
    }

    pub fn register_token_with_regex_str<C: 'static + Fn(&str) -> Atom>(&mut self, regex: &str, constr: C) {
        let regex = Regex::new(regex).unwrap();
        self.register_token(regex, constr)
    }

    pub fn move_front(&mut self, from: &mut Tokenizer) {
        from.move_back(self);
        self.move_back(from);
    }

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

}

/// The meaning of a parsed syntactic element, generated from a substring in the input text
#[derive(Clone, Debug)]
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
    /// Symbol Atom.  A Symbol Atom
    SymbolAtom,
    /// Variable Atom.  A [VariableAtom] constructed from a [VariableToken](SyntaxNodeType::VariableToken)
    VariableAtom,
    /// Expression.  All input text composing an Expression, from the opening '(' to the close
    ExpressionAtom,
    /// Special Atom.  A token matched by a regex registered with the [Tokenizer].  Might be a Grounded
    ///   Atom, but might also be another type of Atom
    SpecialAtom,
    /// Unparsed Leftover Text.  Unparsed text remaining after the parser has encountered an error
    LeftoverText,
    /// Syntax Nodes that cannot be combined into a coherent atom due to a parse error, even if some
    /// of the individual nodes are valid
    ErrorGroup,
}

#[derive(Clone, Debug)]
pub struct SyntaxNode {
    pub node_type: SyntaxNodeType,
    pub src_range: Range<usize>,
    pub atom: Option<Atom>,
    pub sub_nodes: Vec<SyntaxNode>,
    pub message: Option<String>,
    pub is_complete: bool,
}

impl SyntaxNode {
    fn new(node_type: SyntaxNodeType, src_range: Range<usize>, atom: Option<Atom>, sub_nodes: Vec<SyntaxNode>) -> SyntaxNode {
        Self {
            node_type,
            src_range,
            atom,
            sub_nodes,
            message: None,
            is_complete: true
        }
    }

    fn incomplete_with_message(node_type: SyntaxNodeType, src_range: Range<usize>, sub_nodes: Vec<SyntaxNode>, message: String) -> SyntaxNode {
        Self {
            node_type,
            src_range,
            atom: None,
            sub_nodes,
            message: Some(message),
            is_complete: false
        }
    }

    /// Creates a new error group.  Gets the error message associated with the last node
    fn new_error_group(src_range: Range<usize>, sub_nodes: Vec<SyntaxNode>) -> SyntaxNode {
        let message = sub_nodes[sub_nodes.len()-1].message.clone();
        Self {
            node_type: SyntaxNodeType::ErrorGroup,
            src_range,
            atom: None,
            sub_nodes,
            message,
            is_complete: false
        }
    }

    /// Visits all the syntactic nodes (vs. semantic) in a parsed syntax tree
    ///
    /// This method is useful to render syntax styling.
    ///
    /// TODO: In the future, We'll want to be able to use the type system to assign styling,
    ///   which is going to mean looking at Atoms, and not the tokens they were built from
    pub fn visit_syntactic<C>(&self, mut callback: C)
        where C: FnMut(&SyntaxNode)
    {
        self.visit_depth_first(|node| {
            match node.node_type {
                SyntaxNodeType::Comment |
                SyntaxNodeType::VariableToken |
                SyntaxNodeType::StringToken |
                SyntaxNodeType::WordToken |
                SyntaxNodeType::OpenParen |
                SyntaxNodeType::CloseParen |
                SyntaxNodeType::Whitespace |
                SyntaxNodeType::LeftoverText => {
                    callback(node);
                }
                _ => {}
            }
        })
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
            match self.parse_to_syntax_tree(tokenizer) {
                Some(node) => {
                    //If we have an incomplete node, it's an error
                    if !node.is_complete {
                        return Err(node.message.unwrap())
                    }

                    //We are only interested in nodes that represent atoms
                    match node.node_type {
                        SyntaxNodeType::SymbolAtom |
                        SyntaxNodeType::VariableAtom |
                        SyntaxNodeType::ExpressionAtom |
                        SyntaxNodeType::SpecialAtom => {
                            return Ok(node.atom)
                        },
                        _ => ()
                    }
                },
                None => {
                    return Ok(None);
                },
            }
        }
    }

    pub fn parse_to_syntax_tree(&mut self, tokenizer: &Tokenizer) -> Option<SyntaxNode> {
        if let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let comment_node = self.parse_comment().unwrap();
                    return Some(comment_node);
                },
                _ if c.is_whitespace() => {
                    let whispace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, None, vec![]);
                    self.it.next();
                    return Some(whispace_node);
                },
                '$' => {
                    let var_node = self.parse_variable();
                    return Some(var_node);
                },
                '(' => {
                    let expr_node = self.parse_expr(tokenizer);
                    return Some(expr_node);
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, None, vec![]);
                    self.it.next();
                    let leftover_text_node = self.parse_leftovers("Unexpected right bracket".to_string());
                    let error_group_node = SyntaxNode::new_error_group(idx..self.cur_idx(), vec![close_paren_node, leftover_text_node]);
                    return Some(error_group_node);
                },
                _ => {
                    let atom_node = self.parse_atom(tokenizer);
                    return atom_node;
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
            Some(SyntaxNode::new(SyntaxNodeType::Comment, range, None, vec![]))
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

    fn parse_atom(&mut self, tokenizer: &Tokenizer) -> Option<SyntaxNode> {
        if let Some(token_node) = self.parse_token() {
            if token_node.is_complete {
                let token_text = <&SymbolAtom>::try_from(token_node.atom.as_ref().unwrap()).unwrap().name();
                let constr = tokenizer.find_token(token_text);
                if let Some(constr) = constr {
                    let new_atom = constr(token_text);
                    let special_atom_node = SyntaxNode::new(SyntaxNodeType::SpecialAtom, token_node.src_range.clone(), Some(new_atom), vec![token_node]);
                    return Some(special_atom_node);
                } else {
                    let symbol_atom_node = SyntaxNode::new(SyntaxNodeType::SymbolAtom, token_node.src_range.clone(), token_node.atom.clone(), vec![token_node]);
                    return Some(symbol_atom_node);
                }
            } else {
                Some(token_node)
            }
        } else {
            None
        }
    }

    fn parse_expr(&mut self, tokenizer: &Tokenizer) -> SyntaxNode {
        let start_idx = self.cur_idx();
        let mut child_nodes: Vec<SyntaxNode> = Vec::new();

        let open_paren_node = SyntaxNode::new(SyntaxNodeType::OpenParen, start_idx..start_idx+1, None, vec![]);
        child_nodes.push(open_paren_node);
        self.it.next();

        while let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let comment_node = self.parse_comment().unwrap();
                    child_nodes.push(comment_node);
                },
                _ if c.is_whitespace() => {
                    let whitespace_node = SyntaxNode::new(SyntaxNodeType::Whitespace, idx..idx+1, None, vec![]);
                    child_nodes.push(whitespace_node);
                    self.it.next();
                },
                ')' => {
                    let close_paren_node = SyntaxNode::new(SyntaxNodeType::CloseParen, idx..idx+1, None, vec![]);
                    child_nodes.push(close_paren_node);
                    self.it.next();

                    let expr_children: Vec<Atom> = child_nodes.iter().filter_map(|node| node.atom.clone()).collect();
                    let new_expr_atom = Atom::expr(expr_children);
                    let expr_node = SyntaxNode::new(SyntaxNodeType::ExpressionAtom, start_idx..self.cur_idx(), Some(new_expr_atom), child_nodes);
                    return expr_node;
                },
                _ => {
                    if let Some(parsed_node) = self.parse_to_syntax_tree(tokenizer) {
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
        while let Some((_idx, c)) = self.it.next() {
            if c == '"' {
                token.push('"');
                let string_symbol_atom = Atom::sym(token);
                let string_node = SyntaxNode::new(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), Some(string_symbol_atom), vec![]);
                return string_node;
            }
            let c = if c == '\\' {
                match self.it.next() {
                    Some((_idx, c)) => c,
                    None => {
                        let leftover_text_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Escaping sequence is not finished".to_string());
                        return leftover_text_node;
                    },
                }
            } else {
                c
            };
            token.push(c);
        }
        let unclosed_string_node = SyntaxNode::incomplete_with_message(SyntaxNodeType::StringToken, start_idx..self.cur_idx(), vec![], "Unclosed String Literal".to_string());
        unclosed_string_node
    }

    fn parse_word(&mut self) -> SyntaxNode {
        let mut token = String::new();
        let start_idx = self.cur_idx();

        while let Some((_idx, c)) = self.it.peek() {
            if c.is_whitespace() || *c == '(' || *c == ')' {
                break;
            }
            token.push(*c);
            self.it.next();
        }

        let word_symbol_atom = Atom::sym(token);
        let word_node = SyntaxNode::new(SyntaxNodeType::WordToken, start_idx..self.cur_idx(), Some(word_symbol_atom), vec![]);
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
        let range = start_idx..self.cur_idx();
        let var_token_node = SyntaxNode::new(SyntaxNodeType::VariableToken, range.clone(), None, vec![]);
        let new_var_atom = Atom::var(token);
        let variable_atom_node = SyntaxNode::new(SyntaxNodeType::VariableAtom, range, Some(new_var_atom), vec![var_token_node]);
        variable_atom_node
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
    fn test_unbalanced_brackets() {
        let mut parser = SExprParser::new("(a)))");
        let _ = parser.parse(&Tokenizer::new());
        assert_eq!(Err(String::from("Unexpected right bracket")), parser.parse(&Tokenizer::new()));
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
        assert_eq!(tokenizer.find_token("A").unwrap()("A"), Atom::sym("A"));
        tokenizer.register_token(Regex::new(r"A").unwrap(), |_| Atom::sym("B"));
        assert_eq!(tokenizer.find_token("A").unwrap()("A"), Atom::sym("B"));
    }
}
