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
    Variable,
    /// String Literal.  All text between non-escaped '"' (double quote) characters
    StringLiteral,
    /// Special Token.  A token matched by a regex registered with the [Tokenizer]
    //TODO: Currently this `Special` token is never generated.  When I split the atom generation from the parsing, I will
    //  roll the Tokenizer check into `next_token_with_visitor`, and eliminate `parse_atom_with_visitor`
    Special,
    /// Symbol Token.  Any other whitespace-delimited token that isn't a [Variable](SyntaxNodeType::Variable),
    ///   [StringLiteral](SyntaxNodeType::StringLiteral), or [Special](SyntaxNodeType::Special)
    MiscSymbol,
    /// Open Parenthesis.  A non-escaped '(' character indicating the beginning of an expression
    OpenParen,
    /// Close Parenthesis.  A non-escaped ')' character indicating the end of an expression
    CloseParen,
    /// Whitespace. One or more whitespace chars
    Whitespace,
    /// Expression.  All input text composing an Expression, from the opening '(' to the close
    Expression,
    /// Atom.  A Symbol Atom or Grounded Atom
    //TODO, check since I'm not sure about this one.  Maybe I'll want different intermediate tokens
    // when I generate atoms from parse tokens
    Atom,
    /// Unparsed Leftover Text.  Text remaining after the parser has encountered an error
    LeftoverText,
}

#[derive(Clone, Debug)]
pub struct SyntaxNode {
    pub node_type: SyntaxNodeType,
    pub src_range: Range<usize>,
}

pub struct SExprParser<'a> {
    text: &'a str,
    it: Peekable<CharIndices<'a>>,
}

impl<'a> SExprParser<'a> {
    pub fn new(text: &'a str) -> Self {
        Self{ text, it: text.char_indices().peekable() }
    }

    //TODO: Consider reorganizing this function as a visitor
    pub fn parse(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        self.parse_with_visitor(tokenizer, |_tok| ())
    }

    pub fn parse_with_visitor<C>(&mut self, tokenizer: &Tokenizer, mut callback: C) -> Result<Option<Atom>, String>
        where C: FnMut(SyntaxNode)
    {
        self.parse_with_visitor_internal(tokenizer, &mut callback)
    }

    fn parse_with_visitor_internal<C>(&mut self, tokenizer: &Tokenizer, callback: &mut C) -> Result<Option<Atom>, String>
        where C: FnMut(SyntaxNode)
    {
        while let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let start_idx = idx;
                    self.skip_line();
                    let range = start_idx..self.cur_idx();
                    callback(self.new_syntax_node(SyntaxNodeType::Comment, range));
                },
                _ if c.is_whitespace() => {
                    let range = idx..idx+1;
                    callback(self.new_syntax_node(SyntaxNodeType::Whitespace, range));
                    self.it.next();
                },
                '$' => {
                    let token = self.next_var_with_visitor(callback)?;
                    return Ok(Some(Atom::var(token)));
                },
                '(' => {
                    let range = idx..idx+1;
                    callback(self.new_syntax_node(SyntaxNodeType::OpenParen, range));

                    self.it.next();
                    let start_idx = idx;
                    let expr = self.parse_expr_with_visitor(tokenizer, callback)?;
                    let range = start_idx..self.cur_idx();
                    callback(self.new_syntax_node(SyntaxNodeType::Expression, range));
                    return Ok(Some(expr));
                },
                ')' => {
                    let range = idx..idx+1;
                    callback(self.new_syntax_node(SyntaxNodeType::CloseParen, range));
                    self.it.next();

                    self.parse_leftovers_with_visitor(callback);
                    return Err("Unexpected right bracket".to_string())
                },
                _ => {
                    let start_idx = idx;
                    let atom = self.parse_atom_with_visitor(tokenizer, callback)?;
                    let range = start_idx..self.cur_idx();
                    callback(self.new_syntax_node(SyntaxNodeType::Atom, range));
                    return Ok(Some(atom));
                },
            }
        }
        Ok(None)
    }

    ///WARNING: may be (often is) == to text.len(), and thus can't be used as an index to read a char
    fn cur_idx(&mut self) -> usize {
        if let Some((idx, _)) = self.it.peek() {
            *idx
        } else {
            self.text.len()
        }
    }

    fn new_syntax_node(&self, node_type: SyntaxNodeType, src_range: Range<usize>) -> SyntaxNode {
        SyntaxNode {
            node_type,
            src_range: src_range.clone(),
        }
    }

    fn skip_line(&mut self) -> () {
        while let Some((_idx, c)) = self.it.peek() {
            match c {
                '\n' => break,
                _ => { self.it.next(); }
            }
        }
    }

    fn parse_leftovers_with_visitor<C>(&mut self, callback: &mut C)
        where C: FnMut(SyntaxNode)
    {
        if let Some((start_idx, _c)) = self.it.peek().cloned() {
            let (last, _c) = self.it.clone().last().unwrap();
            let range = start_idx..last+1;
            callback(self.new_syntax_node(SyntaxNodeType::LeftoverText, range));
        }
    }

    fn parse_atom_with_visitor<C>(&mut self, tokenizer: &Tokenizer, callback: &mut C) -> Result<Atom, String>
        where C: FnMut(SyntaxNode)
    {
        let token = self.next_token_with_visitor(callback)?;
        let constr = tokenizer.find_token(token.as_str());
        if let Some(constr) = constr {
            return Ok(constr(token.as_str()));
        } else {
            return Ok(Atom::sym(token));
        }
    }

    fn parse_expr_with_visitor<C>(&mut self, tokenizer: &Tokenizer, callback: &mut C) -> Result<Atom, String>
        where C: FnMut(SyntaxNode)
    {
        let mut children: Vec<Atom> = Vec::new();
        while let Some((idx, c)) = self.it.peek().cloned() {
            match c {
                ';' => {
                    let start_idx = idx;
                    self.skip_line();
                    let range = start_idx..self.cur_idx();
                    callback(self.new_syntax_node(SyntaxNodeType::Comment, range));
                },
                _ if c.is_whitespace() => {
                    let range = idx..idx+1;
                    callback(self.new_syntax_node(SyntaxNodeType::Whitespace, range));
                    self.it.next();
                },
                ')' => {
                    let range = idx..idx+1;
                    callback(self.new_syntax_node(SyntaxNodeType::CloseParen, range));
                    self.it.next();
                    let expr = Atom::expr(children);
                    return Ok(expr);
                },
                _ => {
                    if let Ok(Some(child)) = self.parse_with_visitor_internal(tokenizer, callback) {
                        children.push(child);
                    } else {
                        return Err("Unexpected end of expression member".to_string());
                    }
                },
            }
        }
        Err("Unexpected end of expression".to_string())
    }

    fn next_token_with_visitor<C>(&mut self, callback: &mut C) -> Result<String, String>
        where C: FnMut(SyntaxNode)
    {
        match self.it.peek().cloned() {
            Some((idx, '"')) => {
                let start_idx = idx;
                let str_token = self.next_string()?;
                let range = start_idx..self.cur_idx();
                callback(self.new_syntax_node(SyntaxNodeType::StringLiteral, range));
                Ok(str_token)
            },
            Some((idx, _)) => {
                let start_idx = idx;
                let tok = self.next_word()?;
                let range = start_idx..self.cur_idx();
                callback(self.new_syntax_node(SyntaxNodeType::MiscSymbol, range));
                Ok(tok)
            },
            None => Ok(String::new())
        }
    }

    fn next_string(&mut self) -> Result<String, String> {
        let mut token = String::new();

        if let Some((_idx, '"')) = self.it.next() {
            token.push('"');
        } else {
            return Err("Double quote expected".to_string());
        }
        while let Some((_idx, c)) = self.it.next() {
            if c == '"' {
                token.push('"');
                break;
            }
            let c = if c == '\\' {
                match self.it.next() {
                    Some((_idx, c)) => c,
                    None => return Err("Escaping sequence is not finished".to_string()),
                }
            } else {
                c
            };
            token.push(c);
        }
        Ok(token)
    }

    fn next_word(&mut self) -> Result<String, String> {
        let mut token = String::new();
        while let Some((_idx, c)) = self.it.peek() {
            if c.is_whitespace() || *c == '(' || *c == ')' {
                break;
            }
            token.push(*c);
            self.it.next();
        }
        Ok(token) 
    }

    fn next_var_with_visitor<C>(&mut self, callback: &mut C) -> Result<String, String>
        where C: FnMut(SyntaxNode)
    {
        let (start_idx, _c) = self.it.peek().cloned().unwrap();
        let mut tmp_it = self.it.clone();
        tmp_it.next();

        let mut token = String::new();
        while let Some((_idx, c)) = tmp_it.peek() {
            if c.is_whitespace() || *c == '(' || *c == ')' {
                break;
            }
            if *c == '#' {
                self.parse_leftovers_with_visitor(callback);
                return Err("'#' char is reserved for internal usage".to_string());
            }
            token.push(*c);
            tmp_it.next();
        }
        self.it = tmp_it;
        let range = start_idx..self.cur_idx();
        callback(self.new_syntax_node(SyntaxNodeType::Variable, range));
        Ok(token)
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
        let mut parser = SExprParser::new("n)");

        assert_eq!("n".to_string(), parser.next_token_with_visitor(&mut |_tok| ()).unwrap());
        assert_eq!(Some((1, ')')), parser.it.next());
    }

    #[test]
    fn test_next_string_errors() {
        let mut parser = SExprParser::new("a");
        assert_eq!(Err(String::from("Double quote expected")), parser.next_string());

        let mut parser = SExprParser::new("\"\\");
        assert_eq!(Err(String::from("Escaping sequence is not finished")), parser.next_string());
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
