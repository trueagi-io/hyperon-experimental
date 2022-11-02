//! MeTTa parser implementation.

use crate::*;

use std::str::Chars;
use std::iter::Peekable;
use regex::Regex;
use std::rc::Rc;

#[derive(Clone)]
pub struct Tokenizer {
    tokens: Vec<TokenDescr>,
}

#[derive(Clone)]
struct TokenDescr {
    regex: Regex,
    constr: Rc<AtomConstr>,
}

type AtomConstr = dyn Fn(&str) -> Atom;

impl Tokenizer {

    pub fn new() -> Self {
        Self{ tokens: Vec::new() }
    }

    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.tokens.push(TokenDescr{ regex, constr: Rc::new(constr) });
    }

    fn find_token(&self, token: &str) -> Option<&AtomConstr> {
        self.tokens.iter().find(|descr| {
            match descr.regex.find_at(token, 0) {
                Some(m) => m.start() == 0 && m.end() == token.len(),
                None => false,
            }
        }).map(|descr| &*(descr.constr))
    }

}

pub struct SExprParser<'a> {
    it: Peekable<Chars<'a>>,
}

impl<'a> SExprParser<'a> {
    pub fn new(text: &'a str) -> Self {
        Self{ it: text.chars().peekable() }
    }

    pub fn parse(&mut self, tokenizer: &Tokenizer) -> Option<Atom> {
        while let Some(c) = self.it.peek() {
            match c {
                ';' => {
                    self.skip_line();
                },
                _ if c.is_whitespace() => {
                    self.it.next();
                },
                '$' => {
                    self.it.next();
                    let token = next_var(&mut self.it);
                    return Some(Atom::var(token));
                },
                '(' => {
                    self.it.next();
                    return Some(self.parse_expr(tokenizer))
                },
                ')' => panic!("Unexpected right bracket"),
                _ => {
                    return self.parse_atom(tokenizer);
                },
            }
        }
        None
    }

    fn skip_line(&mut self) -> () {
        while let Some(n) = self.it.peek() {
            match n {
                '\n' => break,
                _ => { self.it.next(); }
            }
        }
    }

    fn parse_atom(&mut self, tokenizer: &Tokenizer) -> Option<Atom> {
        let token = next_token(&mut self.it);
        let constr = tokenizer.find_token(token.as_str());
        if let Some(constr) = constr {
            return Some(constr(token.as_str()));
        } else {
            return Some(Atom::sym(token));
        }
    }

    fn parse_expr(&mut self, tokenizer: &Tokenizer) -> Atom {
        let mut children: Vec<Atom> = Vec::new();
        while let Some(c) = self.it.peek() {
            match c {
                _ if c.is_whitespace() => { self.it.next(); },
                ')' => {
                    self.it.next();
                    let expr = Atom::expr(children);
                    return expr;
                },
                _ => {
                    children.push(self.parse(tokenizer).expect("Unexpected end of expression member"));
                },
            }
        }
        panic!("Unexpected end of expression");
    }

}

fn next_token(it: &mut Peekable<Chars<'_>>) -> String {
    match it.peek() {
        Some('"') => next_string(it),
        _ => next_word(it),
    }
}

fn next_string(it: &mut Peekable<Chars<'_>>) -> String {
    let mut token = String::new();
    assert_eq!(Some('"'), it.next(), "Double quote expected");
    token.push('"');
    while let Some(&c) = it.peek() {
        if c == '"' {
            token.push('"');
            it.next();
            break;
        }
        let c = if c == '\\' {
            match it.peek() {
                Some(&c) => c,
                None => panic!("Escaping sequence is not finished"),
            }
        } else {
            c
        };
        token.push(c);
        it.next();
    }
    token 
}

fn next_word(it: &mut Peekable<Chars<'_>>) -> String {
    let mut token = String::new();
    while let Some(&c) = it.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        token.push(c);
        it.next();
    }
    token 
}

fn next_var(it: &mut Peekable<Chars<'_>>) -> String {
    let mut token = String::new();
    while let Some(&c) = it.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        if c == '#' {
            panic!("'#' char is reserved for internal usage");
        }
        token.push(c);
        it.next();
    }
    token 
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

        assert_eq!(Some(expr!("ab")), parser.parse(&tokenizer));
        assert_eq!(None, parser.parse(&tokenizer));
    }

    #[test]
    fn test_text_gnd() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.register_token(Regex::new(r"\d+").unwrap(),
            |token| Atom::value(token.parse::<i32>().unwrap()));

        let mut parser = SExprParser::new("(3d 42)");

        assert_eq!(Some(expr!("3d" {42})), parser.parse(&tokenizer));
        assert_eq!(None, parser.parse(&tokenizer));
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
        let mut it = "n)".chars().peekable();

        assert_eq!("n".to_string(), next_token(&mut it));
        assert_eq!(Some(')'), it.next());
    }

    #[test]
    #[should_panic(expected = "Unexpected right bracket")]
    fn test_panic_on_unbalanced_brackets() {
        let mut parser = SExprParser::new("(a))");
        while let Some(_) = parser.parse(&Tokenizer::new()) {}
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
        while let Some(atom) = parser.parse(&tokenizer) {
            result.push(atom);
        }
        result
    }

    #[test]
    #[should_panic(expected = "'#' char is reserved for internal usage")]
    fn test_panic_on_lattice_in_var_name() {
        let mut parser = SExprParser::new("$a#");
        while let Some(_) = parser.parse(&Tokenizer::new()) {}
    }
}
