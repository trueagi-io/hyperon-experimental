//! MeTTa parser implementation.

use crate::*;

use std::str::Chars;
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

pub struct SExprParser<'a> {
    it: Peekable<Chars<'a>>,
}

impl<'a> SExprParser<'a> {
    pub fn new(text: &'a str) -> Self {
        Self{ it: text.chars().peekable() }
    }

    pub fn parse(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
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
                    let token = next_var(&mut self.it)?;
                    return Ok(Some(Atom::var(token)));
                },
                '(' => {
                    self.it.next();
                    return self.parse_expr(tokenizer).map(Some);
                },
                ')' => return Err("Unexpected right bracket".to_string()),
                _ => {
                    return Ok(self.parse_atom(tokenizer)?);
                },
            }
        }
        Ok(None)
    }

    fn skip_line(&mut self) -> () {
        while let Some(n) = self.it.peek() {
            match n {
                '\n' => break,
                _ => { self.it.next(); }
            }
        }
    }

    fn parse_atom(&mut self, tokenizer: &Tokenizer) -> Result<Option<Atom>, String> {
        let token = next_token(&mut self.it)?.unwrap();
        let constr = tokenizer.find_token(token.as_str());
        if let Some(constr) = constr {
            return Ok(Some(constr(token.as_str())));
        } else {
            return Ok(Some(Atom::sym(token)));
        }
    }

    fn parse_expr(&mut self, tokenizer: &Tokenizer) -> Result<Atom, String> {
        let mut children: Vec<Atom> = Vec::new();
        while let Some(c) = self.it.peek() {
            match c {
                _ if c.is_whitespace() => { self.it.next(); },
                ')' => {
                    self.it.next();
                    let expr = Atom::expr(children);
                    return Ok(expr);
                },
                _ => {
                    if let Ok(Some(child)) = self.parse(tokenizer) {
                        children.push(child);
                    } else {
                        return Err("Unexpected end of expression member".to_string());
                    }
                },
            }
        }
        Err("Unexpected end of expression".to_string())
    }
    

}

fn next_token(it: &mut Peekable<Chars<'_>>) -> Result<Option<String>, String> {
    match it.peek() {
        Some('"') => next_string(it),
        _ => Ok(Some(next_word(it)?)),
    }
}


fn next_string(it: &mut Peekable<Chars<'_>>) -> Result<Option<String>, String> {
    let mut token = String::new();

    if it.next() != Some('"') {
        return Err("Double quote expected".to_string());
    } else {
        token.push('"');
    }
    while let Some(&c) = it.peek() {
        if c == '"' {
            token.push('"');
            it.next();
            break;
        }
        let c = if c == '\\' {
            match it.peek() {
                Some(&c) => c,
                None => return Err("Escaping sequence is not finished".to_string()), //no idea how to trigger this
            }
        } else {
            c
        };
        token.push(c);
        it.next();
    }
    Ok(Some(token))
}

fn next_word(it: &mut Peekable<Chars<'_>>) -> Result<String, String> {
    let mut token = String::new();
    while let Some(&c) = it.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        token.push(c);
        it.next();
    }
    Ok(token) 
}

fn next_var(it: &mut Peekable<Chars<'_>>) -> Result<String, String> {
    let mut token = String::new();
    while let Some(&c) = it.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        if c == '#' {
            return Err("'#' char is reserved for internal usage".to_string());
        }
        token.push(c);
        it.next();
    }
    Ok(token)
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
        let mut it = "n)".chars().peekable();

        assert_eq!("n".to_string(), next_token(&mut it).unwrap().unwrap());
        assert_eq!(Some(')'), it.next());
    }

    #[test]
    fn test_next_string_errors() {

        let mut token = String::new();
        token.push('a');
        let mut it = token.chars().peekable();
        assert_eq!(Err(String::from("Double quote expected")), next_string(&mut it));


        let mut token = String::new();
        token.push('"');
        token.push('\\');
        let mut it = token.chars().peekable();
        //assert_eq!(Err(String::from("Escaping sequence is not finished")), next_string(&mut it));
        let x = next_string(&mut it);
        println!("val = {:?}", x);
        //assert_eq!(1,2);

    }

    #[test]
    fn test_unbalanced_brackets() {
        let mut parser = SExprParser::new("(a)))");
        let x = parser.parse(&Tokenizer::new());
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
