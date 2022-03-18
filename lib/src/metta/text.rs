use crate::*;
use crate::space::grounding::GroundingSpace;

use std::io::Read;
use std::str::Chars;
use std::iter::Peekable;
use regex::Regex;

pub struct Tokenizer {
    tokens: Vec<TokenDescr>,
}

struct TokenDescr {
    regex: Regex,
    constr: Box<AtomConstr>,
}

type AtomConstr = dyn Fn(&str) -> Atom;

impl Tokenizer {

    pub fn new() -> Self {
        Self{ tokens: Vec::new() }
    }

    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.tokens.push(TokenDescr{ regex, constr: Box::new(constr) });
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
    tokenizer: &'a Tokenizer,
    it: Peekable<Chars<'a>>,
}

impl<'a> SExprParser<'a> {
    pub fn new(tokenizer: &'a Tokenizer, text: &'a str) -> Self {
        Self{ tokenizer: tokenizer, it: text.chars().peekable() }
    }

    fn parse(&mut self) -> Option<Atom> {
        while let Some(c) = self.it.peek() {
            match c {
                _ if c.is_whitespace() => { self.it.next(); },
                '$' => {
                    self.it.next();
                    let token = next_token(&mut self.it);
                    return Some(Atom::Variable(token.into()));
                },
                '(' => {
                    self.it.next();
                    return Some(self.parse_expr());
                },
                _ => {
                    let token = next_token(&mut self.it);
                    let constr = self.tokenizer.find_token(token.as_str());
                    if let Some(constr) = constr {
                        return Some(constr(token.as_str()));
                    } else {
                        return Some(Atom::Symbol(token.into()));
                    }
                },
            }
        }
        None
    }

    fn parse_expr(&mut self) -> Atom {
        let mut children: Vec<Atom> = Vec::new();
        while let Some(c) = self.it.peek() {
            match c {
                _ if c.is_whitespace() => { self.it.next(); },
                ')' => {
                    self.it.next();
                    let expr = Atom::Expression(children.into());
                    return expr;
                },
                _ => {
                    children.push(self.parse().expect("Unexpected end of expression member"));
                },
            }
        }
        panic!("Unexpected end of expression");
    }

}

fn next_token(it: &mut Peekable<Chars<'_>>) -> String {
    let mut token = String::new();
    while let Some(&c) = it.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        // TODO: it would be cool to not push chars one by one into token but
        // create a string from two iterators (begin, end) as in C++. But it looks
        // like Rust standard library doesn't allow it.
        token.push(c);
        it.next();
    }
    token 
}

impl Iterator for SExprParser<'_> {
    type Item = Atom;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

pub struct SExprSpace {
    tokenizer: Tokenizer,
    content: String,
}

impl SExprSpace {
    pub fn new() -> Self {
        Self{ tokenizer: Tokenizer::new(), content: String::new() }
    }

    pub fn add_str(&mut self, text: &str) -> Result<(), String> {
        self.add_reader(&mut text.as_bytes())
    }

    pub fn add_reader(&mut self, reader: &mut dyn Read) -> Result<(), String> {
        reader.read_to_string(&mut self.content)
            .map_err(|e| format!("Could not read text: {}", e))?;
        // TODO: add a check that buffer contains valid Metta code and 
        // return proper Result
        Ok(())
    }


    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.tokenizer.register_token(regex, constr)
    }

    pub fn into_grounding_space(&self, other: &mut GroundingSpace) {
        for atom in SExprParser::new(&self.tokenizer, &self.content) {
            other.add(atom);
        }
    }
}

impl From<&SExprSpace> for GroundingSpace {
    fn from(other: &SExprSpace) -> Self {
        let mut space = GroundingSpace::new();
        other.into_grounding_space(&mut space);
        space
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_var() {
        let mut text = SExprSpace::new();

        text.add_str("$n").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!(n)], *space.borrow_vec());
    }

    #[test]
    fn test_text_sym() {
        let mut text = SExprSpace::new();

        text.add_str("test").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!("test")], *space.borrow_vec());
    }

    #[test]
    fn test_text_recognize_full_token() {
        let mut text = SExprSpace::new();
        text.register_token(Regex::new(r"b").unwrap(),
            |_| Atom::gnd("b"));

        text.add_str("ab").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!("ab")], *space.borrow_vec());
    }

    #[test]
    fn test_text_gnd() {
        let mut text = SExprSpace::new();
        text.register_token(Regex::new(r"\d+").unwrap(),
            |token| Atom::gnd(token.parse::<i32>().unwrap()));

        text.add_str("(3d 42)").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![Atom::expr(&[Atom::sym("3d"), Atom::gnd(42)])],
            *space.borrow_vec());
    }

    #[test]
    fn test_text_expr() {
        let mut text = SExprSpace::new();

        text.add_str("(= (fac $n) (* $n (fac (- $n 1))))").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!("=", ("fac", n), ("*", n, ("fac", ("-", n, "1"))))],
            *space.borrow_vec());
    }

    #[test]
    fn test_text_few_expr() {
        let mut text = SExprSpace::new();

        text.add_str("(a) (b)").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!(("a")), expr!(("b"))], *space.borrow_vec());
    }

    #[test]
    fn test_next_token() {
        let mut it = "n)".chars().peekable();

        assert_eq!("n".to_string(), next_token(&mut it));
        assert_eq!(Some(')'), it.next());
    }
}
