use crate::*;

use std::io::Read;
use regex::Regex;

pub struct SExprSpace {
    content: Vec<String>,
    tokens: Vec<TokenDescr>,
}

struct TokenDescr {
    regex: Regex,
    constr: Box<AtomConstr>,
}

type AtomConstr = dyn Fn(&str) -> Atom;

impl SExprSpace {

    pub fn new() -> Self {
        Self{ content: Vec::new(), tokens: Vec::new() }
    }

    pub fn add_str(&mut self, text: &str) -> Result<(), String> {
        self.add_reader(&mut text.as_bytes())
    }

    pub fn add_reader(&mut self, reader: &mut dyn Read) -> Result<(), String> {
        let mut buffer = String::new();
        reader.read_to_string(&mut buffer)
            .map_err(|e| format!("Could not read text: {}", e))?;
        self.content.push(buffer);
        // TODO: add a check that buffer contains valid Metta code and 
        // return proper Result
        Ok(())
    }

    pub fn register_token<C: 'static + Fn(&str) -> Atom>(&mut self, regex: Regex, constr: C) {
        self.tokens.push(TokenDescr{ regex, constr: Box::new(constr) });
    }

    pub fn into_grounding_space(&self, other: &mut GroundingSpace) {
        for text in &self.content {
            self.parse(&mut text.chars(), &mut |atom| other.add(atom));
        }
    }

    // TODO: can we replace FnMut by fn() here?
    fn parse(&self, it: &mut dyn Iterator<Item=char>, add: &mut dyn FnMut(Atom)) {
        let mut it = it.peekable();
        'outer: while let Some(c) = it.peek() {
            match c {
                _ if c.is_whitespace() => { it.next(); },
                '$' => {
                    it.next();
                    let token = next_token(&mut it);
                    add(Atom::Variable(token.into()));
                },
                '(' => {
                    let mut children: Vec<Atom> = Vec::new();
                    it.next();
                    self.parse(&mut it, &mut |atom| children.push(atom));
                    // FIXME: add From trait implementations for all atoms
                    let expr = Atom::Expression(children.into());
                    add(expr);
                },
                ')' => {
                    it.next();
                    break 'outer;
                }
                _ => {
                    let token = next_token(&mut it);
                    let constr = self.find_token(token.as_str());
                    if let Some(constr) = constr {
                        add(constr(token.as_str()));
                    } else {
                        add(Atom::Symbol(token.into()))
                    }
                },
            }
        }
    }

    fn find_token(&self, token: &str) -> Option<&AtomConstr> {
        self.tokens.iter().find(|descr| {
            match descr.regex.find_at(token, 0) {
                Some(m) => m.end() == token.len(),
                None => false,
            }
        }).map(|descr| &*(descr.constr))
    }

}


fn next_token<I: Iterator<Item=char>>(it: &mut std::iter::Peekable<I>) -> String {
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_var() {
        let mut text = SExprSpace::new();

        text.add_str("$n").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!(n)], space.atom_iter().cloned().collect::<Vec<_>>());
    }

    #[test]
    fn test_text_sym() {
        let mut text = SExprSpace::new();

        text.add_str("test").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!("test")], space.atom_iter().cloned().collect::<Vec<_>>());
    }

    #[test]
    fn test_text_gnd() {
        let mut text = SExprSpace::new();
        text.register_token(Regex::new(r"\d+").unwrap(),
            |token| Atom::gnd(token.parse::<i32>().unwrap()));

        text.add_str("(3d 42)").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![Atom::expr(&[Atom::sym("3d"), Atom::gnd(42)])], space.atom_iter().cloned().collect::<Vec<_>>());
    }

    #[test]
    fn test_text_expr() {
        let mut text = SExprSpace::new();

        text.add_str("(= (fac $n) (* $n (fac (- $n 1))))").unwrap();
        let space = GroundingSpace::from(&text);

        assert_eq!(vec![expr!("=", ("fac", n), ("*", n, ("fac", ("-", n, "1"))))],
            space.atom_iter().cloned().collect::<Vec<_>>());
    }

    #[test]
    fn test_next_token() {
        let mut it = "n)".chars().peekable();

        assert_eq!("n".to_string(), next_token(&mut it));
        assert_eq!(Some(')'), it.next());
    }
}
