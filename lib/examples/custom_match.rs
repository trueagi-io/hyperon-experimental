use std::fmt::Display;
use hyperon::*;
use hyperon::atom::matcher::*;

#[derive(PartialEq, Clone, Debug)]
struct TestDict(Vec<(Atom, Atom)>);

impl TestDict {
    fn new() -> Self {
        TestDict(Vec::new())
    }
    fn get(&self, key: &Atom) -> Option<&Atom> {
        self.0.iter().filter(|(k, _)| { k == key }).nth(0).map(|(_, v)| { v })
    }
    fn remove(&mut self, key: &Atom) -> Option<Atom> {
        let v = self.get(key).map(Atom::clone);
        self.0 = self.0.drain(..).filter(|(k, _)| { k != key }).collect();
        v
    }
    fn put(&mut self, key: Atom, value: Atom) -> Option<Atom> {
        let v = self.remove(&key);
        self.0.push((key, value));
        v
    }
}

impl Grounded for TestDict {
    fn type_(&self) -> Atom {
        Atom::sym("Dict")
    }
    fn execute(&self, _args: &mut Vec<Atom>) -> Result<Vec<Atom>, ExecError> {
        execute_not_executable(self)
    }
    fn match_(&self, other: &Atom) -> matcher::MatchResultIter {
        if let Some(other) = other.as_gnd::<TestDict>() {
            other.0.iter().map(|(ko, vo)| {
                self.0.iter().map(|(k, v)| {
                    match_atoms(&Atom::expr(vec![k.clone(), v.clone()]), &Atom::expr(vec![ko.clone(), vo.clone()]))
                }).fold(Box::new(std::iter::empty()) as MatchResultIter, |acc, i| {
                    Box::new(acc.chain(i))
                })
            }).fold(Box::new(std::iter::once(Bindings::new())),
            |acc, i| { matcher::match_result_product(acc, i) })
        } else {
            Box::new(std::iter::empty())
        }
    }
}

impl Display for TestDict {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{ ").and_then(|_| self.0.iter().fold(Ok(()),
        |ret, (key, val)| ret.and_then(
            |_| write!(f, "{}: {}, ", key, val))))
            .and_then(|_| write!(f, "}}"))
    }
}

fn main() {
    let mut dict = TestDict::new();
    dict.put(expr!("x"), expr!({2} {5}));
    dict.put(expr!("y"), expr!({5}));
    let dict = expr!({dict}); 

    let mut query = TestDict::new();
    query.put(expr!(b), expr!(y));
    query.put(expr!(a), expr!({2} y));
    let query = expr!({query});

    let result: Vec<Bindings> = match_atoms(&dict, &query).collect();
    assert_eq!(result, vec![bind!{y: expr!({5}), b: expr!("y"), a: expr!("x")}]);
}

