use super::*;

impl Atom {
    /// Return iterator through all sub-atoms of the [Atom].
    pub fn iter(&self) -> AtomIter {
        AtomIter::new(self)
    }

    /// Return mutable iterator through all sub-atoms of the [Atom].
    pub fn iter_mut(&mut self) -> AtomIterMut {
        AtomIterMut::new(self)
    }
}

/// Iterator of the sub-atoms of the [Atom].
// TODO: Single/Expression enum can be used inside to make code more clear.
pub struct AtomIter<'a> {
    levels: Vec<std::slice::Iter<'a, Atom>>,
    single: Option<&'a Atom>,
}

impl<'a> AtomIter<'a> {

    pub fn new(atom: &'a Atom) -> Self {
        match atom {
            Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) => {
                Self{ levels: vec![], single: Some(atom) }
            },
            Atom::Expression(expr) => {
                Self{ levels: vec![expr.children().iter()], single: None }
            },
        }
    }

    pub fn filter_type<T: TryFrom<&'a Atom>>(self) -> impl Iterator<Item=T> + 'a {
        self.filter_map(|a| <T>::try_from(a).ok())
    }
}

impl<'a> Iterator for AtomIter<'a> {
    type Item = &'a Atom;

    fn next(&mut self) -> Option<&'a Atom> {
        match self.single.take() {
            Some(atom) => Some(atom),
            None => loop {
                match self.levels.last_mut() {
                    None => { return None; },
                    Some(iter) => {
                        match iter.next() {
                            None => { self.levels.pop(); },
                            Some(Atom::Expression(expr)) => {
                                self.levels.push(expr.children().iter());
                            },
                            Some(atom) => { return Some(atom); },
                        }
                    }
                }
            }
        }
    }
}

/// Mutable iterator of the sub-atoms of the [Atom].
pub struct AtomIterMut<'a> {
    levels: Vec<std::slice::IterMut<'a, Atom>>,
    single: Option<&'a mut Atom>,
}

impl<'a> AtomIterMut<'a> {

    pub fn new(atom: &'a mut Atom) -> Self {
        match atom {
            Atom::Symbol(_) | Atom::Variable(_) | Atom::Grounded(_) => {
                Self{ levels: vec![], single: Some(atom) }
            },
            Atom::Expression(expr) => {
                Self{ levels: vec![expr.children_mut().iter_mut()], single: None }
            },
        }
    }

    pub fn filter_type<T: TryFrom<&'a mut Atom>>(self) -> impl Iterator<Item=T> + 'a {
        self.filter_map(|a| <T>::try_from(a).ok())
    }
}

impl<'a> Iterator for AtomIterMut<'a> {
    type Item = &'a mut Atom;

    fn next(&mut self) -> Option<&'a mut Atom> {
        match self.single.take() {
            Some(atom) => Some(atom),
            None => loop {
                match self.levels.last_mut() {
                    None => { return None; },
                    Some(iter) => {
                        match iter.next() {
                            None => { self.levels.pop(); },
                            Some(Atom::Expression(expr)) => {
                                self.levels.push(expr.children_mut().iter_mut());
                            },
                            Some(atom) => { return Some(atom); },
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn atom_iter_collect() {
        assert_eq!(expr!("A").iter().collect::<Vec<&Atom>>(), vec![&expr!("A")]);
        assert_eq!(expr!(a).iter().collect::<Vec<&Atom>>(), vec![&expr!(a)]);
        assert_eq!(expr!({1}).iter().collect::<Vec<&Atom>>(), vec![&expr!({1})]);
    }

    #[test]
    fn expr_iter_collect() {
        assert_eq!(expr!("A" a {1}).iter().collect::<Vec<&Atom>>(),
            vec![&expr!("A"), &expr!(a), &expr!({1})]);
        assert_eq!(expr!("A" (a {1})).iter().collect::<Vec<&Atom>>(),
            vec![&expr!("A"), &expr!(a), &expr!({1})]);
    }

    #[test]
    fn atom_iter_mut_collect() {
        assert_eq!(expr!("A").iter_mut().collect::<Vec<&mut Atom>>(), vec![&mut expr!("A")]);
        assert_eq!(expr!(a).iter_mut().collect::<Vec<&mut Atom>>(), vec![&mut expr!(a)]);
        assert_eq!(expr!({1}).iter_mut().collect::<Vec<&mut Atom>>(), vec![&mut expr!({1})]);
    }

    #[test]
    fn expr_iter_mut_collect() {
        assert_eq!(expr!("A" a {1}).iter_mut().collect::<Vec<&mut Atom>>(),
            vec![&mut expr!("A"), &mut expr!(a), &mut expr!({1})]);
        assert_eq!(expr!("A" (a {1})).iter_mut().collect::<Vec<&mut Atom>>(),
            vec![&mut expr!("A"), &mut expr!(a), &mut expr!({1})]);
    }

    #[test]
    fn symbol_iter_mut() {
        let mut symbol = expr!("A");
        *symbol.iter_mut().next().unwrap() = expr!("B");
        assert_eq!(symbol, expr!("B"));
    }

    #[test]
    fn var_iter_mut() {
        let mut var = expr!(a);
        *var.iter_mut().next().unwrap() = expr!(b);
        assert_eq!(var, expr!(b));
    }

    #[test]
    fn grounded_iter_mut() {
        let mut grounded = expr!({1});
        *grounded.iter_mut().next().unwrap() = expr!({2});
        assert_eq!(grounded, expr!({2}));
    }

    #[test]
    fn expr_iter_mut() {
        let mut expr = expr!(("A" (a {1})));
        let mut iter = expr.iter_mut();
        *iter.next().unwrap() = expr!("B");
        *iter.next().unwrap() = expr!(b);
        *iter.next().unwrap() = expr!({2});
        assert_eq!(expr, expr!(("B" (b {2}))));
    }
}
