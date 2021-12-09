use crate::*;

#[derive(Clone)]
pub struct SubexpressionStream {
    expr: Atom,
    levels: Vec<usize>,
}

impl SubexpressionStream {
    pub fn as_atom(&self) -> &Atom {
        &self.expr
    }

    pub fn into_atom(self) -> Atom {
        self.expr
    }

    fn next_rec(levels: &mut Vec<usize>, expr: &ExpressionAtom, level: usize) {
        if level < levels.len() - 1 {
            Self::next_rec(levels, as_expr(&expr.children()[levels[level] - 1]), level + 1);
            return;
        }
        loop {
            let idx = levels[level];
            if idx >= expr.children().len() {
                levels.pop();
                return;
            }
            let child = &expr.children()[idx];
            levels[level] = idx + 1;
            if let Atom::Expression(ref child_expr) = child {
                levels.push(0);
                Self::next_rec(levels, child_expr, level + 1);
                return;
            }
        }
    }

    pub fn next(&mut self) {
        if let Atom::Expression(ref expr) = self.expr {
            Self::next_rec(&mut self.levels, expr, 0);
        }
    }

    pub fn has_next(&self) -> bool {
        self.levels.len() > 0
    }

    fn get_mut_rec<'a>(levels: &'a Vec<usize>, atom: &'a mut Atom, level: usize) -> &'a mut Atom {
        if level >= levels.len() {
            atom
        } else {
            let child = &mut (as_expr_mut(atom).children_mut()[levels[level] - 1]);
            Self::get_mut_rec(levels, child, level + 1)
        }
    }

    pub fn get_mut(&mut self) -> &mut Atom {
        Self::get_mut_rec(&self.levels, &mut self.expr, 0)
    }
}

impl Iterator for SubexpressionStream {
    type Item = Atom;
    
    fn next(&mut self) -> Option<Self::Item> {
        if !self.has_next() {
            None
        } else {
            self.next();
            Some(self.get_mut().clone())
        }
    }
}

impl From<ExpressionAtom> for SubexpressionStream {
    fn from(expr: ExpressionAtom) -> Self {
        Self{ expr: Atom::Expression(expr), levels: vec![0] }
    }
}

fn as_expr(atom: &Atom) -> &ExpressionAtom {
    match atom {
        Atom::Expression(ref expr) => expr,
        _ => panic!("Atom::Expression is expected"),
    }
}

fn as_expr_mut(atom: &mut Atom) -> &mut ExpressionAtom {
    match atom {
        Atom::Expression(ref mut expr) => expr,
        _ => panic!("Atom::Expression is expected"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn into_expr_atom(atom: Atom) -> ExpressionAtom {
        match atom {
            Atom::Expression(expr) => expr,
            _ => panic!("Atom::Expression is expected"),
        }
    }

    #[test]
    fn test_subexpression_iterator() {
        let expr = expr!("+", ("*", "3", ("+", "1", n)), ("-", "4", "3"));

        let iter = SubexpressionStream::from(into_expr_atom(expr));

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("+", "1", n),
        expr!("*", "3", ("+", "1", n)),
        expr!("-", "4", "3"),
        expr!("+", ("*", "3", ("+", "1", n)), ("-", "4", "3")),
        ]);
    }

    #[test]
    fn test_subexpression_iterator_two_sub_expr() {
        let expr = expr!("*", ("+", "3", "4"), ("-", "5", "2"));

        let iter = SubexpressionStream::from(into_expr_atom(expr));

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("+", "3", "4"),
        expr!("-", "5", "2"),
        expr!("*", ("+", "3", "4"), ("-", "5", "2")),
        ]);
    }
}
