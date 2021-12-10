use crate::*;

fn get_expr<'a>(levels: &Vec<usize>, expr: &'a ExpressionAtom, level: usize) -> &'a ExpressionAtom {
    as_expr(&expr.children()[levels[level] - 1])
}

fn find_next_sibling_expr(levels: &mut Vec<usize>, expr: &ExpressionAtom, level: usize) -> bool {
    let mut idx = levels[level];
    while idx < expr.children().len() {
        let child = &expr.children()[idx];
        if let Atom::Expression(_) = child {
            levels[level] = idx + 1;
            log::trace!("find_next_sibling_expr: return: {}", child);
            return true;
        }
        idx += 1;
    }
    levels.pop();
    return false;
}

fn move_top_down_depth(levels: &mut Vec<usize>, expr: &ExpressionAtom, level: usize) -> bool {
    log::trace!("move_top_down_depth: expr: {}, level: {}, levels.len(): {}, idx: {}", expr, level, levels.len(), levels[level]);
    if level < levels.len() - 1 {
        let found = move_top_down_depth(levels, get_expr(levels, expr, level), level + 1);
        return found || find_next_sibling_expr(levels, expr, level);
    } else {
        let idx = levels[level];
        if idx == 0 {
            return find_next_sibling_expr(levels, expr, level);
        } else {
            levels.push(0);
            let child = as_expr(&expr.children()[idx - 1]);
            let found = move_top_down_depth(levels, child, level + 1);
            return found || find_next_sibling_expr(levels, expr, level);
        }
    }
}

fn move_bottom_up_depth(levels: &mut Vec<usize>, expr: &ExpressionAtom, level: usize) -> bool {
    if level < levels.len() - 1 {
        return move_bottom_up_depth(levels, as_expr(&expr.children()[levels[level] - 1]), level + 1);
    }
    loop {
        let idx = levels[level];
        if idx >= expr.children().len() {
            levels.pop();
            return level > 0;
        }
        let child = &expr.children()[idx];
        levels[level] = idx + 1;
        if let Atom::Expression(ref child_expr) = child {
            levels.push(0);
            return move_bottom_up_depth(levels, child_expr, level + 1);
        }
    }
}

type WalkStrategy = fn(&mut Vec<usize>, &ExpressionAtom, usize) -> bool;

pub static BOTTOM_UP_DEPTH_WALK: WalkStrategy = move_bottom_up_depth;
pub static TOP_DOWN_DEPTH_WALK: WalkStrategy = move_top_down_depth;

#[derive(Clone)]
pub struct SubexprStream {
    expr: Atom,
    levels: Vec<usize>,
    walk: WalkStrategy,
}

impl SubexprStream {
    pub fn from_expr(expr: Atom, walk: WalkStrategy) -> Self {
        Self{
            expr: expr,
            levels: vec![0],
            walk: walk,
        }
    }
    pub fn next(&mut self) -> bool {
        (self.walk)(&mut self.levels, as_expr(&self.expr), 0)
    }

    pub fn has_next(&self) -> bool {
        self.levels.len() > 0
    }

    pub fn as_atom(&self) -> &Atom {
        &self.expr
    }

    pub fn into_atom(self) -> Atom {
        self.expr
    }

    fn get_mut_rec<'a>(levels: &Vec<usize>, atom: &'a mut Atom, level: usize) -> &'a mut Atom {
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

impl Iterator for SubexprStream {
    type Item = Atom;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.next() {
            Some(self.get_mut().clone())
        } else {
            None
        }
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

#[derive(Clone)]
pub struct TopSubexprStream {
    expr: Atom,
    levels: Vec<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bottom_up_depth_walk() {
        let expr = expr!("+", ("*", "3", ("+", "1", n)), ("-", "4", "3"));

        let iter = SubexprStream::from_expr(expr, BOTTOM_UP_DEPTH_WALK);

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("+", "1", n),
        expr!("*", "3", ("+", "1", n)),
        expr!("-", "4", "3"),
        ]);
    }

    #[test]
    fn bottom_up_depth_walk_two_sub_expr() {
        let expr = expr!("*", ("+", "3", "4"), ("-", "5", "2"));

        let iter = SubexprStream::from_expr(expr, BOTTOM_UP_DEPTH_WALK);

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("+", "3", "4"),
        expr!("-", "5", "2"),
        ]);
    }

    #[test]
    fn top_down_depth_walk() {
        let expr = expr!("+", ("*", "3", ("+", "1", n)), ("-", "4", "3"));

        let iter = SubexprStream::from_expr(expr, TOP_DOWN_DEPTH_WALK);

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("*", "3", ("+", "1", n)),
        expr!("+", "1", n),
        expr!("-", "4", "3"),
        ]);
    }

    #[test]
    fn top_down_depth_walk_two_sub_expr() {
        let expr = expr!("*", ("+", "3", "4"), ("-", "5", "2"));

        let iter = SubexprStream::from_expr(expr, TOP_DOWN_DEPTH_WALK);

        assert_eq!(iter.collect::<Vec<_>>(),
        vec![
        expr!("+", "3", "4"),
        expr!("-", "5", "2"),
        ]);
    }
}
