use super::*;

use std::fmt::Debug;

pub struct ModuleSpace {
    main: DynSpace,
    deps: Vec<DynSpace>,
}

impl Display for ModuleSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ModuleSpace({})", &self.main)
    }
}

impl Debug for ModuleSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ModuleSpace({:?})", &self.main)
    }
}

impl ModuleSpace {
    pub fn new(space: DynSpace) -> Self {
        Self { main: space, deps: Vec::new() }
    }

    pub fn query(&self, query: &Atom) -> BindingsSet {
        complex_query(query, |query| self.single_query(query))
    }
 
    fn single_query(&self, query: &Atom) -> BindingsSet {
        log::debug!("ModuleSpace::query: {} {}", self, query);
        let mut results = self.main.borrow().query(query);
        for dep in &self.deps {
            if let Some(space) = dep.borrow().as_any().downcast_ref::<Self>()  {
                results.extend(space.query_no_deps(query));
            } else {
                panic!("Only ModuleSpace is expected inside dependencies collection");
            }
        }
        results
    }

    fn query_no_deps(&self, query: &Atom) -> BindingsSet {
        log::debug!("ModuleSpace::query_no_deps: {} {}", self, query);
        self.main.borrow().query(query)
    }

    pub fn add_dep(&mut self, space: DynSpace) {
        self.deps.push(space)
    }

    pub fn deps(&self) -> &Vec<DynSpace> {
        &self.deps
    }
}

impl Space for ModuleSpace {
    fn common(&self) -> FlexRef<SpaceCommon> {
        self.main.common()
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        ModuleSpace::query(self, query)
    }
    fn atom_count(&self) -> Option<usize> {
        self.main.borrow().atom_count()
    }
    fn visit(&self, v: &mut dyn SpaceVisitor) -> Result<(), ()> {
        self.main.borrow().visit(v)
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl SpaceMut for ModuleSpace {
    fn add(&mut self, atom: Atom) {
        self.main.borrow_mut().add(atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        self.main.borrow_mut().remove(atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        self.main.borrow_mut().replace(from, to)
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod test {
    use hyperon_common::assert_eq_no_order;
    use hyperon_atom::*;
    use crate::space::grounding::*;
    use super::*;

    #[test]
    fn complex_query_two_subspaces() {
        let mut a = GroundingSpace::new();
        a.add(expr!("a" "b"));
        let mut b = GroundingSpace::new();
        b.add(expr!("b" "c"));

        let mut main = ModuleSpace::new(GroundingSpace::new().into());
        main.add_dep(ModuleSpace::new(a.into()).into());
        main.add_dep(ModuleSpace::new(b.into()).into());

        assert_eq_no_order!(main.query(&expr!("," (a "b") ("b" c))), vec![bind!{ a: sym!("a"), c: sym!("c") }]);
        assert_eq_no_order!(main.query(&expr!("," ("a" b) (b "c"))), vec![bind!{ b: sym!("b") }]);
    }
}

