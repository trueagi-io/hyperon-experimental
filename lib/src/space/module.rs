use super::*;

use std::fmt::Debug;

pub struct ModuleSpace {
    main: Box<dyn SpaceMut>,
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
    pub fn new<T: SpaceMut + 'static>(space: T) -> Self {
        Self { main: Box::new(space), deps: Vec::new() }
    }

    pub fn query(&self, query: &Atom) -> BindingsSet {
        log::debug!("ModuleSpace::query: {} {}", self, query);
        let mut results = self.main.query(query);
        for dep in &self.deps {
            if let Some(space) = dep.borrow().as_any() {
                if let Some(space) = space.downcast_ref::<Self>()  {
                    results.extend(space.query_no_deps(query));
                } else {
                    panic!("Only ModuleSpace is expected inside dependencies collection");
                }
            } else {
                panic!("Cannot get space as Any inside ModuleSpace dependencies: {}", dep);
            }
        }
        results
    }

    fn query_no_deps(&self, query: &Atom) -> BindingsSet {
        log::debug!("ModuleSpace::query_no_deps: {} {}", self, query);
        self.main.query(query)
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
        self.main.atom_count()
    }
    fn visit(&self, v: &mut dyn SpaceVisitor) -> Result<(), ()> {
        self.main.visit(v)
    }
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }
    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}

impl SpaceMut for ModuleSpace {
    fn add(&mut self, atom: Atom) {
        self.main.add(atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        self.main.remove(atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        self.main.replace(from, to)
    }
    fn as_space<'a>(&self) -> &(dyn Space + 'a) {
        self
    }
}

