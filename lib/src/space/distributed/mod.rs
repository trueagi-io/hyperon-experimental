// DAS
use std::{fmt::{Debug, Display}, sync::{Arc, Mutex}};

use hyperon_common::FlexRef;
use metta_bus_client::{query_with_das, service_bus::ServiceBus};

use hyperon_atom::{
    matcher::{BindingsSet, MatchResultIter},
    Atom,
    CustomMatch,
    Grounded,
    rust_type_atom,
};

use super::{
    grounding::index::AtomIndex,
    Space,
    SpaceCommon,
    SpaceEvent,
    SpaceMut,
    SpaceVisitor
};

#[derive(Clone)]
pub struct DistributedAtomSpace {
    index: AtomIndex,
    common: SpaceCommon,
    service_bus: Arc<Mutex<ServiceBus>>,
    name: Option<String>,
}

impl DistributedAtomSpace {
    pub fn new(service_bus: Arc<Mutex<ServiceBus>>, name: Option<String>) -> Self {
        Self {
            index: AtomIndex::new(),
            common: SpaceCommon::default(),
            service_bus,
            name,
        }
    }

    pub fn query(&self, query: &Atom) -> BindingsSet {
        query_with_das(self.name.clone(), self.service_bus.clone(), query).unwrap()
    }

    pub fn add(&mut self, atom: Atom) {
        self.index.insert(atom.clone());
        self.common.notify_all_observers(&SpaceEvent::Add(atom));
    }

    pub fn remove(&mut self, atom: &Atom) -> bool {
        let is_removed = self.index.remove(atom);
        if is_removed {
            self.common.notify_all_observers(&SpaceEvent::Remove(atom.clone()));
        }
        is_removed
    }

    pub fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        let is_replaced = self.index.remove(from);
        if is_replaced {
            self.index.insert(to.clone());
            self.common.notify_all_observers(&SpaceEvent::Replace(from.clone(), to));
        }
        is_replaced
    }
}

impl Space for DistributedAtomSpace {
    fn common(&self) -> FlexRef<SpaceCommon> {
        FlexRef::from_simple(&self.common)
    }
    fn query(&self, query: &Atom) -> BindingsSet {
        self.query(query)
    }
    fn atom_count(&self) -> Option<usize> {
        Some(self.index.iter().count())
    }
    fn visit(&self, v: &mut dyn SpaceVisitor) -> Result<(), ()> {
       Ok(self.index.iter().for_each(|atom| v.accept(atom)))
    }
    fn subst(&self, pattern: &Atom, template: &Atom) -> Vec<Atom> {
        self.query(pattern).drain(0..)
            .map(| bindings | hyperon_atom::matcher::apply_bindings_to_atom_move(template.clone(), &bindings))
            .collect()
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl SpaceMut for DistributedAtomSpace {
    fn add(&mut self, atom: Atom) {
        self.add(atom)
    }
    fn remove(&mut self, atom: &Atom) -> bool {
        self.remove(atom)
    }
    fn replace(&mut self, from: &Atom, to: Atom) -> bool {
        self.replace(from, to)
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }
}

impl PartialEq for DistributedAtomSpace {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Debug for DistributedAtomSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "DistributedAtomSpace-{name} ({self:p})"),
            None => write!(f, "DistributedAtomSpace-{self:p}")
        }
    }
}

impl Display for DistributedAtomSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "DistributedAtomSpace-{name}"),
            None => write!(f, "DistributedAtomSpace-{self:p}")
        }
    }
}

impl Grounded for DistributedAtomSpace {
    fn type_(&self) -> Atom {
        rust_type_atom::<DistributedAtomSpace>()
    }

    fn as_match(&self) -> Option<&dyn CustomMatch> {
        Some(self)
    }
}

impl CustomMatch for DistributedAtomSpace {
    fn match_(&self, other: &Atom) -> MatchResultIter {
        Box::new(self.query(other).into_iter())
    }
}
