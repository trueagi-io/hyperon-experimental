
use std::cell::Ref;

pub enum FlexRef<'a, T> {
    Simple(&'a T),
    RefCell(Ref<'a, T>)
}

impl<'a, T> FlexRef<'a, T> {
    pub fn from_ref_cell(the_ref: Ref<'a, T>) -> Self {
        FlexRef::RefCell(the_ref)
    }
    pub fn from_simple(the_ref: &'a T) -> Self {
        FlexRef::Simple(the_ref)
    }
    pub fn into_simple(self) -> &'a T {
        match self {
            FlexRef::Simple(the_ref) => the_ref,
            FlexRef::RefCell(_) => panic!()
        }
    }
}

impl<'a, T> core::ops::Deref for FlexRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            FlexRef::Simple(the_ref) => the_ref,
            FlexRef::RefCell(the_ref) => &*the_ref
        }
    }
}
