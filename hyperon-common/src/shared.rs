use std::sync::{Arc, Mutex};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Debug, Display};

pub trait LockBorrow<T: ?Sized> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_>;
}

pub trait LockBorrowMut<T: ?Sized> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_>;
}

impl<T> LockBorrow<T> for Arc<Mutex<T>> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(self.lock().expect("Mutex is poisoned"))
    }
}

impl<T> LockBorrowMut<T> for Arc<Mutex<T>> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(self.lock().expect("Mutex is poisoned"))
    }
}

impl<T> LockBorrow<T> for Rc<RefCell<T>> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(RefCell::borrow(self))
    }
}

impl<T> LockBorrowMut<T> for Rc<RefCell<T>> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(RefCell::borrow_mut(self))
    }
}

struct RefHolder<'a, T: 'a>(&'a &'a T);

impl<'a, T: 'a> Deref for RefHolder<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        *(self.0)
    }
}

impl<T> LockBorrow<T> for &T {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(RefHolder(self))
    }
}

struct RefHolderMut<'a, T: 'a>(&'a &'a mut T);

impl<'a, T: 'a> Deref for RefHolderMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        *(self.0)
    }
}

impl<T> LockBorrow<T> for &mut T {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(RefHolderMut(self))
    }
}

struct RefHolderMutMut<'a, 'b: 'a, T: 'a>(&'a mut &'b mut T);

impl<'a, 'b: 'a, T: 'a> Deref for RefHolderMutMut<'a, 'b, T> {
    type Target = T;

    fn deref(&self) -> &T {
        *(self.0)
    }
}

impl<'a, 'b: 'a, T: 'a> DerefMut for RefHolderMutMut<'a, 'b, T> {
    fn deref_mut(&mut self) -> &mut T {
        *(self.0)
    }
}

impl<T> LockBorrowMut<T> for &mut T {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(RefHolderMutMut(self))
    }
}

pub struct Shared<T: ?Sized>(pub Rc<RefCell<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(RefCell::borrow(&self.0))
    }

    pub fn borrow_mut(&self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(RefCell::borrow_mut(&self.0))
    }

    pub fn clone_inner(&self) -> Self where T: Clone {
        Self::new(RefCell::borrow(&self.0).clone())
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub fn unwrap_or_clone(self) -> T where T: Clone {
        match Rc::try_unwrap(self.0) {
            Err(rc) => RefCell::borrow(&rc).clone(),
            Ok(ref_cell) => ref_cell.into_inner(),
        }
    }
}

impl<T> LockBorrow<T> for Shared<T> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        self.borrow()
    }
}

impl<T> LockBorrowMut<T> for Shared<T> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Shared::borrow_mut(self)
    }
}

impl<T> PartialEq for Shared<T> {
    fn eq(&self, other: &Self) -> bool {
        RefCell::as_ptr(&self.0) == RefCell::as_ptr(&other.0)
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Debug> Debug for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Shared{{ val={:?}, addr={:?} }}", RefCell::borrow(&self.0), RefCell::as_ptr(&self.0))
    }
}

impl<T: Display> Display for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}(addr={:?})", RefCell::borrow(&self.0), RefCell::as_ptr(&self.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_for_shared() {
        let shared = Shared::new("some-string");
        assert_eq!(format!("{:?}", shared), format!("Shared{{ val=\"some-string\", addr={:?} }}", RefCell::as_ptr(&shared.0)));
    }

    #[test]
    fn display_for_shared() {
        let shared = Shared::new("some-string");
        assert_eq!(format!("{}", shared), format!("some-string(addr={:?})", RefCell::as_ptr(&shared.0)));
    }

    #[test]
    fn shared_cloned() {
        let value = 0;
        let shared = Shared::new(value);
        let cloned = shared.clone_inner();

        **cloned.borrow_mut() = 1;

        assert_eq!(**shared.borrow(), 0);
        assert_eq!(**cloned.borrow(), 1);
    }
}
