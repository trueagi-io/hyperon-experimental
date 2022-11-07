use std::sync::{Arc, Mutex};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Debug;

pub trait LockBorrow<T> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_>;
}

pub trait LockBorrowMut<T> : LockBorrow<T> {
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
    fn borrow<'a>(&'a self) -> Box<dyn Deref<Target=T> + '_> {
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
    fn borrow<'a>(&'a self) -> Box<dyn Deref<Target=T> + '_> {
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

pub struct Shared<T>(Rc<RefCell<T>>);

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
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Debug> Debug for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} addr {:?}", self.0, Rc::as_ptr(&self.0))
    }
}
