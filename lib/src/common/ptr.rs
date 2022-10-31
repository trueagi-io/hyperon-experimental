use std::sync::{Arc, Mutex};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Debug;

pub trait SmartPtr<T> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_>;
}

pub trait SmartPtrMut<T> : SmartPtr<T> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_>;
}

impl<T> SmartPtr<T> for Arc<Mutex<T>> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(self.lock().expect("Mutex is poisoned"))
    }
}

impl<T> SmartPtrMut<T> for Arc<Mutex<T>> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(self.lock().expect("Mutex is poisoned"))
    }
}

impl<T> SmartPtr<T> for Rc<RefCell<T>> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(RefCell::borrow(self))
    }
}

impl<T> SmartPtrMut<T> for Rc<RefCell<T>> {
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

impl<T> SmartPtr<T> for &T {
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

impl<T> SmartPtr<T> for &mut T {
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

impl<T> SmartPtrMut<T> for &mut T {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(RefHolderMutMut(self))
    }
}

pub struct ArcMutex<T>(Arc<Mutex<T>>);

impl<T> ArcMutex<T> {
    pub fn new(value: T) -> Self {
        Self(Arc::new(Mutex::new(value)))
    }

    pub fn as_arc(&self) -> Arc<Mutex<T>> {
        self.0.clone()
    }
}

impl<T> SmartPtr<T> for ArcMutex<T> {
    fn borrow(&self) -> Box<dyn Deref<Target=T> + '_> {
        Box::new(self.0.lock().expect("Mutex is poisoned"))
    }
}

impl<T> SmartPtrMut<T> for ArcMutex<T> {
    fn borrow_mut(&mut self) -> Box<dyn DerefMut<Target=T> + '_> {
        Box::new(self.0.lock().expect("Mutex is poisoned"))
    }
}

impl<T> PartialEq for ArcMutex<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.0) == Arc::as_ptr(&other.0)
    }
}

impl<T> Clone for ArcMutex<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Debug> Debug for ArcMutex<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
