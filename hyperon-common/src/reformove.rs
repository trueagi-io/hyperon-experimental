pub trait RefOrMove<T> {
    fn as_value(self) -> T;
    fn as_ref(&self) -> &T;
}

impl<T> RefOrMove<T> for T {
    fn as_value(self) -> T {
        self
    }

    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: Clone> RefOrMove<T> for &T {
    fn as_value(self) -> T {
        self.clone()
    }

    fn as_ref(&self) -> &T {
        *self
    }
}

