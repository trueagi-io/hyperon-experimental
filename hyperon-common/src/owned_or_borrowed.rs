
/// A simple enum that can wrap an owned type or a reference
pub enum OwnedOrBorrowed<'a, T> {
    Owned(T),
    Borrowed(&'a T)
}

impl<T> From<T> for OwnedOrBorrowed<'_, T> {
    fn from(val: T) -> Self {
        Self::Owned(val)
    }
}

impl<'a, T> From<&'a T> for OwnedOrBorrowed<'a, T> {
    fn from(val: &'a T) -> Self {
        Self::Borrowed(val)
    }
}

impl<T> std::borrow::Borrow<T> for OwnedOrBorrowed<'_, T> {
    fn borrow(&self) -> &T  {
        match self {
            Self::Owned(val) => &val,
            Self::Borrowed(val) => val
        }
    }
}

impl<T> AsRef<T> for OwnedOrBorrowed<'_, T> {
    fn as_ref(&self) -> &T  {
        core::borrow::Borrow::borrow(self)
    }
}

impl<T> OwnedOrBorrowed<'_, T> {
    pub fn into_inner(self) -> T {
        match self {
            Self::Owned(val) => val,
            Self::Borrowed(_) => panic!("Can't take unowned reference")
        }
    }
}
