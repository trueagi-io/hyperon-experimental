#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecOnDemand<T> {
    vec: Option<Box<Vec<T>>>,
}

impl<T> Default for VecOnDemand<T> {
    fn default() -> Self {
        Self{ vec: None }
    }
}

impl<T> VecOnDemand<T> {
    #[inline]
    fn op<'a, R: 'a, S: Fn(&'a Vec<T>) -> R, N: Fn() -> R>(&'a self, on_some: S, on_none: N) -> R {
        match &self.vec {
            Some(vec) => on_some(vec),
            None => on_none(),
        }
    }

    #[inline]
    fn op_mut<'a, R: 'a, S: Fn(&'a mut Vec<T>) -> R, N: Fn() -> R>(&'a mut self, on_some: S, on_none: N) -> R {
        match &mut self.vec {
            Some(vec) => on_some(vec),
            None => on_none(),
        }
    }

    pub fn push(&mut self, item: T) {
        match &mut self.vec {
            Some(vec) => vec.push(item),
            None => {
                let vec = Box::new(vec![item]);
                self.vec = Some(vec);
            },
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        match &mut self.vec {
            Some(vec) => vec.remove(index),
            None => panic!("removal index (is {index}) should be < len (is 0)"),
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.op(|v| v.iter(), || [].iter())
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.op_mut(|v| v.iter_mut(), || [].iter_mut())
    }

    pub fn is_empty(&self) -> bool {
        self.op(Vec::is_empty, || true)
    }

    pub fn len(&self) -> usize {
        self.op(Vec::len, || 0)
    }
}

impl<T> std::ops::Deref for VecOnDemand<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        self.op(Vec::as_slice, || &[])
    }
}

impl<T> std::ops::DerefMut for VecOnDemand<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        self.op_mut(Vec::as_mut_slice, || &mut [])
    }
}

impl<'a, T> IntoIterator for &'a VecOnDemand<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut VecOnDemand<T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

