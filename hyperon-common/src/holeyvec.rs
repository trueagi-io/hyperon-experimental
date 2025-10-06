#[derive(Clone, Debug, PartialEq, Eq)]
enum Cell<T> {
    Value(T),
    Hole(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HoleyVec<T> {
    first_hole: usize,
    vec: Vec<Cell<T>>,
}

impl<T> HoleyVec<T> {

    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn next_index(&self) -> usize {
        self.first_hole
    }

    #[inline]
    pub fn index_upper_bound(&self) -> usize {
        self.vec.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    #[inline]
    pub fn is_hole(&self, index: usize) -> bool {
        matches!(self.vec[index], Cell::Hole(_))
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.vec.get(index).and_then(|cell| {
            match cell {
                Cell::Value(value) => Some(value),
                Cell::Hole(_) => None,
            }
        })
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        match self.vec.get_unchecked(index) {
            Cell::Value(value) => value,
            Cell::Hole(_) => unreachable!(),
        }
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.vec.get_mut(index).and_then(|cell| {
            match cell {
                Cell::Value(value) => Some(value),
                Cell::Hole(_) => None,
            }
        })
    }

    #[inline]
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        match self.vec.get_unchecked_mut(index) {
            Cell::Value(value) => value,
            Cell::Hole(_) => unreachable!(),
        }
    }

    pub fn push(&mut self, value: T) -> usize {
        if self.first_hole >= self.vec.len() {
            let index = self.vec.len();
            self.vec.push(Cell::Value(value));
            self.first_hole = index + 1;
            index
        } else {
            let index = self.first_hole;
            match self.vec[index] {
                Cell::Hole(next_hole) => {
                    self.first_hole = next_hole;
                    self.vec[index] = Cell::Value(value);
                },
                _ => unreachable!(),
            }
            index
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        let mut value = Cell::Hole(self.first_hole);
        std::mem::swap(&mut self.vec[index], &mut value);
        match value {
            Cell::Value(value) => {
                self.first_hole = index;
                value
            },
            Cell::Hole(_) => {
                panic!("Index doesn't exist");
            }
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<T> {
        fn unwrap<T>(cell: &Cell<T>) -> Option<&T> {
            match cell {
                Cell::Value(value) => Some(value),
                Cell::Hole(_) => None,
            }
        }
        Iter(self.vec.iter().filter_map(unwrap))
    }

    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<T> {
        fn unwrap<T>(cell: &mut Cell<T>) -> Option<&mut T> {
            match cell {
                Cell::Value(value) => Some(value),
                Cell::Hole(_) => None,
            }
        }
        IterMut(self.vec.iter_mut().filter_map(unwrap))
    }
}

impl<T> std::ops::Index<usize> for HoleyVec<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Index doesn't exist")
    }
}

impl<T> std::ops::IndexMut<usize> for HoleyVec<T> {

    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).expect("Index doesn't exist")
    }
}

pub struct Iter<'a, T>(std::iter::FilterMap<std::slice::Iter<'a, Cell<T>>, fn(&Cell<T>) -> Option<&T>>);
pub struct IterMut<'a, T>(std::iter::FilterMap<std::slice::IterMut<'a, Cell<T>>, fn(&mut Cell<T>) -> Option<&mut T>>);

impl<'a, T> std::iter::Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, T> IntoIterator for &'a HoleyVec<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> std::iter::Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, T> IntoIterator for &'a mut HoleyVec<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T> Default for HoleyVec<T> {
    #[inline]
    fn default() -> Self {
        Self{ first_hole: 0, vec: Vec::new() }
    }
}
