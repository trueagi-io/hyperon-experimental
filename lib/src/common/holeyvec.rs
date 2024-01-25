#[derive(Clone, Debug)]
enum Cell<T> {
    Value(T),
    Hole(usize),
}

#[derive(Clone, Debug)]
pub struct HoleyVec<T> {
    first_hole: usize,
    vec: Vec<Cell<T>>,
}

impl<T> HoleyVec<T> {

    pub fn new() -> Self {
        Self{ first_hole: 0, vec: Vec::new() }
    }

    pub fn next_index(&self) -> usize {
        self.first_hole
    }

    pub fn index_upper_bound(&self) -> usize {
        self.vec.len()
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn is_hole(&self, index: usize) -> bool {
        match &self.vec[index] {
            Cell::Value(_) => false,
            Cell::Hole(_) => true,
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match &self.vec[index] {
            Cell::Value(value) => Some(value),
            Cell::Hole(_) => None,
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match &mut self.vec[index] {
            Cell::Value(value) => Some(value),
            Cell::Hole(_) => None,
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
                _ => panic!("Unexpected state"),
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

    pub fn iter(&self) -> Iter<T> {
        Iter::new(self)
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut::new(self)
    }
}

impl<T> std::ops::Index<usize> for HoleyVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Index doesn't exist")
    }
}

impl<T> std::ops::IndexMut<usize> for HoleyVec<T> {

    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).expect("Index doesn't exist")
    }
}

pub struct Iter<'a, T> {
    delegate: std::slice::Iter<'a, Cell<T>>
}

impl<'a, T> Iter<'a, T> {
    fn new(vec: &'a HoleyVec<T>) -> Self {
        Self{ delegate: vec.vec.iter() }
    }
}

impl<'a, T> std::iter::Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.delegate.next() {
                None => return None,
                Some(Cell::Hole(_)) => continue,
                Some(Cell::Value(value)) => return Some(value),
            }
        }
    }
}

impl<'a, T> IntoIterator for &'a HoleyVec<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct IterMut<'a, T> {
    delegate: std::slice::IterMut<'a, Cell<T>>
}

impl<'a, T> IterMut<'a, T> {
    fn new(vec: &'a mut HoleyVec<T>) -> Self {
        Self{ delegate: vec.vec.iter_mut() }
    }
}

impl<'a, T> std::iter::Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.delegate.next() {
                None => return None,
                Some(Cell::Hole(_)) => continue,
                Some(Cell::Value(value)) => return Some(value),
            }
        }
    }
}

impl<'a, T> IntoIterator for &'a mut HoleyVec<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
