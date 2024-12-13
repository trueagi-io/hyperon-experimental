use super::collections::{ListMap, Equality, DefaultEquality};

use std::cmp::Ordering;

pub fn vec_eq_no_order<'a, T, A, B>(left: A, right: B) -> Option<String>
where
    T: 'a + PartialEq + std::fmt::Debug,
    A: Iterator<Item=&'a T>,
    B: Iterator<Item=&'a T>,
{
    compare_vec_no_order(left, right, DefaultEquality{}).as_string()
}

pub fn compare_vec_no_order<'a, T, A, B, E>(left: A, right: B, _cmp: E) -> VecDiff<'a, T, E>
where
    A: Iterator<Item=&'a T>,
    B: Iterator<Item=&'a T>,
    E: Equality<&'a T>,
{
    let mut left_count: ListMap<&T, usize, E> = ListMap::new();
    let mut right_count: ListMap<&T, usize, E> = ListMap::new();
    for i in left {
        *left_count.entry(&i).or_insert(0) += 1;
    }
    for i in right {
        *right_count.entry(&i).or_insert(0) += 1;
    }
    VecDiff{ left_count, right_count }
}

pub struct VecDiff<'a, T, E: Equality<&'a T>> {
    left_count: ListMap<&'a T, usize, E>,
    right_count: ListMap<&'a T, usize, E>,
}

trait DiffVisitor<'a, T> {
    fn diff(&mut self, item: &'a T, left: usize, right: usize) -> bool;
}

impl<'a, T: std::fmt::Debug, E: Equality<&'a T>> VecDiff<'a, T, E> {
    pub fn has_diff(&self) -> bool {
        #[derive(Default)]
        struct FindDiff {
            diff: bool,
        }
        impl<T: std::fmt::Debug> DiffVisitor<'_, T> for FindDiff {
            fn diff(&mut self, _item: &T, left: usize, right: usize) -> bool {
                if left == right {
                    false
                } else {
                    self.diff = true;
                    true
                }
            }
        }
        let mut f = FindDiff::default();
        self.visit(&mut f);
        f.diff
    }

    pub fn as_string(&self) -> Option<String> {
        #[derive(Default)]
        struct StringDiff {
            diff: Option<String>,
        }
        impl<'a, T: std::fmt::Debug> DiffVisitor<'a, T> for StringDiff {
            fn diff(&mut self, item: &'a T, left: usize, right: usize) -> bool {
                match left.cmp(&right) {
                    Ordering::Less => {
                        self.diff = Some(format!("Missed result: {:?}", item));
                        true
                    },
                    Ordering::Greater => {
                        self.diff = Some(format!("Excessive result: {:?}", item));
                        true
                    },
                    Ordering::Equal => false,
                }
            }
        }
        let mut d = StringDiff{ diff: None };
        self.visit(&mut d);
        d.diff
    }

    fn visit<'b, V: DiffVisitor<'b, T>>(&'b self, visitor: &mut V) {
        for e in self.right_count.iter() {
            let count = self.left_count.get(e.0).unwrap_or(&0);
            if visitor.diff(e.0, *count, *e.1) { return }
        }
        for e in self.left_count.iter() {
            let count = self.right_count.get(e.0).unwrap_or(&0);
            if visitor.diff(e.0, *e.1, *count) { return }
        }
    }
}

#[macro_export]
macro_rules! assert_eq_no_order {
    ($left:expr, $right:expr) => {
        {
            assert!($crate::common::assert::vec_eq_no_order($left.iter(), $right.iter()) == None,
                "(left == right some order)\n  left: {:?}\n right: {:?}", $left, $right);
        }
    }
}

pub fn metta_results_eq<T: PartialEq + std::fmt::Debug>(
    left: &Result<Vec<Vec<T>>, String>, right: &Result<Vec<Vec<T>>, String>) -> bool
{
    match (left, right) {
        (Ok(left), Ok(right)) if left.len() == right.len() => {
            for (left, right) in left.iter().zip(right.iter()) {
                if vec_eq_no_order(left.iter(), right.iter()).is_some() {
                    return false;
                }
            }
            true
        },
        _ => false,
    }
}

#[macro_export]
macro_rules! assert_eq_metta_results {
    ($left:expr, $right:expr) => {
        {
            let left = &$left;
            let right = &$right;
            assert!($crate::common::assert::metta_results_eq(left, right),
                "(left == right)\n  left: {:?}\n right: {:?}", left, right);
        }
    }
}
