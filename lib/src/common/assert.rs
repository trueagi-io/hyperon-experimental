use super::collections::{ListMap, Equality, DefaultEquality};

use itertools::Itertools;

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
    let mut diff: ListMap<&T, Count, E> = ListMap::new();
    for i in left {
        diff.entry(&i).or_default().left += 1;
    }
    for i in right {
        diff.entry(&i).or_default().right += 1;
    }
    diff = diff.into_iter().filter(|(_v, c)| c.left != c.right).collect();
    VecDiff{ diff }
}

#[derive(Default)]
struct Count {
    left: usize,
    right: usize,
}

pub struct VecDiff<'a, T, E: Equality<&'a T>> {
    diff: ListMap<&'a T, Count, E>,
}

impl<'a, T: std::fmt::Debug, E: Equality<&'a T>> VecDiff<'a, T, E> {
    pub fn has_diff(&self) -> bool {
        !self.diff.is_empty()
    }

    pub fn as_string(&self) -> Option<String> {
        let mut diff = String::new();
        if self.has_diff() {
            let mut missed = self.diff.iter()
                .filter(|(_v, c)| c.left < c.right)
                .flat_map(|(v, c)| std::iter::repeat_n(v, c.right - c.left))
                .peekable();
            let mut excessive = self.diff.iter()
                .filter(|(_v, c)| c.left > c.right)
                .flat_map(|(v, c)| std::iter::repeat_n(v, c.left - c.right))
                .peekable();
            if missed.peek().is_some() {
                diff.push_str(format!("Missed results: {:?}", missed.format(", ")).as_str());
            }
            if excessive.peek().is_some() {
                if !diff.is_empty() {
                    diff.push_str("\n");
                }
                diff.push_str(format!("Excessive results: {:?}", excessive.format(", ")).as_str());
            }
            Some(diff)
        } else {
            None
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
