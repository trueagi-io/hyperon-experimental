use super::collections::ListMap;

use std::cmp::Ordering;

pub fn vec_eq_no_order<T: PartialEq + std::fmt::Debug>(left: &Vec<T>, right: &Vec<T>) -> Result<(), String> {
    let mut left_count: ListMap<&T, usize> = ListMap::new();
    let mut right_count: ListMap<&T, usize> = ListMap::new();
    for i in left.iter() {
        *left_count.entry(&i).or_insert(0) += 1;
    }
    for i in right.iter() {
        *right_count.entry(&i).or_insert(0) += 1;
    }
    counter_eq_explanation(&left_count, &right_count)
}

fn counter_eq_explanation<T: PartialEq + std::fmt::Debug>(left: &ListMap<&T, usize>, right: &ListMap<&T, usize>) -> Result<(), String> {
    for e in right.iter() {
        if let Some(count) = left.get(e.0) {
            match count.cmp(e.1) {
                Ordering::Less => return Err(format!("Missed result: {:?}", e.0)),
                Ordering::Greater => return Err(format!("Excessive result: {:?}", e.0)),
                Ordering::Equal => {},
            }
        } else {
            return Err(format!("Missed result: {:?}", e.0));
        }
    }
    for e in left.iter() {
        if let Some(count) = right.get(e.0) {
            match e.1.cmp(count) {
                Ordering::Less => return Err(format!("Missed result: {:?}", e.0)),
                Ordering::Greater => return Err(format!("Excessive result: {:?}", e.0)),
                Ordering::Equal => {},
            }
        } else {
            return Err(format!("Excessive result: {:?}", e.0));
        }
    }
    Ok(())
}

#[macro_export]
macro_rules! assert_eq_no_order {
    ($left:expr, $right:expr) => {
        {
            let left = &$left;
            let right = &$right;
            assert!($crate::common::assert::vec_eq_no_order(left, right) == Ok(()),
                "(left == right some order)\n  left: {:?}\n right: {:?}", left, right);
        }
    }
}

pub fn metta_results_eq<T: PartialEq + std::fmt::Debug>(
    left: &Result<Vec<Vec<T>>, String>, right: &Result<Vec<Vec<T>>, String>) -> bool
{
    match (left, right) {
        (Ok(left), Ok(right)) if left.len() == right.len() => {
            for (left, right) in left.iter().zip(right.iter()) {
                if let Err(_) = vec_eq_no_order(left, right) {
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
