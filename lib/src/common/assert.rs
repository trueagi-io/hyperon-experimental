use super::collections::ListMap;

pub fn vec_eq_some_order<T: PartialEq + std::fmt::Debug>(left: &Vec<T>, right: &Vec<T>) -> bool {
    let mut amap: ListMap<&T, usize> = ListMap::new();
    let mut bmap: ListMap<&T, usize> = ListMap::new();
    for i in left.iter() {
        *amap.entry(&i).or_insert(0) += 1;
    }
    for i in right.iter() {
        *bmap.entry(&i).or_insert(0) += 1;
    }
    amap == bmap
}

#[macro_export]
macro_rules! assert_eq_no_order {
    ($left:expr, $right:expr) => {
        {
            let left = &$left;
            let right = &$right;
            assert!($crate::common::assert::vec_eq_some_order(left, right),
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
                if !vec_eq_some_order(left, right) {
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
